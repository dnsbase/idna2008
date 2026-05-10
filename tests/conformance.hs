-- |
-- Conformance test harness for the @idna2008@ library.  Reads
-- a JSON-encoded list of test vectors from @tests/vectors.json@
-- and runs each through the public parser, comparing the actual
-- result to the expected one.
--
-- Exit code: zero on full pass, non-zero on any failure.
--
-- The same JSON file is intended for reuse by ports of the
-- library to other languages; the schema is documented in
-- @tests/README.md@.
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Test.Tasty as Tasty
import Control.Monad (when)
import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import Data.Char (chr, ord)
import Data.Text (Text)
import Data.Word (Word8)
import System.Exit (exitFailure)
import Test.Tasty.HUnit (assertFailure, testCase)

import Text.IDNA2008
    ( BidiRuleViolation(..)
    , Domain
    , IdnaError(..)
    , IdnaFlags
    , IdnaLoc(..)
    , LabelReason(..)
    , AceReason(..)
    , LabelForm
    , LabelFormSet
    , defaultIdnaFlags
    , domainToAscii
    , domainToUnicode
    , domainToUnicodeLax
    , domainToUnicodeOpt
    , getLabelForms
    , hostnameLabelForms
    , isValidDomainWire
    , labelToAscii
    , labelToUnicode
    , labelToUnicodeLax
    , mkDomain
    , mkDomainShort
    , mkDomainStr
    , mkDomainUtf8
    , parseDomain
    , parseDomainOpts
    , parseDomainShort
    , parseDomainUtf8
    , parseIdnaFlags
    , parseLabelFormSet
    , wireBytes
    )

import qualified Data.ByteString.Short as SBS
import Data.ByteString.Short (ShortByteString)

----------------------------------------------------------------------
-- JSON schema (mirrors tests/README.md)
----------------------------------------------------------------------

data VectorFile = VectorFile
    { vfVersion :: !Int
    , vfTests   :: ![Vector]
    } deriving (Show)

instance FromJSON VectorFile where
    parseJSON = withObject "VectorFile" \o -> VectorFile
        <$> o .:  "version"
        <*> o .:  "tests"

data Vector = Vector
    { vName    :: !Text
    , vInput   :: !Text
    , vClasses :: !(Maybe Text)   -- ^ default: \"hostname\"
    , vFlags   :: !(Maybe Text)   -- ^ default: \"default\"
    , vExpect  :: !Expected
    } deriving (Show)

instance FromJSON Vector where
    parseJSON = withObject "Vector" \o -> Vector
        <$> o .:  "name"
        <*> o .:  "input"
        <*> o .:? "classes"
        <*> o .:? "flags"
        <*> o .:  "expect"

data Expected
    = ExpectOk  !OkBody
    | ExpectErr !ErrBody
    deriving (Show)

instance FromJSON Expected where
    parseJSON = withObject "Expected" \o -> do
        mOk  <- o .:? "ok"
        mErr <- o .:? "err"
        case (mOk, mErr) of
          (Just ok,  Nothing) -> pure (ExpectOk  ok)
          (Nothing,  Just er) -> pure (ExpectErr er)
          (Just _,   Just _ ) -> fail "vector specifies both ok and err"
          (Nothing,  Nothing) -> fail "vector specifies neither ok nor err"

data OkBody = OkBody
    { okWireHex          :: !Text
    , okClasses          :: ![Text]   -- ^ per-label, in label order
    , okDisplayForm      :: !(Maybe Text)     -- ^ 'domainToUnicode' result, if pinned
    , okDisplayFormLax   :: !(Maybe Text)     -- ^ 'domainToUnicodeLax' result, if pinned
    , okDisplayFormAscii :: !(Maybe Text)     -- ^ 'domainToAscii' result, if pinned
    , okDisplayFormOpt   :: !(Maybe ExpectOpt) -- ^ 'domainToUnicodeOpt' run + outcome
    } deriving (Show)

instance FromJSON OkBody where
    parseJSON = withObject "OkBody" \o -> OkBody
        <$> o .:  "wireHex"
        <*> o .:  "classes"
        <*> o .:? "displayForm"
        <*> o .:? "displayFormLax"
        <*> o .:? "displayFormAscii"
        <*> o .:? "displayFormOpt"

-- | Specification for a 'domainToUnicodeOpt' check.  The 'flags'
-- field selects the 'IdnaFlags' bitmask to pass; only 'BIDICHECK'
-- and 'ASCIIFALLBACK' have an effect at this stage
-- (parse-time flags are baked into the 'Domain' already).  Exactly
-- one of 'ok' or 'err' must be set.
data ExpectOpt = ExpectOpt
    { eoFlags :: !Text
    , eoOk    :: !(Maybe Text)
    , eoErr   :: !(Maybe ErrBody)
    } deriving (Show)

instance FromJSON ExpectOpt where
    parseJSON = withObject "ExpectOpt" \o -> ExpectOpt
        <$> o .:  "flags"
        <*> o .:? "ok"
        <*> o .:? "err"

-- | Flat schema for error-shape vectors.  Any field may be
-- omitted; only fields present in the vector are checked.
data ErrBody = ErrBody
    { erKind        :: !Text         -- ^ required: ErrEmptyLabel, ErrCrossLabelBidi, ...
    , erLabelIndex  :: !(Maybe Int)
    , erRule        :: !(Maybe Text) -- ^ BidiRuleViolation tag
    , erReason      :: !(Maybe Text) -- ^ LabelReason or AceReason tag
    , erInnerReason :: !(Maybe Text) -- ^ inner LabelReason inside DecodedInvalid
    , erCodepoint   :: !(Maybe Int)
    , erLength      :: !(Maybe Int)
    } deriving (Show)

instance FromJSON ErrBody where
    parseJSON = withObject "ErrBody" \o -> ErrBody
        <$> o .:  "kind"
        <*> o .:? "labelIndex"
        <*> o .:? "rule"
        <*> o .:? "reason"
        <*> o .:? "innerReason"
        <*> o .:? "codepoint"
        <*> o .:? "length"

----------------------------------------------------------------------
-- Hex helpers (avoid base16-bytestring dep)
----------------------------------------------------------------------

encodeHex :: ByteString -> Text
encodeHex = T.pack . concatMap byteHex . BS.unpack
  where
    byteHex !b = [hex ((b `shiftR` 4) .&. 0xf), hex (b .&. 0xf)]
    hex !n
      | n < 10    = chr (ord '0' + fromIntegral n)
      | otherwise = chr (ord 'a' + fromIntegral n - 10)

decodeHex :: Text -> Either String ByteString
decodeHex t0 = go (T.unpack (T.toLower (T.filter (/= ' ') t0))) []
  where
    go []         acc = Right (BS.pack (reverse acc))
    go [_]        _   = Left "odd-length hex"
    go (a:b:rest) acc = do
        hi <- hexDigit a
        lo <- hexDigit b
        go rest (fromIntegral (hi * 16 + lo) : acc)
    hexDigit c
      | c >= '0' && c <= '9' = Right (ord c - ord '0')
      | c >= 'a' && c <= 'f' = Right (ord c - ord 'a' + 10)
      | otherwise            = Left ("bad hex digit: " ++ [c])

----------------------------------------------------------------------
-- LabelForm -> token-name conversion
----------------------------------------------------------------------

-- | Singleton name for each 'LabelForm'.  Exhaustive over the
-- COMPLETE pragma in 'Text.IDNA2008.Internal.LabelForm'.
-- | Render any tag-style value by the leading constructor name
-- in its 'Show' output.  Works for sum-type constructors with
-- or without payloads because GHC's default 'Show' format
-- always emits the constructor name first, separated from any
-- arguments by a space.
ctorName :: Show a => a -> Text
ctorName = T.pack . takeWhile (/= ' ') . show

formName :: LabelForm -> Text
formName = ctorName

----------------------------------------------------------------------
-- Run a single vector
----------------------------------------------------------------------

runVector :: Vector -> IO ()
runVector v = do
    classes <- resolveClasses (vClasses v)
    flags   <- resolveFlags   (vFlags   v)
    let !result = parseDomainOpts classes flags (vInput v)
    case (vExpect v, result) of
      (ExpectOk eok, Right (dom, info)) -> do
          expectedWire <- case decodeHex (okWireHex eok) of
              Right b  -> pure b
              Left  e  -> assertFailure ("vector wireHex: " ++ e)
                          >> pure BS.empty
          let !actualWire = wireBytes dom
          when (actualWire /= expectedWire) $
              assertFailure $ unlines
                  [ "wire mismatch:"
                  , "  expected: " ++ T.unpack (encodeHex expectedWire)
                  , "  actual:   " ++ T.unpack (encodeHex actualWire)
                  ]
          let !actualClassNames   = map formName (getLabelForms info)
              !expectedClassNames = okClasses eok
          when (actualClassNames /= expectedClassNames) $
              assertFailure $ unlines
                  [ "per-label form mismatch:"
                  , "  expected: " ++ show expectedClassNames
                  , "  actual:   " ++ show actualClassNames
                  ]
          -- Optional rendering-side checks.  Each runs only when the
          -- corresponding field is present in the vector.
          checkDisplayForm    "displayForm"      domainToUnicode    dom
                              (okDisplayForm eok)
          checkDisplayForm    "displayFormLax"   domainToUnicodeLax dom
                              (okDisplayFormLax eok)
          checkDisplayForm    "displayFormAscii" domainToAscii      dom
                              (okDisplayFormAscii eok)
          checkDisplayFormOpt dom (okDisplayFormOpt eok)
      (ExpectOk _,   Left err) ->
          assertFailure ("expected ok, got error: " ++ show err)
      (ExpectErr ee, Left err) ->
          checkErr ee err
      (ExpectErr ee, Right r) ->
          assertFailure
              ("expected error " ++ T.unpack (erKind ee)
               ++ ", got ok: "  ++ show r)

resolveClasses :: Maybe Text -> IO LabelFormSet
resolveClasses Nothing  = pure hostnameLabelForms
resolveClasses (Just t) =
    case parseLabelFormSet hostnameLabelForms (T.encodeUtf8 t) of
      Right c  -> pure c
      Left  e  -> do _ <- assertFailure ("classes: " ++ e); pure mempty

resolveFlags :: Maybe Text -> IO IdnaFlags
resolveFlags Nothing  = pure defaultIdnaFlags
resolveFlags (Just t) =
    case parseIdnaFlags defaultIdnaFlags (T.encodeUtf8 t) of
      Right f  -> pure f
      Left  e  -> do _ <- assertFailure ("flags: " ++ e); pure defaultIdnaFlags

----------------------------------------------------------------------
-- Rendering-side comparisons
----------------------------------------------------------------------

-- | Helper for the two infallible renderers ('domainToUnicode',
-- 'domainToUnicodeLax').  Runs the renderer if the vector pins
-- a value; otherwise no-op.
checkDisplayForm
    :: String                    -- ^ field name, for diagnostics
    -> (Domain -> Text)          -- ^ renderer
    -> Domain                    -- ^ parsed domain
    -> Maybe Text                -- ^ expected, if pinned
    -> IO ()
checkDisplayForm _     _    _   Nothing     = pure ()
checkDisplayForm field run  dom (Just want) =
    let !got = run dom
    in when (got /= want) $
         assertFailure $ unlines
             [ field ++ " mismatch:"
             , "  expected: " ++ show want
             , "  actual:   " ++ show got
             ]

-- | Run 'domainToUnicodeOpt' with the vector-supplied flags and
-- compare the outcome (ok or err) to what the vector pins.  No-op
-- if the field is absent.
checkDisplayFormOpt :: Domain -> Maybe ExpectOpt -> IO ()
checkDisplayFormOpt _   Nothing  = pure ()
checkDisplayFormOpt dom (Just o) = do
    flags <- resolveOptFlags (eoFlags o)
    let !got = domainToUnicodeOpt flags dom
    case (eoOk o, eoErr o, got) of
        (Just want, Nothing, Right actual) ->
            when (actual /= want) $
                assertFailure $ unlines
                    [ "displayFormOpt mismatch:"
                    , "  expected: " ++ show want
                    , "  actual:   " ++ show actual
                    ]
        (Just want, Nothing, Left err) ->
            assertFailure $
                "displayFormOpt: expected ok " ++ show want
                ++ ", got error: " ++ show err
        (Nothing, Just ee, Left err) ->
            checkErr ee err
        (Nothing, Just ee, Right actual) ->
            assertFailure $
                "displayFormOpt: expected error " ++ T.unpack (erKind ee)
                ++ ", got ok: " ++ show actual
        (Nothing, Nothing, _) ->
            assertFailure "displayFormOpt: vector specifies neither ok nor err"
        (Just _,  Just _,  _) ->
            assertFailure "displayFormOpt: vector specifies both ok and err"
  where
    -- displayFormOpt's flags share the same CLI vocabulary as the
    -- top-level 'flags' field, but only BIDICHECK and ASCIIFALLBACK
    -- have an effect at this stage.
    resolveOptFlags :: Text -> IO IdnaFlags
    resolveOptFlags t =
        case parseIdnaFlags defaultIdnaFlags (T.encodeUtf8 t) of
          Right f  -> pure f
          Left  e  -> do _ <- assertFailure ("displayFormOpt.flags: " ++ e)
                         pure defaultIdnaFlags

----------------------------------------------------------------------
-- Error matching: inspect actual error, compare to vector's
-- per-field expectations (each optional)
----------------------------------------------------------------------

checkErr :: ErrBody -> IdnaError -> IO ()
checkErr ee err = do
    let !actualKind = errKind err
    when (erKind ee /= actualKind) $
        mismatch "kind" (erKind ee) actualKind
    checkOpt "labelIndex"  erLabelIndex  (errLabelIndex  err) (T.pack . show)
    checkOpt "rule"        erRule        (errBidiRule    err) id
    checkOpt "reason"      erReason      (errReason      err) id
    checkOpt "innerReason" erInnerReason (errInnerReason err) id
    checkOpt "codepoint"   erCodepoint   (errCodepoint   err) (T.pack . show)
    checkOpt "length"      erLength      (errLength      err) (T.pack . show)
  where
    checkOpt
        :: (Eq a)
        => Text
        -> (ErrBody -> Maybe a)
        -> Maybe a
        -> (a -> Text)
        -> IO ()
    checkOpt field getWant got render =
        case getWant ee of
            Nothing  -> pure ()
            Just w   -> case got of
                Just g | g == w    -> pure ()
                       | otherwise -> mismatch field (render w) (render g)
                Nothing            -> mismatch field (render w) "<absent>"

    mismatch field want got =
        assertFailure $ T.unpack $ T.concat
            [ "error ", field, " mismatch: expected "
            , want, ", got ", got
            , " (full error: ", T.pack (show err), ")"
            ]

errKind :: IdnaError -> Text
errKind = ctorName

errLabelIndex :: IdnaError -> Maybe Int
errLabelIndex e = case e of
    ErrEmptyLabel           loc   -> fromLoc loc
    ErrLabelTooLong         loc _ -> fromLoc loc
    ErrNameTooLong              _ -> Nothing
    ErrBadEscape            loc _ -> fromLoc loc
    ErrInvalidUtf8          loc _ -> fromLoc loc
    ErrCodepointTooLarge    loc _ -> fromLoc loc
    ErrUnpresentableLabel   loc   -> fromLoc loc
    ErrFormNotAllowed       loc _ -> fromLoc loc
    ErrLabelInvalid         loc _ -> fromLoc loc
    ErrAceInvalid           loc _ -> fromLoc loc
    ErrPunycodeOverflow     loc   -> fromLoc loc
    ErrCrossLabelBidi       i   _ -> Just i
  where
    fromLoc loc
        | i < 0     = Nothing
        | otherwise = Just i
      where i = idnaLabelIndex loc

errBidiRule :: IdnaError -> Maybe Text
errBidiRule (ErrCrossLabelBidi _ r)            = Just (bidiRuleName r)
errBidiRule (ErrLabelInvalid _ (LabelBidi r))  = Just (bidiRuleName r)
errBidiRule _                                  = Nothing

errReason :: IdnaError -> Maybe Text
errReason (ErrLabelInvalid _ r) = Just (labelReasonName r)
errReason (ErrAceInvalid   _ r) = Just (aceReasonName   r)
errReason _                     = Nothing

errInnerReason :: IdnaError -> Maybe Text
errInnerReason (ErrAceInvalid _ (DecodedInvalid r)) = Just (labelReasonName r)
errInnerReason _                                    = Nothing

errCodepoint :: IdnaError -> Maybe Int
errCodepoint (ErrLabelInvalid _ r)                          = labelReasonCp r
errCodepoint (ErrAceInvalid   _ (DecodedInvalid r))         = labelReasonCp r
errCodepoint (ErrCodepointTooLarge _ cp)                    = Just cp
errCodepoint _                                              = Nothing

labelReasonCp :: LabelReason -> Maybe Int
labelReasonCp (DisallowedCodepoint   cp) = Just cp
labelReasonCp (ContextRule           cp) = Just cp
labelReasonCp (LeadingCombiningMark  cp) = Just cp
labelReasonCp _                          = Nothing

errLength :: IdnaError -> Maybe Int
errLength (ErrLabelTooLong _ n) = Just n
errLength (ErrNameTooLong    n) = Just n
errLength _                     = Nothing

bidiRuleName :: BidiRuleViolation -> Text
bidiRuleName = ctorName

labelReasonName :: LabelReason -> Text
labelReasonName = ctorName

aceReasonName :: AceReason -> Text
aceReasonName = ctorName

----------------------------------------------------------------------
-- Driver
----------------------------------------------------------------------

main :: IO ()
main = do
    raw <- BS.readFile "tests/vectors.json"
    case A.eitherDecodeStrict raw of
      Left err -> do
          putStrLn ("vectors.json: parse error: " ++ err)
          exitFailure
      Right vf -> do
          when (vfVersion vf /= 1) $ do
              putStrLn ("vectors.json: unsupported version "
                        ++ show (vfVersion vf))
              exitFailure
          Tasty.defaultMain $ Tasty.testGroup "idna2008 tests"
              [ Tasty.testGroup "conformance"
                  [ testCase (T.unpack (vName v)) (runVector v)
                  | v <- vfTests vf
                  ]
              , labelTests
              , domainWireTests
              , parserEntryPointTests
              , invalidUtf8Tests
              , mkDomainFamilyTests
              , tokenVocabularyTests
              ]

----------------------------------------------------------------------
-- Per-label rendering tests.
--
-- Domain-level semantics (parse-then-render of full names) live
-- in the JSON-driven 'conformance' group above.  These hand-built
-- label-level cases exercise 'labelToAscii', 'labelToUnicode',
-- and 'labelToUnicodeLax' in isolation, with the wire-form
-- 'ShortByteString' constructed directly (no parser in the
-- loop).
----------------------------------------------------------------------

labelTests :: Tasty.TestTree
labelTests = Tasty.testGroup "labels"
    [ Tasty.testGroup "labelToAscii"
        [ testCase "LDH passthrough" $
            assertLabel labelToAscii (ascii "example") "example"
        , testCase "ATTRLEAF passthrough" $
            assertLabel labelToAscii (ascii "_dmarc") "_dmarc"
        , testCase "WILDLABEL passthrough" $
            assertLabel labelToAscii (ascii "*") "*"
        , testCase "ACE label stays literal" $
            assertLabel labelToAscii
                (ascii "xn--mnchen-3ya") "xn--mnchen-3ya"
        , testCase "every special escaped" $
            assertLabel labelToAscii
                (ascii "\"$().;@\\")
                (T.pack "\\\"\\$\\(\\)\\.\\;\\@\\\\")
        , testCase "control bytes -> DDD" $
            assertLabel labelToAscii
                (SBS.pack [0x00, 0x09, 0x20, 0x7F])
                (T.pack "\\000\\009\\032\\127")
        , testCase "high bytes -> DDD" $
            assertLabel labelToAscii
                (SBS.pack [0x80, 0xC2, 0xFF])
                (T.pack "\\128\\194\\255")
        ]
    , Tasty.testGroup "labelToUnicode"
        [ testCase "LDH passthrough" $
            assertLabel labelToUnicode (ascii "example") "example"
        , testCase "real A-label decodes" $
            assertLabel labelToUnicode
                (ascii "xn--mnchen-3ya") (T.pack "m\252nchen")
        , testCase "FAKEA emoji stays in ACE" $
            assertLabel labelToUnicode
                (ascii "xn--ls8h") "xn--ls8h"
        , testCase "FAKEA control stays in ACE" $
            assertLabel labelToUnicode
                (ascii "xn--a") "xn--a"
        , testCase "specials escaped same as ASCII" $
            assertLabel labelToUnicode
                (ascii "\"$().;@\\")
                (T.pack "\\\"\\$\\(\\)\\.\\;\\@\\\\")
        ]
    , Tasty.testGroup "labelToUnicodeLax"
        [ testCase "LDH passthrough" $
            assertLabel labelToUnicodeLax (ascii "example") "example"
        , testCase "real A-label decodes" $
            assertLabel labelToUnicodeLax
                (ascii "xn--mnchen-3ya") (T.pack "m\252nchen")
        , testCase "FAKEA emoji decoded" $
            assertLabel labelToUnicodeLax
                (ascii "xn--ls8h") (T.singleton '\x1F4A9')
        , testCase "FAKEA control decoded" $
            assertLabel labelToUnicodeLax
                (ascii "xn--a") (T.singleton '\x80')
          -- OCTET-with-xn--prefix-and-basic-prefix-special.
          -- Punycode decode of "$-a" yields [U+0080, U+0024]:
          -- the extended insertion lands at position 0, shifting
          -- the basic-prefix '$' to position 1.  '$' is then
          -- zone-file-escaped on output.
        , testCase "OCTET xn-- with special in basic prefix" $
            assertLabel labelToUnicodeLax
                (ascii "xn--$-a") (T.pack "\x80\\$")
        , testCase "undecodable body falls back" $
            assertLabel labelToUnicodeLax
                (ascii "xn---hgi") "xn---hgi"
        ]
    ]

-- | Wire-form 'ShortByteString' from a plain ASCII 'String'.
ascii :: String -> ShortByteString
ascii = SBS.pack . map (toEnum . fromEnum)

-- | Assert that a per-label renderer produces the expected 'Text'.
assertLabel
    :: (ShortByteString -> Text)
    -> ShortByteString
    -> Text
    -> IO ()
assertLabel f input expected =
    let got = f input
    in if got == expected
         then pure ()
         else assertFailure $
             "expected " ++ show expected ++ "\n     got " ++ show got

----------------------------------------------------------------------
-- Smart-constructor / wire-form validation tests.
--
-- 'isValidDomainWire' is the predicate the 'Domain' bidirectional
-- pattern synonym uses to gate its writer side.  These cases
-- cover the canonical accept/reject boundary: minimum / maximum
-- lengths, label-length-byte limits, compression-pointer bytes,
-- truncated input, and stray bytes past the root terminator.
----------------------------------------------------------------------

domainWireTests :: Tasty.TestTree
domainWireTests = Tasty.testGroup "isValidDomainWire"
    [ testCase "bare root" $
        assertValid True (SBS.pack [0x00])
    , testCase "single label" $
        assertValid True (lp "abc" <> term)
    , testCase "two labels" $
        assertValid True (lp "abc" <> lp "com" <> term)
    , testCase "label of length 63" $
        assertValid True (lp (replicate 63 'x') <> term)
    , testCase "wire length exactly 255" $
        -- 127 one-char labels (2 bytes each) + 1 terminator = 255
        assertValid True (mconcat (replicate 127 (lp "a")) <> term)

    , testCase "empty input" $
        assertValid False SBS.empty
    , testCase "missing terminator" $
        assertValid False (lp "abc")
    , testCase "truncated content" $
        assertValid False (SBS.pack [0x03, 0x61, 0x62])
    , testCase "premature root with trailing junk" $
        assertValid False (SBS.pack [0x00, 0x00])
    , testCase "stray byte past the root" $
        assertValid False (lp "abc" <> term <> SBS.pack [0x78])
    , testCase "length byte 64 (reserved range)" $
        assertValid False (SBS.pack [0x40, 0x66, 0x6F, 0x6F, 0x00])
    , testCase "compression pointer byte 0xC0" $
        assertValid False (SBS.pack [0xC0, 0x00])
    , testCase "length byte 0xFF" $
        assertValid False (SBS.pack [0xFF, 0x00])
    , testCase "wire length exceeds 255" $
        -- 128 one-char labels + terminator = 257
        assertValid False (mconcat (replicate 128 (lp "a")) <> term)
    ]
  where
    -- Length-prefixed label from an ASCII 'String'.
    lp :: String -> ShortByteString
    lp s = SBS.pack (fromIntegral (length s) : map (toEnum . fromEnum) s)

    -- Root terminator.
    term :: ShortByteString
    term = SBS.pack [0x00]

    assertValid :: Bool -> ShortByteString -> IO ()
    assertValid expected input =
        let got = isValidDomainWire input
        in if got == expected
             then pure ()
             else assertFailure $
                 "expected " ++ show expected
                 ++ " got " ++ show got
                 ++ " for input " ++ show input

----------------------------------------------------------------------
-- Parser entry-point parity.
--
-- All four parser entry points ('parseDomain', 'parseDomainOpts',
-- 'parseDomainUtf8', 'parseDomainShort') call through to the same
-- 'parseDomainView' core, so given the same input they should
-- produce identical results.  The JSON-driven 'conformance' group
-- only exercises 'parseDomainOpts'; these tests confirm the three
-- wrappers behave correctly too.
----------------------------------------------------------------------

parserEntryPointTests :: Tasty.TestTree
parserEntryPointTests = Tasty.testGroup "parser entry-point parity"
    [ testCase "valid name agrees across entry points" $
        parserParity "www.example.com"
    , testCase "non-ASCII name agrees across entry points" $
        parserParity "münchen.example"
    , testCase "invalid name agrees across entry points" $
        parserParity "foo..bar"
    , testCase "too-long label agrees across entry points" $
        parserParity (T.replicate 64 "a" <> ".example")
    ]
  where
    parserParity :: T.Text -> IO ()
    parserParity input = do
        let inputBs  = T.encodeUtf8 input
            inputSbs = SBS.toShort inputBs
            r1 = fmap (wireBytes . fst) (parseDomain hostnameLabelForms input)
            r2 = fmap (wireBytes . fst) (parseDomainOpts hostnameLabelForms defaultIdnaFlags input)
            r3 = fmap (wireBytes . fst) (parseDomainUtf8 hostnameLabelForms inputBs)
            r4 = fmap (wireBytes . fst) (parseDomainShort hostnameLabelForms inputSbs)
        when (not (r1 == r2 && r1 == r3 && r1 == r4)) $
            assertFailure $ unlines
                [ "parser entry points disagreed for " ++ show input
                , "  parseDomain:      " ++ show r1
                , "  parseDomainOpts:  " ++ show r2
                , "  parseDomainUtf8:  " ++ show r3
                , "  parseDomainShort: " ++ show r4
                ]

----------------------------------------------------------------------
-- Invalid UTF-8 byte sequences.
--
-- These inputs carry bare bytes that no JSON string can express
-- (JSON encodes everything as UTF-8 text), so they're driven from
-- Haskell into 'parseDomainUtf8'.  Each input targets one branch of
-- the internal UTF-8 decoder ('decodeUtf8At' in
-- 'Text.IDNA2008.Internal.Parse'); all observable outcomes are
-- 'ErrInvalidUtf8'.
----------------------------------------------------------------------

invalidUtf8Tests :: Tasty.TestTree
invalidUtf8Tests = Tasty.testGroup "invalid UTF-8 rejected"
    [ rejectUtf8 "bare continuation byte"        [0x80]
    , rejectUtf8 "invalid start byte 0xC0"       [0xC0, 0x80]
    , rejectUtf8 "invalid start byte 0xC1"       [0xC1, 0x80]
    , rejectUtf8 "invalid start byte 0xF5"       [0xF5, 0x80, 0x80, 0x80]
    , rejectUtf8 "invalid start byte 0xFF"       [0xFF]
    , rejectUtf8 "two-byte truncated"            [0xC2]
    , rejectUtf8 "two-byte bad continuation"     [0xC2, 0x00]
    , rejectUtf8 "three-byte truncated"          [0xE0, 0xA0]
    , rejectUtf8 "three-byte bad continuation"   [0xE0, 0x80, 0x00]
    , rejectUtf8 "three-byte overlong"           [0xE0, 0x80, 0x80]
    , rejectUtf8 "three-byte surrogate"          [0xED, 0xA0, 0x80]
    , rejectUtf8 "four-byte truncated"           [0xF0, 0x90, 0x80]
    , rejectUtf8 "four-byte bad continuation"    [0xF0, 0x90, 0x80, 0x00]
    , rejectUtf8 "four-byte overlong"            [0xF0, 0x80, 0x80, 0x80]
    , rejectUtf8 "four-byte beyond U+10FFFF"     [0xF4, 0x90, 0x80, 0x80]
      -- After a backslash, a non-numeric non-ASCII byte is fed to
      -- 'decodeUtf8At'.  Invalid bytes there propagate as
      -- ErrInvalidUtf8 -- distinct internal path from the
      -- main-loop UTF-8 decode but the same observable outcome.
    , rejectUtf8 "backslash + bare continuation" [0x66, 0x6F, 0x6F, 0x5C, 0x80]
    , rejectUtf8 "backslash + truncated 2-byte"  [0x66, 0x6F, 0x6F, 0x5C, 0xC2]
    ]
  where
    rejectUtf8 :: String -> [Word8] -> Tasty.TestTree
    rejectUtf8 name bytes = testCase name do
        case parseDomainUtf8 hostnameLabelForms (BS.pack bytes) of
          Left (ErrInvalidUtf8 {}) -> pure ()
          other -> assertFailure $
              "expected ErrInvalidUtf8 for " ++ show bytes
              ++ ", got: " ++ show other

----------------------------------------------------------------------
-- 'Maybe'-returning convenience wrappers.
--
-- The 'mkDomain' family is a thin shim around the @parseDomain*@
-- entry points that discards 'LabelInfo' and collapses any parse
-- error to 'Nothing'.  Spot-check that each shape works for one
-- valid and one invalid input.
----------------------------------------------------------------------

mkDomainFamilyTests :: Tasty.TestTree
mkDomainFamilyTests = Tasty.testGroup "mkDomain helpers"
    [ testCase "mkDomain accepts valid name" $
        assertJust "mkDomain"      (mkDomain     "www.example.com")
    , testCase "mkDomain rejects invalid name" $
        assertNothing "mkDomain"   (mkDomain     "..bad..")
    , testCase "mkDomainStr accepts valid name" $
        assertJust "mkDomainStr"   (mkDomainStr  "www.example.com")
    , testCase "mkDomainStr rejects invalid name" $
        assertNothing "mkDomainStr"(mkDomainStr  "..bad..")
    , testCase "mkDomainUtf8 accepts valid name" $
        assertJust "mkDomainUtf8"
            (mkDomainUtf8 (T.encodeUtf8 ("www.example.com" :: T.Text)))
    , testCase "mkDomainUtf8 rejects invalid name" $
        assertNothing "mkDomainUtf8"
            (mkDomainUtf8 (T.encodeUtf8 ("..bad.." :: T.Text)))
    , testCase "mkDomainUtf8 rejects bad UTF-8" $
        assertNothing "mkDomainUtf8"
            (mkDomainUtf8 (BS.pack [0xC0, 0x80]))
    , testCase "mkDomainShort accepts valid name" $
        assertJust "mkDomainShort"
            (mkDomainShort (SBS.toShort (T.encodeUtf8 ("www.example.com" :: T.Text))))
    , testCase "mkDomainShort rejects invalid name" $
        assertNothing "mkDomainShort"
            (mkDomainShort (SBS.toShort (T.encodeUtf8 ("..bad.." :: T.Text))))
    ]
  where
    assertJust :: String -> Maybe a -> IO ()
    assertJust _    (Just _)  = pure ()
    assertJust name Nothing   =
        assertFailure (name ++ ": expected Just, got Nothing")

    assertNothing :: Show a => String -> Maybe a -> IO ()
    assertNothing _    Nothing  = pure ()
    assertNothing name (Just _) =
        assertFailure (name ++ ": expected Nothing, got Just _")

----------------------------------------------------------------------
-- CLI vocabulary parsers.
--
-- 'parseLabelFormSet' and 'parseIdnaFlags' translate
-- comma-separated token strings into 'LabelFormSet' / 'IdnaFlags'
-- values.  The conformance suite passes happy-path strings via the
-- @classes@ and @flags@ vector fields; these tests cover the
-- negative paths (unknown tokens, ambiguous prefixes) and a few
-- alias-resolution and arithmetic combinations that the JSON
-- vectors don't otherwise exercise.
----------------------------------------------------------------------

tokenVocabularyTests :: Tasty.TestTree
tokenVocabularyTests = Tasty.testGroup "CLI vocabulary parsers"
    [ Tasty.testGroup "parseLabelFormSet"
        [ testCase "preset 'host' parses" $
            assertOkLfs "host"
        , testCase "preset 'idn' parses" $
            assertOkLfs "idn"
        , testCase "preset 'all' parses" $
            assertOkLfs "all"
        , testCase "preset 'strict' parses" $
            assertOkLfs "strict"
        , testCase "'+attrleaf' extends default" $
            assertOkLfs "+attrleaf"
        , testCase "'-fakea' shrinks default" $
            assertOkLfs "-fakea"
        , testCase "unknown token rejected" $
            assertFailLfs "bogus"
        ]
    , Tasty.testGroup "parseIdnaFlags"
        [ testCase "preset 'default' parses" $
            assertOkFlags "default"
        , testCase "preset 'map' parses" $
            assertOkFlags "map"
        , testCase "'+emoji-ok' extends default" $
            assertOkFlags "+emoji-ok"
        , testCase "'-bidi-check' shrinks default" $
            assertOkFlags "-bidi-check"
        , testCase "alias 'xncheck' resolves" $
            assertOkFlags "xncheck"
        , testCase "alias 'cmap' resolves" $
            assertOkFlags "cmap"
        , testCase "unknown token rejected" $
            assertFailFlags "bogus-flag"
        , testCase "ascii-fallback alone parses" $
            assertOkFlags "ascii-fallback"
        , testCase "lax-decode alone parses" $
            assertOkFlags "lax-decode"
        ]
    ]
  where
    assertOkLfs :: BS.ByteString -> IO ()
    assertOkLfs s = case parseLabelFormSet hostnameLabelForms s of
        Right _  -> pure ()
        Left  e  -> assertFailure ("expected Right for " ++ show s
                                    ++ ", got Left: " ++ e)

    assertFailLfs :: BS.ByteString -> IO ()
    assertFailLfs s = case parseLabelFormSet hostnameLabelForms s of
        Left  _  -> pure ()
        Right _  -> assertFailure ("expected Left for " ++ show s)

    assertOkFlags :: BS.ByteString -> IO ()
    assertOkFlags s = case parseIdnaFlags defaultIdnaFlags s of
        Right _  -> pure ()
        Left  e  -> assertFailure ("expected Right for " ++ show s
                                    ++ ", got Left: " ++ e)

    assertFailFlags :: BS.ByteString -> IO ()
    assertFailFlags s = case parseIdnaFlags defaultIdnaFlags s of
        Left  _  -> pure ()
        Right _  -> assertFailure ("expected Left for " ++ show s)
