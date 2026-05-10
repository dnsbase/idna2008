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
import Data.Foldable (foldMap')
import Data.Text (Text)
import Data.Word (Word8)
import System.Exit (exitFailure)
import Test.Tasty.HUnit (assertFailure, testCase)

import Text.IDNA2008
    ( BidiRuleViolation(..)
    , Domain
    , IdnaError(..)
    , IdnaFlags(..)
    , LabelReason(..)
    , AceReason(..)
    , LabelForm(..)
    , LabelFormSet
    , defaultIdnaFlags
    , domainToAscii
    , domainToUnicode
    , getLabelForms
    , hostnameLabelForms
    , isValidWireForm
    , labelFormToSet
    , labelToAscii
    , labelToUnicode
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
    , unparseDomainOpts
    , unparseLabelOpts
    , wireBytes
    )

import qualified Data.ByteString.Short as SBS
import Data.ByteString.Short (ShortByteString)

----------------------------------------------------------------------
-- Test-local equivalents of the (now-retired) lax renderers.
--
-- These exist purely for the conformance test framework, which
-- pins per-vector @displayFormLax@ expectations against a
-- permissive renderer that decodes @\"xn--\"@ labels regardless
-- of whether they pass strict IDN validation.  The library API
-- expresses the same behaviour by including 'LAXULABEL' (and
-- 'FAKEA' as the Punycode-failure fallback) in the
-- 'LabelFormSet'.
----------------------------------------------------------------------

domainToUnicodeLax :: Domain -> Text
domainToUnicodeLax dom = case unparseDomainOpts laxForms defaultIdnaFlags dom of
    Right (t, _) -> t
    Left  _      -> error "domainToUnicodeLax: unreachable"

labelToUnicodeLax :: ShortByteString -> Text
labelToUnicodeLax sbs = case unparseLabelOpts laxForms defaultIdnaFlags sbs of
    Right (t, _) -> t
    Left  _      -> error "labelToUnicodeLax: unreachable"

laxForms :: LabelFormSet
laxForms = foldMap' labelFormToSet
    [LDH, RLDH, ULABEL, LAXULABEL, FAKEA, ATTRLEAF, WILDLABEL, OCTET]

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
    , okDisplayFormOpt   :: !(Maybe ExpectOpt) -- ^ 'unparseDomainOpts' run + outcome
    } deriving (Show)

instance FromJSON OkBody where
    parseJSON = withObject "OkBody" \o -> OkBody
        <$> o .:  "wireHex"
        <*> o .:  "classes"
        <*> o .:? "displayForm"
        <*> o .:? "displayFormLax"
        <*> o .:? "displayFormAscii"
        <*> o .:? "displayFormOpts"

-- | Specification for an 'unparseDomainOpts' check.  The 'flags'
-- field selects the 'IdnaFlags' bitmask to pass; 'BIDICHECK' and
-- 'ASCIIFALLBACK' have an effect at this stage (parse-time flags
-- are baked into the 'Domain' already).  The optional 'classes'
-- field overrides the 'LabelFormSet': if absent, the parser-side
-- 'classes' is reused as-is; if present with a leading @\'+\'@ or
-- @\'-\'@ token, it adjusts the parser-side set; otherwise it
-- replaces it.  Exactly one of 'ok' or 'err' must be set.
data ExpectOpt = ExpectOpt
    { eoFlags   :: !Text
    , eoClasses :: !(Maybe Text)
    , eoOk      :: !(Maybe Text)
    , eoErr     :: !(Maybe ErrBody)
    } deriving (Show)

instance FromJSON ExpectOpt where
    parseJSON = withObject "ExpectOpt" \o -> ExpectOpt
        <$> o .:  "flags"
        <*> o .:? "classes"
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
          checkDisplayFormOpt classes dom (okDisplayFormOpt eok)
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
      Left  e  -> mempty <$ assertFailure ("classes: " ++ e)

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

-- | Run 'unparseDomainOpts' with the vector-supplied flags and
-- 'LabelFormSet' and compare the outcome (ok or err) to what the
-- vector pins.  No-op if the field is absent.  The parser-side
-- 'LabelFormSet' is threaded in as the default for an optional
-- @classes@ override in the @displayFormOpts@ block: a leading
-- @\'+\'@ or @\'-\'@ in that override adjusts the inherited set,
-- otherwise the override replaces it cleanly.
checkDisplayFormOpt :: LabelFormSet -> Domain -> Maybe ExpectOpt -> IO ()
checkDisplayFormOpt _       _   Nothing  = pure ()
checkDisplayFormOpt parserForms dom (Just o) = do
    flags <- resolveOptFlags (eoFlags o)
    forms <- resolveOptClasses parserForms (eoClasses o)
    let !got = fmap fst (unparseDomainOpts forms flags dom)
    case (eoOk o, eoErr o, got) of
        (Just want, Nothing, Right actual) ->
            when (actual /= want) $
                assertFailure $ unlines
                    [ "displayFormOpts mismatch:"
                    , "  expected: " ++ show want
                    , "  actual:   " ++ show actual
                    ]
        (Just want, Nothing, Left err) ->
            assertFailure $
                "displayFormOpts: expected ok " ++ show want
                ++ ", got error: " ++ show err
        (Nothing, Just ee, Left err) ->
            checkErr ee err
        (Nothing, Just ee, Right actual) ->
            assertFailure $
                "displayFormOpts: expected error " ++ T.unpack (erKind ee)
                ++ ", got ok: " ++ show actual
        (Nothing, Nothing, _) ->
            assertFailure "displayFormOpts: vector specifies neither ok nor err"
        (Just _,  Just _,  _) ->
            assertFailure "displayFormOpts: vector specifies both ok and err"
  where
    -- displayFormOpts flags share the same command-line option
    -- syntax as the top-level 'flags' field; 'BIDICHECK' and
    -- 'ASCIIFALLBACK' control cross-label Bidi behaviour at this stage.
    resolveOptFlags :: Text -> IO IdnaFlags
    resolveOptFlags t =
        case parseIdnaFlags defaultIdnaFlags (T.encodeUtf8 t) of
          Right f  -> pure f
          Left  e  -> do _ <- assertFailure ("displayFormOpts.flags: " ++ e)
                         pure defaultIdnaFlags

    -- Optional classes override: absent → use the parser-side
    -- 'LabelFormSet' unchanged; present → parse with the
    -- parser-side set as the default, so a leading @\'+\'@ or
    -- @\'-\'@ token adjusts the inherited set and a bare token
    -- list replaces it.
    resolveOptClasses :: LabelFormSet -> Maybe Text -> IO LabelFormSet
    resolveOptClasses base Nothing  = pure base
    resolveOptClasses base (Just t) =
        case parseLabelFormSet base (T.encodeUtf8 t) of
          Right f  -> pure f
          Left  e  -> do _ <- assertFailure ("displayFormOpts.classes: " ++ e)
                         pure base

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
        | loc < 0   = Nothing
        | otherwise = Just loc

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
          -- The body @\"foo$bar--e6a\"@ is structurally a valid
          -- Punycode encoding -- it would Punycode-decode to
          -- @\"foo$bar-\252\"@ (the basic prefix @\"foo$bar-\"@
          -- plus a U+00FC insertion).  But Punycode only emits
          -- LDH bytes; this body contains @\'$\'@, so the bytes
          -- on the wire could not have come from any IDN
          -- encoder.  The classifier therefore routes to 'OCTET'
          -- on byte shape alone, and the renderer keeps the
          -- literal form with the @\'$\'@ zone-file-escaped --
          -- regardless of any 'LAXULABEL' admission.
        , testCase "OCTET xn-- with non-LDH ASCII inside Punycode body" $
            assertLabel labelToUnicodeLax
                (ascii "xn--foo$bar--e6a")
                (T.pack "xn--foo\\$bar--e6a")
        , testCase "undecodable body falls back" $
            assertLabel labelToUnicodeLax
                (ascii "xn---hgi") "xn---hgi"
        ]
    , Tasty.testGroup "unparseLabelOpts"
        -- The new fallible per-label primitive.  Lets the caller
        -- choose between Unicode-rendering ('ULABEL' / 'LAXULABEL')
        -- and literal @\"xn--\"@ ('ALABEL' / 'FAKEA') via the
        -- 'LabelFormSet'.  These tests cover the strict / permissive
        -- distinction at the label level.
        [ testCase "LDH passthrough" $
            assertUnparseLabel laxForms defaultIdnaFlags
                (ascii "example")
                (Right ("example", LDH))
        , testCase "clean A-label decodes to ULABEL" $
            assertUnparseLabel laxForms defaultIdnaFlags
                (ascii "xn--mnchen-3ya")
                (Right (T.pack "m\252nchen", ULABEL))
        , testCase "FAKEA emoji literal under non-lax set" $
            assertUnparseLabel noLaxForms defaultIdnaFlags
                (ascii "xn--ls8h")
                (Right ("xn--ls8h", FAKEA))
        , testCase "FAKEA emoji Unicode under lax set" $
            assertUnparseLabel laxForms defaultIdnaFlags
                (ascii "xn--ls8h")
                (Right (T.singleton '\x1F4A9', LAXULABEL))
        , testCase "bad-punycode literal under non-lax set" $
            assertUnparseLabel noLaxForms defaultIdnaFlags
                (ascii "xn---hgi")
                (Right ("xn---hgi", FAKEA))
        , testCase "strict set rejects FAKEA" $
            assertUnparseLabel ulabelOnlyForms defaultIdnaFlags
                (ascii "xn--ls8h")
                (Left (ErrFormNotAllowed 0 FAKEA))
        , testCase "EMOJIOK admits emoji as clean ULABEL" $
            assertUnparseLabel laxForms (defaultIdnaFlags <> EMOJIOK)
                (ascii "xn--ls8h")
                (Right (T.singleton '\x1F4A9', ULABEL))
        , testCase "uppercase ASCII body tolerated" $
            assertUnparseLabel laxForms defaultIdnaFlags
                (ascii "xn--Mnchen-3ya")
                (Right (T.pack "m\252nchen", ULABEL))
        ]
    ]
  where
    -- Permissive 'LabelFormSet': admits both literal and lax-decoded
    -- xn-- variants (parallel to the test-local 'laxForms' defined
    -- at the top of the file).
    --
    -- The 'noLaxForms' set drops 'LAXULABEL', so a strict-failing
    -- xn-- label stays literal as 'FAKEA' instead of decoding lax.
    --
    -- The 'ulabelOnlyForms' set drops both 'LAXULABEL' and 'FAKEA',
    -- so a strict-failing xn-- label rejects with 'ErrFormNotAllowed'.
    noLaxForms = foldMap' labelFormToSet
        [LDH, RLDH, ULABEL, ALABEL, FAKEA, ATTRLEAF, WILDLABEL, OCTET]
    ulabelOnlyForms = foldMap' labelFormToSet
        [LDH, RLDH, ULABEL, ATTRLEAF, WILDLABEL, OCTET]

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

-- | Assert that 'unparseLabelOpts' produces the expected
-- @'Either' 'IdnaError' ('Text', 'LabelForm')@ on the given
-- wire-form 'ShortByteString' under the supplied 'LabelFormSet'
-- and 'IdnaFlags'.
assertUnparseLabel
    :: LabelFormSet
    -> IdnaFlags
    -> ShortByteString
    -> Either IdnaError (Text, LabelForm)
    -> IO ()
assertUnparseLabel forms flags input expected =
    let got = unparseLabelOpts forms flags input
    in if got == expected
         then pure ()
         else assertFailure $
             "expected " ++ show expected ++ "\n     got " ++ show got


----------------------------------------------------------------------
-- Smart-constructor / wire-form validation tests.
--
-- 'isValidWireForm' is the predicate the 'Domain' bidirectional
-- pattern synonym uses to gate its writer side.  These cases
-- cover the canonical accept/reject boundary: minimum / maximum
-- lengths, label-length-byte limits, compression-pointer bytes,
-- truncated input, and stray bytes past the root terminator.
----------------------------------------------------------------------

domainWireTests :: Tasty.TestTree
domainWireTests = Tasty.testGroup "isValidWireForm"
    [ testCase "root domain" $
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
        let got = isValidWireForm input
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
            r3 = fmap (wireBytes . fst) (parseDomainUtf8 hostnameLabelForms defaultIdnaFlags inputBs)
            r4 = fmap (wireBytes . fst) (parseDomainShort hostnameLabelForms defaultIdnaFlags inputSbs)
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
        case parseDomainUtf8 hostnameLabelForms defaultIdnaFlags (BS.pack bytes) of
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
        assertRight "mkDomain"      (mkDomain     "www.example.com")
    , testCase "mkDomain rejects invalid name" $
        assertLeft "mkDomain"   (mkDomain     "..bad..")
    , testCase "mkDomainStr accepts valid name" $
        assertRight "mkDomainStr"   (mkDomainStr  "www.example.com")
    , testCase "mkDomainStr rejects invalid name" $
        assertLeft "mkDomainStr"(mkDomainStr  "..bad..")
    , testCase "mkDomainUtf8 accepts valid name" $
        assertRight "mkDomainUtf8"
            (mkDomainUtf8 (T.encodeUtf8 ("www.example.com" :: T.Text)))
    , testCase "mkDomainUtf8 rejects invalid name" $
        assertLeft "mkDomainUtf8"
            (mkDomainUtf8 (T.encodeUtf8 ("..bad.." :: T.Text)))
    , testCase "mkDomainUtf8 rejects bad UTF-8" $
        assertLeft "mkDomainUtf8"
            (mkDomainUtf8 (BS.pack [0xC0, 0x80]))
    , testCase "mkDomainShort accepts valid name" $
        assertRight "mkDomainShort"
            (mkDomainShort (SBS.toShort (T.encodeUtf8 ("www.example.com" :: T.Text))))
    , testCase "mkDomainShort rejects invalid name" $
        assertLeft "mkDomainShort"
            (mkDomainShort (SBS.toShort (T.encodeUtf8 ("..bad.." :: T.Text))))
    ]
  where
    assertRight :: String -> Either a b -> IO ()
    assertRight _    (Right _)  = pure ()
    assertRight name (Left _)   =
        assertFailure (name ++ ": expected Right ..., got Left ...")

    assertLeft :: Show a => String -> Either a b -> IO ()
    assertLeft _    (Left _)  = pure ()
    assertLeft name (Right _) =
        assertFailure (name ++ ": expected Left ..., got Right ...")

----------------------------------------------------------------------
-- Command-line option parsers.
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
tokenVocabularyTests = Tasty.testGroup "command-line option parsers"
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
