{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Main
-- Description : Unicode @IdnaTestV2.txt@ conformance harness.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Runs the Unicode Consortium's @IdnaTestV2.txt@ vectors against
-- 'Text.IDNA2008.parseDomainOpts' chained with
-- 'Text.IDNA2008.domainToUnicodeOpt' (for the cross-label Bidi
-- check), reporting agreement and disagreement with the @toAsciiN@
-- (Nontransitional) column of the file -- the column closest to
-- strict RFC 5891 IDNA2008.  Conformance is a binary check per the
-- file's own format header:
--
-- @
--   Implementations need only record that there is an error: they
--   need not reproduce the precise status codes (after removing
--   the ignored status values).
-- @
--
-- so vectors pass whenever the library's accept\/reject verdict
-- matches UTS \#46's, regardless of the specific error reason.
--
-- The harness is gated on the @unicode-conformance@ Cabal flag (off
-- by default) so that the default @cabal test@ invocation has no
-- network dependency, no extra build-dependencies, and no
-- multi-thousand-vector test report cluttering output.  Enable with:
--
-- @
--    cabal test --flags=+unicode-conformance
-- @
--
-- On first run the harness downloads
-- @https:\/\/www.unicode.org\/Public\/idna\/\<version>\/IdnaTestV2.txt@
-- via @curl@ into a cache directory (default
-- @.cache\/idna-test-v2\/@), and reuses the cached copy on subsequent
-- runs.  Several environment variables tune the behaviour:
--
--   * @IDNA_TEST_V2_FILE@ -- path to a local @IdnaTestV2.txt@.
--     Overrides the cache mechanism entirely; no fetch is attempted.
--   * @IDNA_TEST_V2_VERSION@ -- the Unicode version to fetch when no
--     local file is supplied.  Default: @17.0.0@.
--   * @IDNA_TEST_V2_CACHE_DIR@ -- where fetched files are stored.
--     Default: @.cache\/idna-test-v2@.
--
-- Known systematic disagreements between strict IDNA2008 and UTS #46
-- Nontransitional are listed in
-- @tests\/data\/idna-test-v2-expected-diffs.txt@.  Each entry there pairs a
-- source string with a disposition (@skip@ \/ @xfail@) plus a brief reason.
-- See the file's header comment for the format.
module Main (main) where

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Exception (catch)
import Control.Monad (unless, when)
#if !MIN_VERSION_base(4,20,0)
import Data.Foldable(foldl')
#endif
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Numeric (readHex)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure)

import Text.IDNA2008
    ( AceReason(..)
    , IdnaError(..)
    , IdnaFlags
    , LabelFormSet
    , LabelReason(..)
    , allLabelForms
    , defaultIdnaFlags
    , domainToUnicodeOpt
    , parseDomainOpts
    , parseIdnaFlagsStr
    , parseLabelFormSetStr
    )

----------------------------------------------------------------------
-- Knobs / source resolution
----------------------------------------------------------------------

-- | Read the @x-unicode-version@ custom field from @idna2008.cabal@,
-- which is the single source of truth for the Unicode version the
-- library's generated tables track.  Reported in stderr alongside
-- the loaded-vector count so the user knows which Unicode version
-- the bundled @tests\/data\/\*.xz@ snapshots reflect.
cabalUnicodeVersion :: IO String
cabalUnicodeVersion = do
    body <- readFile cabalPath `catch` \(e :: IOError) ->
        fail $ "unicode-conformance: could not read " ++ cabalPath
            ++ ": " ++ show e
    case extract body of
      Just v  -> pure v
      Nothing -> fail $
          "unicode-conformance: x-unicode-version field not found in "
          ++ cabalPath
  where
    cabalPath  = "idna2008.cabal"
    fieldName  = "x-unicode-version:"
    extract body =
        case dropWhile (not . (fieldName `isPrefixOf`)) (lines body) of
          (line:_) -> Just (trim (drop (length fieldName) line))
          []       -> Nothing

-- | Path to the bundled gzip-compressed @IdnaTestV2.txt@ snapshot.
-- Listed under @extra-source-files@ in @idna2008.cabal@ so it
-- travels with @cabal sdist@.
bundledVectorsFile :: FilePath
bundledVectorsFile = "tests/data/IdnaTestV2.txt.gz"

-- | Path to the bundled gzip-compressed @IdnaMappingTable.txt@
-- snapshot, used by the precise cp-disagreement filter.
bundledMappingFile :: FilePath
bundledMappingFile = "tests/data/IdnaMappingTable.txt.gz"

-- | Where to look for the expected-diffs file (relative to the
-- directory @cabal test@ is invoked from, normally the package root).
-- Missing file is treated as \"no known diffs\".
defaultExpectedDiffsFile :: FilePath
defaultExpectedDiffsFile = "tests/data/idna-test-v2-expected-diffs.txt"

-- | Read a UTS \#46 data file.  Honours an environment-variable
-- override (callers pass e.g. @\"IDNA_TEST_V2_FILE\"@) pointing at
-- an /uncompressed/ plain-text local copy; otherwise reads the
-- bundled gzip-compressed snapshot at @bundledPath@ and
-- decompresses in memory.  The env override is the supported way
-- to test against a Unicode version other than the bundled one
-- without repacking @tests\/data\/@.
readUnicodeFile :: FilePath -> String -> IO String
readUnicodeFile bundledPath envOverride = do
    mPath <- lookupEnv envOverride
    case mPath of
      Just path -> readFile path
      Nothing   -> do
          compressed <- BL.readFile bundledPath
          let decompressed = GZip.decompress compressed
          pure (T.unpack (T.decodeUtf8 (BL.toStrict decompressed)))

----------------------------------------------------------------------
-- Parser for IdnaTestV2.txt
--
-- File format (as documented in the file's own header):
--
--   * @#@ starts a comment that runs to end-of-line.
--   * Blank lines are ignored.
--   * Other lines have seven semicolon-separated fields:
--
--       source ; toUnicode ; toUnicodeStatus ;
--       toAsciiN ; toAsciiNStatus ;
--       toAsciiT ; toAsciiTStatus
--
--   * An empty @toUnicode@ field means "same as @source@".
--   * An empty @toAsciiN@ field means "same as @toUnicode@".
--   * An empty @toAsciiT@ field means "same as @toAsciiN@".
--   * An empty @toUnicodeStatus@\/@toAsciiNStatus@\/@toAsciiTStatus@
--     field means "OK" (no errors expected).
--   * Status fields, when non-empty, are space-separated lists of
--     bracketed codes like @[V5]@, @[A3]@, @[Bn]@, @[Pn]@, @[Cn]@.
--     UTS #46 section 6 defines the code vocabulary.
--   * Source strings can contain @\\uXXXX@ escapes that resolve to the
--     literal codepoint.
----------------------------------------------------------------------

-- | One vector loaded from @IdnaTestV2.txt@, with inheritance applied
-- (empty fields filled in from earlier columns per the file's rules).
data Vector = Vector
    { vSource          :: !String   -- ^ Input string.
    , vToUnicode       :: !String   -- ^ Expected toUnicode output.
    , vToUnicodeStatus :: ![String] -- ^ Expected toUnicode status codes.
    , vToAsciiN        :: !String   -- ^ Expected toAsciiN output.
    , vToAsciiNStatus  :: ![String] -- ^ Expected toAsciiN status codes.
    , vToAsciiT        :: !String   -- ^ Expected toAsciiT output.
    , vToAsciiTStatus  :: ![String] -- ^ Expected toAsciiT status codes.
    , vLineNo          :: !Int      -- ^ Source-file line for diagnostics.
    } deriving (Show, Eq)

-- | Load and parse the bundled (or env-overridden) @IdnaTestV2.txt@.
loadVectors :: IO [Vector]
loadVectors = do
    raw <- readUnicodeFile bundledVectorsFile "IDNA_TEST_V2_FILE"
    pure (parseVectors raw)

parseVectors :: String -> [Vector]
parseVectors body =
    [ v
    | (lineNo, line) <- zip [1..] (lines body)
    , let stripped = stripComment line
    , not (allBlank stripped)
    , Just v <- [parseLine lineNo stripped]
    ]
  where
    stripComment s = takeWhile (/= '#') s
    allBlank       = all (`elem` (" \t" :: String))

parseLine :: Int -> String -> Maybe Vector
parseLine lineNo s =
    case splitOn ';' s of
      [src, tu, tus, taN, taNs, taT, taTs] ->
          let src'    = decodeFieldString (trim src) ""
              tu'     = decodeFieldString (trim tu)  src'
              taN'    = decodeFieldString (trim taN) tu'
              taT'    = decodeFieldString (trim taT) taN'
              -- Status-column inheritance per the file's own format
              -- header (column 3-7 documentation):
              --
              --   * Column 3 (toUnicodeStatus): blank means @[]@ (no
              --     errors).
              --   * Column 5 (toAsciiNStatus): blank means /same as
              --     toUnicodeStatus/.  An explicit @[]@ means no
              --     errors.
              --   * Column 7 (toAsciiTStatus): blank means /same as
              --     toAsciiNStatus/.  An explicit @[]@ means no
              --     errors.
              --
              -- 'parseStatusField' distinguishes blank ('Nothing')
              -- from explicit ('Just'), so we can resolve inheritance.
              tuStat  = fromMaybe [] (parseStatusField tus)
              taNStat = fromMaybe tuStat  (parseStatusField taNs)
              taTStat = fromMaybe taNStat (parseStatusField taTs)
          in Just Vector
              { vSource          = src'
              , vToUnicode       = tu'
              , vToUnicodeStatus = tuStat
              , vToAsciiN        = taN'
              , vToAsciiNStatus  = taNStat
              , vToAsciiT        = taT'
              , vToAsciiTStatus  = taTStat
              , vLineNo          = lineNo
              }
      _ -> Nothing

-- | Parse a status field.  Distinguishes:
--
--   * @Nothing@ -- the field was blank (no characters between the
--     semicolon separators).  Per the file's format, blank in
--     toAsciiN\/T status columns means \"inherit the previous
--     column's value\"; the caller resolves that.
--   * @Just []@ -- the field was an explicit @[]@ marker meaning
--     \"no errors expected\".
--   * @Just xs@ -- the field carried one or more bracketed status
--     codes (e.g.  @[V5]@, @[V5, B3]@, @[V5] [B3]@); 'xs' is the
--     code list with brackets, commas, and inter-code whitespace
--     stripped.
parseStatusField :: String -> Maybe [String]
parseStatusField raw =
    case trim raw of
      ""   -> Nothing
      body -> Just (filter (not . null) (words (map normalize body)))
  where
    normalize c
      | c == '[' || c == ']' || c == ',' = ' '
      | otherwise                        = c

-- | Interpret a trimmed value-column field per the file's format
-- header (column 1, 2, 4, 6 documentation):
--
--   * @\"\"@ (literal two double-quotes) means the empty string.
--   * Any other non-empty content is the literal field value, with
--     @\\uXXXX@ \/ @\\x{HEX}@ escapes decoded.
--   * Empty content (after trimming) inherits the supplied parent.
--
-- 'parent' is the value to inherit on empty (caller chooses what's
-- meaningful for the column being parsed).
decodeFieldString :: String -> String -> String
decodeFieldString trimmed parent = case trimmed of
    ""      -> parent
    "\"\""  -> ""
    other   -> decodeEscapes other

-- | UTS #46's source field uses @\\uXXXX@ for non-printable / non-ASCII
-- codepoints.  Decode those (and the few backslash escapes the file
-- uses) into literal characters.
decodeEscapes :: String -> String
decodeEscapes = go
  where
    go [] = []
    go ('\\':'u':a:b:c:d:rest)
      | all isHex [a,b,c,d] =
          toEnum (readHex4 [a,b,c,d]) : go rest
    go ('\\':'x':'{':rest) =
        let (hex, more) = break (== '}') rest
        in case more of
             '}':more' | not (null hex) && all isHex hex ->
                 toEnum (readHexAny hex) : go more'
             _ -> '\\' : 'x' : '{' : go rest
    go (c:rest) = c : go rest

    isHex c = (c >= '0' && c <= '9')
           || (c >= 'a' && c <= 'f')
           || (c >= 'A' && c <= 'F')
    readHex4 cs = readHexAny cs
    readHexAny = foldl (\acc c -> acc * 16 + hexDigit c) 0
    hexDigit c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = 10 + fromEnum c - fromEnum 'a'
      | c >= 'A' && c <= 'F' = 10 + fromEnum c - fromEnum 'A'
      | otherwise            = 0

splitOn :: Char -> String -> [String]
splitOn sep = foldr go [[]]
  where
    go c acc@(cur:rest)
      | c == sep  = [] : acc
      | otherwise = (c:cur) : rest
    go _ []       = [[]]

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
  where isSpace c = c == ' ' || c == '\t'

----------------------------------------------------------------------
-- Parser for IdnaMappingTable.txt
--
-- File format (per UTS \#46 \"Section 5: IDNA Mapping Table\"):
--
--   <cp>[..<cp>] ; <status> [; <mapping>] [; <idna2008-status>]
--                                                            # comment
--
-- Statuses we care about for marking a codepoint as \"UTS \#46 treats
-- this more permissively than strict IDNA2008\":
--
--   * @valid@ with a 4th-column tag of @NV8@ or @XV8@ -- the cp is
--     valid under IDNA2003\/legacy IDNA but disallowed by IDNA2008's
--     §5892 disposition table.  The file header explicitly tells
--     strict-IDNA2008 implementations to skip these.
--   * @mapped@ -- UTS \#46 rewrites the cp to the mapping target.
--     We don't apply that rewrite, so we see the original cp and
--     reject it.
--   * @deviation@ -- UTS \#46 conditionally rewrites under
--     Transitional Processing; under Nontransitional it's left
--     valid, but RFC 5891 may not treat the cp the same way.
--   * @ignored@ -- UTS \#46 removes the cp.  We keep it, which can
--     turn an otherwise-valid label into a leading-combining-mark
--     violation (e.g. variation selectors at the start of a label).
--   * @disallowed_STD3_mapped@ -- under default UTS \#46 settings
--     (STD3 = true) UTS \#46 rejects; under STD3 = false it maps.
--     IDNA2008 always rejects.  Treat as permissive for safety.
--
-- All other statuses (@valid@ without NV8\/XV8, @disallowed@,
-- @disallowed_STD3_valid@) are agreed-on between the two specs.
----------------------------------------------------------------------

-- | Codepoints UTS \#46 treats more permissively than strict
-- IDNA2008, as extracted from @IdnaMappingTable.txt@.  Membership in
-- this set is the precise signal the comparator uses to decide that
-- a 'DisallowedCodepoint' or 'LeadingCombiningMark' rejection from
-- our library reflects a documented spec disagreement rather than a
-- library bug.
loadMappingTable :: IO (Set Int)
loadMappingTable = do
    body <- readUnicodeFile bundledMappingFile "IDNA_MAPPING_TABLE_FILE"
    pure $ Set.fromList
        [ cp
        | line <- lines body
        , let payload = takeWhile (/= '#') line
        , not (allBlank payload)
        , Just (lo, hi) <- [parseMappingLine payload]
        , cp <- [lo .. hi]
        ]
  where
    allBlank = all (`elem` (" \t" :: String))

-- | Parse one non-comment line of @IdnaMappingTable.txt@; return the
-- inclusive range @(lo, hi)@ if and only if the line's status marks
-- the codepoint(s) as a UTS \#46-vs-IDNA2008 disagreement.
parseMappingLine :: String -> Maybe (Int, Int)
parseMappingLine line = case map trim (splitOn ';' line) of
    (cpField : status : rest) | isPermissive status rest ->
        parseRange cpField
    _ -> Nothing

-- | A status (column 2) and the trailing columns indicate a
-- UTS \#46-more-permissive disposition iff one of the following:
isPermissive :: String -> [String] -> Bool
isPermissive status rest = case status of
    "mapped"                 -> True
    "deviation"              -> True
    "ignored"                -> True
    "disallowed_STD3_mapped" -> True
    "valid"                  -> hasNvXv rest
    _                        -> False
  where
    -- @valid@ rows tag NV8/XV8 in either the 3rd or 4th column,
    -- depending on whether the mapping field is present.
    hasNvXv fields = any (\f -> trim f == "NV8" || trim f == "XV8")
                         fields

-- | Parse a @cp@ or @cp..cp@ field.  Hex with no @0x@ prefix.
parseRange :: String -> Maybe (Int, Int)
parseRange s = case break (== '.') (trim s) of
    (lo, "")             -> singleton (parseHex lo)
    (lo, '.':'.':hiRest) -> do
        a <- parseHex lo
        b <- parseHex hiRest
        pure (a, b)
    _                    -> Nothing
  where
    singleton (Just a) = Just (a, a)
    singleton Nothing  = Nothing

    parseHex :: String -> Maybe Int
    parseHex hex = case readHex (trim hex) of
        [(n, "")] -> Just n
        _         -> Nothing

----------------------------------------------------------------------
-- Expected-diffs file
----------------------------------------------------------------------

-- | How to treat a vector that the expected-diffs file mentions.
data Disposition
    = DSkip   -- ^ Drop the vector before running it.
    | DXFail  -- ^ Expect the comparator to report a disagreement; if
              --   it agrees instead, that's an unexpected pass worth
              --   investigating.
    deriving (Eq, Show)

-- | Load the expected-diffs file.  Each non-blank, non-comment line is
-- @\<disposition>\\t\<source>\\t\<reason>@; lines starting with @#@ are
-- comments.  Missing file returns an empty map.
loadExpectedDiffs :: FilePath -> IO (Map String (Disposition, String))
loadExpectedDiffs path = do
    exists <- doesFileExist path
    if not exists
      then pure Map.empty
      else do
          body <- readFile path
          pure $ Map.fromList
              [ (decodeEscapes src, (disp, reason))
              | line <- lines body
              , not (isCommentLine line)
              , Just (disp, src, reason) <- [parseDiffLine line]
              ]
  where
    isCommentLine line = case dropWhile (`elem` (" \t" :: String)) line of
        ('#':_) -> True
        ""      -> True
        _       -> False

    parseDiffLine line = case splitOn '\t' line of
        (d:s:rs) -> do
            disp <- case dropWhile (`elem` (" \t" :: String)) d of
                "skip"  -> Just DSkip
                "xfail" -> Just DXFail
                _       -> Nothing
            pure (disp, s, unwords (filter (not . null) rs))
        _ -> Nothing

----------------------------------------------------------------------
-- Library flag and label-form set chosen to approximate UTS #46
-- Nontransitional semantics.
----------------------------------------------------------------------

-- | Closest match to UTS #46 Nontransitional: the parser default
-- (which already enables strict A-label check, strict NFC validation,
-- and per-label Bidi check) plus all four mapping toggles
-- (@map-case@, @map-width@, @map-nfc@, @map-dots@).  Combined under
-- the @\"default,map\"@ token preset; we feed the library default
-- 'IdnaFlags' as the base value the @default@ token resolves to.
uts46Flags :: IdnaFlags
uts46Flags = case parseIdnaFlagsStr defaultIdnaFlags "default,map" of
    Right fs -> fs
    Left e   -> error ("uts46Flags: " ++ e)

-- | UTS #46 only admits A-, U-, and conventional LDH labels.  FAKEA,
-- OCTET, ATTRLEAF, BLANK, WILDLABEL are not part of its domain-name
-- vocabulary; we use the @idn@ preset to reject them up front so the
-- comparator doesn't try to map their library-internal classifications
-- onto UTS #46 codes that don't exist.  'allLabelForms' is fed as the
-- base value for the @default@ token (not used in our preset, but the
-- parser requires a default to resolve to).
uts46Forms :: LabelFormSet
uts46Forms = case parseLabelFormSetStr allLabelForms "idn" of
    Right fs -> fs
    Left e   -> error ("uts46Forms: " ++ e)

----------------------------------------------------------------------
-- (No specific-code matching: per the file's own header, conformance
-- is binary error-or-no-error.  The previous attempt to map our
-- 'IdnaError' constructors onto UTS \#46 codes is intentionally
-- absent here.)
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Comparator
----------------------------------------------------------------------

data Outcome
    = OPass
      -- ^ Library matched UTS #46.
    | OUnexpectedAccept ![String]
      -- ^ UTS expected error; we succeeded.  The list is the
      --   expected status set.
    | OUnexpectedReject !IdnaError
      -- ^ UTS expected success; we rejected.
    | OCpDisagreement !IdnaError
      -- ^ Library rejection traces back to a codepoint UTS \#46
      --   would have preprocessed away (or accepted) but strict
      --   IDNA2008 can't.  Covers three flavours:
      --
      --   * 'DisallowedCodepoint' (direct or via 'DecodedInvalid')
      --     -- the NV8\/XV8 cases the file header explicitly tells
      --     strict implementations to skip, plus UTS \#46's
      --     NFKC-style mapped codepoints we don't apply.
      --   * 'LeadingCombiningMark' on a codepoint UTS \#46 treats
      --     as @ignored@ (e.g. variation selectors @U+FE00@ -
      --     @U+FE0F@): UTS \#46 removes them, exposing the next
      --     codepoint as the label start; we keep them and
      --     reject under RFC 5891 §4.2.3.2.
      --
      --   Auto-skipped to avoid drowning real bugs.
    | ORootDifference ![String]
      -- ^ Library accepted a domain UTS #46 rejects as A4_1\/A4_2
      --   \/X4_2, and the source is empty, all-dot-mappings, or
      --   ends in one.  Captures the DNS convention (trailing
      --   dot = absolute root) versus UTS #46's \"no empty label
      --   anywhere\".  Auto-skipped.

-- | The UTS \#46 file header is explicit that conformance is a
-- binary error-or-no-error check, not a specific-code match:
--
-- @
--   Implementations need only record that there is an error: they
--   need not reproduce the precise status codes (after removing
--   the ignored status values).
-- @
--
-- So the comparator collapses to four outcomes:
--
--   * Both sides agree on success                       -> 'OPass'
--   * Both sides agree on /some/ error                  -> 'OPass'
--   * UTS \#46 expected an error, library accepted      -> 'OUnexpectedAccept'
--     (or 'ORootDifference' for the DNS-root subset)
--   * UTS \#46 expected success, library rejected       -> 'OUnexpectedReject'
--     (or 'OCpDisagreement' for the documented spec-
--     disagreement subset)
runVector :: Set Int -> Vector -> Outcome
runVector permissiveCps v = case fullPipeline input of
    Right ()
      | null expected                     -> OPass
      | isRootLike (vSource v)
        && any isLenCode expected         -> ORootDifference expected
      | otherwise                         -> OUnexpectedAccept expected
    Left err
      | not (null expected)               -> OPass
      | isCpDisagreement err              -> OCpDisagreement err
      | otherwise                         -> OUnexpectedReject err
  where
    input      = T.pack (vSource v)
    expected   = vToAsciiNStatus v

    isLenCode c = c == "A4_1" || c == "A4_2" || c == "X4_2"

    -- UTS #46 treats every empty label (including the canonical
    -- DNS \"trailing dot = root\" form) as A4_2; the library
    -- treats a trailing dot as the absolute-root indicator
    -- consistent with master-file syntax.  These two views
    -- disagree by definition on inputs that are empty, consist
    -- solely of label-separator characters, or end in one.
    isRootLike s = null s || all isDotMap s || lastIsDotMap s
      where
        lastIsDotMap [] = False
        lastIsDotMap xs = isDotMap (last xs)
        isDotMap c = c == '.'      -- ASCII full stop
                  || c == '\x3002' -- ideographic full stop
                  || c == '\xFF0E' -- fullwidth full stop
                  || c == '\xFF61' -- halfwidth ideographic full stop

    -- Precise cp-disagreement test: the offending codepoint is one
    -- IdnaMappingTable.txt marks as @mapped@, @deviation@,
    -- @ignored@, or @valid;NV8/XV8@ -- the documented UTS \#46-vs-
    -- IDNA2008 disagreement set.  If 'permissiveCps' is empty
    -- (mapping table failed to load) the check degrades to
    -- \"always false\", which makes such disagreements surface as
    -- 'OUnexpectedReject' rather than being silently absorbed.
    -- Only consulted when @UTS \#46 expected success@; if UTS
    -- already expected /some/ error, the comparator counts our
    -- error as PASS regardless of the specific reason.
    isCpDisagreement = \case
        ErrLabelInvalid _ (DisallowedCodepoint cp)                  ->
            cp `Set.member` permissiveCps
        ErrAceInvalid   _ (DecodedInvalid (DisallowedCodepoint cp)) ->
            cp `Set.member` permissiveCps
        ErrLabelInvalid _ (LeadingCombiningMark cp)                 ->
            cp `Set.member` permissiveCps
        ErrAceInvalid   _ (DecodedInvalid (LeadingCombiningMark cp)) ->
            cp `Set.member` permissiveCps
        _                                                           ->
            False

-- | UTS #46 \"toAsciiN\" composes per-label parsing with the
-- cross-label Bidi check ('renderUnicodeTextChecked' inside
-- 'domainToUnicodeOpt').  'parseDomainOpts' alone only enforces the
-- per-label half of RFC 5893, so we chain the two: parse, then if
-- parse succeeded, run the cross-label check.  We discard the
-- rendered Unicode text the second step returns -- only its
-- error\/success status matters for conformance.
fullPipeline :: T.Text -> Either IdnaError ()
fullPipeline input = do
    (dom, _info) <- parseDomainOpts uts46Forms uts46Flags input
    _ <- domainToUnicodeOpt uts46Flags dom
    pure ()

----------------------------------------------------------------------
-- Aggregation
----------------------------------------------------------------------

data Counts = Counts
    { cTotal          :: !Int
    , cPass           :: !Int
    , cSkip           :: !Int        -- ^ Explicit skip via expected-diffs.
    , cXFail          :: !Int        -- ^ Expected-diffs xfail confirmed.
    , cUPass          :: !Int        -- ^ XFail predicted; actually agreed.
    , cCpDisagree     :: !Int        -- ^ Auto-skip: NV8\/XV8\/NFKC-mapped.
    , cRootDifference :: !Int        -- ^ Auto-skip: DNS root vs UTS #46.
    , cDiff           :: !Int
    , cDiffSamples    :: ![String]   -- ^ First N unexpected-diff lines.
    , cUPassSamples   :: ![String]   -- ^ First N unexpected-pass lines.
    }

emptyCounts :: Counts
emptyCounts = Counts 0 0 0 0 0 0 0 0 [] []

-- | Cap on how many lines of detail we accumulate in the failure
-- message.  Anything beyond this is summarised numerically.
sampleLimit :: Int
sampleLimit = 30

step
    :: Set Int
    -> Map String (Disposition, String)
    -> Counts -> Vector -> Counts
step permissiveCps diffs c v =
    let total' = cTotal c + 1
        bumped = c { cTotal = total' }
    in case Map.lookup (vSource v) diffs of
         Just (DSkip, _) -> bumped { cSkip = cSkip c + 1 }
         mDiff -> case runVector permissiveCps v of
             OPass -> case mDiff of
                 Just (DXFail, reason) ->
                     bumped { cUPass = cUPass c + 1
                            , cUPassSamples =
                                addSample sampleLimit (renderUPass v reason)
                                          (cUPassSamples c)
                            }
                 _ -> bumped { cPass = cPass c + 1 }
             OCpDisagreement{} ->
                 -- Heuristic auto-skip: never counted as a diff,
                 -- regardless of xfail entries.  An explicit xfail
                 -- entry for one of these would be redundant.
                 bumped { cCpDisagree = cCpDisagree c + 1 }
             ORootDifference{} ->
                 bumped { cRootDifference = cRootDifference c + 1 }
             outcome -> case mDiff of
                 Just (DXFail, _) ->
                     bumped { cXFail = cXFail c + 1 }
                 _ ->
                     bumped { cDiff = cDiff c + 1
                            , cDiffSamples =
                                addSample sampleLimit (renderDiff v outcome)
                                          (cDiffSamples c)
                            }

addSample :: Int -> a -> [a] -> [a]
addSample lim x xs
    | length xs < lim = xs ++ [x]
    | otherwise       = xs

renderDiff :: Vector -> Outcome -> String
renderDiff v outcome = case outcome of
    OPass -> ""
    OCpDisagreement{}  -> ""    -- auto-skipped, never rendered
    ORootDifference{}  -> ""    -- auto-skipped, never rendered
    OUnexpectedAccept exp_ -> concat
        [ "  L", pad6 (vLineNo v)
        , "  accepted but UTS expected ", show exp_
        , ": ",   show (vSource v)
        ]
    OUnexpectedReject err -> concat
        [ "  L", pad6 (vLineNo v)
        , "  UTS expected success, library returned "
        , showError err
        , ": ", show (vSource v)
        ]
  where
    pad6 n = let s = show n in replicate (max 0 (6 - length s)) ' ' ++ s

renderUPass :: Vector -> String -> String
renderUPass v reason = concat
    [ "  L", show (vLineNo v)
    , "  xfail predicted but library agreed with UTS"
    , (if null reason then "" else " (" ++ reason ++ ")")
    , ": ", show (vSource v)
    ]

-- | Compact one-liner for an 'IdnaError', avoiding the verbose
-- generic 'show' for record-shaped errors that quote @IdnaLoc@s.
showError :: IdnaError -> String
showError = \case
    ErrEmptyLabel{}              -> "EmptyLabel"
    ErrLabelTooLong _ n          -> "LabelTooLong(" ++ show n ++ ")"
    ErrNameTooLong n             -> "NameTooLong(" ++ show n ++ ")"
    ErrBadEscape{}               -> "BadEscape"
    ErrInvalidUtf8{}             -> "InvalidUtf8"
    ErrCodepointTooLarge _ cp    -> "CodepointTooLarge(U+" ++ hex cp ++ ")"
    ErrUnpresentableLabel{}      -> "UnpresentableLabel"
    ErrFormNotAllowed _ f        -> "FormNotAllowed(" ++ show f ++ ")"
    ErrLabelInvalid _ r          -> "LabelInvalid(" ++ showReason r ++ ")"
    ErrAceInvalid   _ r          -> "AceInvalid(" ++ showAceReason r ++ ")"
    ErrPunycodeOverflow{}        -> "PunycodeOverflow"
    ErrCrossLabelBidi i rule     -> "CrossLabelBidi(" ++ show i ++ "," ++ show rule ++ ")"
  where
    hex cp = let s = showHex' cp in replicate (max 0 (4 - length s)) '0' ++ s
    showHex' 0 = "0"
    showHex' n = go n ""
      where go 0 acc = acc
            go k acc = go (k `div` 16) (h (k `mod` 16) : acc)
            h x | x < 10    = toEnum (x + fromEnum '0')
                | otherwise = toEnum (x - 10 + fromEnum 'a')

showReason :: LabelReason -> String
showReason = \case
    DisallowedCodepoint cp -> "Disallowed(U+" ++ hex4 cp ++ ")"
    ContextRule cp         -> "Context(U+" ++ hex4 cp ++ ")"
    NotNFC                 -> "NotNFC"
    LabelBidi rule         -> "Bidi(" ++ show rule ++ ")"
    HyphenViolation        -> "Hyphen"
    LeadingCombiningMark cp -> "LeadingCM(U+" ++ hex4 cp ++ ")"
  where
    hex4 cp = let s = sh cp in replicate (max 0 (4 - length s)) '0' ++ s
    sh 0 = "0"
    sh n = go n ""
      where go 0 acc = acc
            go k acc = go (k `div` 16) (h (k `mod` 16) : acc)
            h x | x < 10    = toEnum (x + fromEnum '0')
                | otherwise = toEnum (x - 10 + fromEnum 'a')

showAceReason :: AceReason -> String
showAceReason = \case
    BadPunycode        -> "BadPunycode"
    DecodedInvalid r   -> "DecodedInvalid(" ++ showReason r ++ ")"
    RoundTripMismatch  -> "RoundTripMismatch"

summary :: Counts -> String
summary c = unlines
    [ "  total                  : " ++ show (cTotal c)
    , "  pass                   : " ++ show (cPass c)
    , "  skip (explicit)        : " ++ show (cSkip c)
    , "  xfail (explicit)       : " ++ show (cXFail c)
    , "  auto-skip: cp-disagree : " ++ show (cCpDisagree c)
    , "  auto-skip: root-diff   : " ++ show (cRootDifference c)
    , "  unexpected pass        : " ++ show (cUPass c)
    , "  unexpected diff        : " ++ show (cDiff c)
    ]

----------------------------------------------------------------------
-- Test tree
----------------------------------------------------------------------

main :: IO ()
main = do
    ver           <- cabalUnicodeVersion
    vectors       <- loadVectors
    permissiveCps <- loadMappingTable
    diffs         <- loadExpectedDiffs defaultExpectedDiffsFile
    hPutStrLn stderr $
        "unicode-conformance: Unicode " ++ ver
        ++ "; " ++ show (length vectors) ++ " vectors, "
        ++ show (Set.size permissiveCps) ++ " UTS#46-permissive codepoints"
    unless (Map.null diffs) $
        hPutStrLn stderr $
            "unicode-conformance: loaded " ++ show (Map.size diffs)
            ++ " expected-diff entries from "
            ++ defaultExpectedDiffsFile
    defaultMain (conformanceTree ver vectors diffs permissiveCps)

conformanceTree
    :: String
    -> [Vector]
    -> Map String (Disposition, String)
    -> Set Int
    -> TestTree
conformanceTree ver vectors diffs permissiveCps =
    testGroup ("IdnaTestV2.txt for Unicode " ++ ver)
    [ testCase "conformance" $ do
        let !counts = foldl' (step permissiveCps diffs) emptyCounts vectors
        hPutStrLn stderr (summary counts)
        when (cDiff counts > 0 || cUPass counts > 0) $ do
            let report = unlines $
                    [ ""
                    , "unicode-conformance: comparator disagreed with UTS #46"
                    , "Nontransitional on " ++ show (cDiff counts) ++ " vector(s)"
                    , "and saw " ++ show (cUPass counts)
                                 ++ " unexpected pass(es)."
                    , ""
                    , summary counts
                    ]
                    ++ (if null (cDiffSamples counts)
                          then []
                          else ("First " ++ show (length (cDiffSamples counts))
                                         ++ " unexpected diffs:")
                               : cDiffSamples counts)
                    ++ (if null (cUPassSamples counts)
                          then []
                          else ("First " ++ show (length (cUPassSamples counts))
                                         ++ " unexpected passes:")
                               : cUPassSamples counts)
                    ++ [ ""
                       , "Tune tests/data/idna-test-v2-expected-diffs.txt to mark"
                       , "known UTS#46-vs-IDNA2008 disagreements (skip/xfail)."
                       ]
            assertFailure report
    ]
