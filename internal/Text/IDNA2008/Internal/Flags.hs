-- |
-- Module      : Text.IDNA2008.Internal.Flags
-- Description : Parser and presentation flags.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Knobs that control how strictly the parser validates a domain
-- name and how leniently it cleans up its input.  Composed with
-- @('<>')@; tested with 'meetsIdnaFlags'.  The data constructor
-- is intentionally not exposed; callers compose values from the
-- singleton patterns.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.IDNA2008.Internal.Flags
    ( -- * Flag set with bundled singleton patterns
      IdnaFlags( ALABELCHECK
               , NFCCHECK
               , EMOJIOK
               , MAPDOTS
               , MAPNFC
               , MAPCASE
               , MAPWIDTH
               , BIDICHECK
               , ASCIIFALLBACK
               , LAXDECODE
               )
    , defaultIdnaFlags
    , effectiveIdnaFlags
    , meetsIdnaFlags
    , withoutIdnaFlags

      -- * CLI token vocabulary
    , idnaFlagsTokens
    , idnaFlagsPresets
    , parseIdnaFlags
    , parseIdnaFlagsStr
    ) where

import Control.Monad ((>=>))
import Data.Bits ((.|.), (.&.), complement, unsafeShiftR)
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Word (Word16)

import Text.IDNA2008.Internal.Tokens
    ( TokenEntry(..), Preset, asciiByteString, parseTokens )

-- | Flag set for the parser, stored as a bitmask of one-bit
-- pattern singletons.
newtype IdnaFlags = IdnaFlags Word16
    deriving (Eq, Ord)

instance Semigroup IdnaFlags where
    IdnaFlags a <> IdnaFlags b = IdnaFlags (a .|. b)
    {-# INLINE (<>) #-}

instance Monoid IdnaFlags where
    mempty = IdnaFlags 0
    {-# INLINE mempty #-}

instance Show IdnaFlags where
    show o = case flagNames o of
        []  -> "mempty"
        ns  -> "(" ++ intercalate "<>" ns ++ ")"

flagNames :: IdnaFlags -> [String]
flagNames (IdnaFlags w) = go names w
  where
    names = [ "ALABELCHECK", "NFCCHECK", "EMOJIOK", "MAPDOTS"
            , "MAPNFC", "MAPCASE", "MAPWIDTH", "BIDICHECK"
            , "ASCIIFALLBACK", "LAXDECODE" ]
    go _ 0  = []
    go [] _ = []
    go (s:ss) n
        | odd n     = s : go ss (n `unsafeShiftR` 1)
        | otherwise = go ss (n `unsafeShiftR` 1)

----------------------------------------------------------------------
-- Pattern singletons
----------------------------------------------------------------------

-- | Strict A-label check.  An @\"xn--\"@-prefixed label whose
-- Punycode body doesn't decode to a valid IDN label, or whose
-- re-encoding doesn't match the input bytes, classifies as
-- 'Text.IDNA2008.Internal.LabelForm.FAKEA' rather than
-- 'Text.IDNA2008.Internal.LabelForm.ALABEL'.  Without this
-- flag every well-formed @\"xn--\"@ LDH label is reported as
-- 'Text.IDNA2008.Internal.LabelForm.ALABEL'.
pattern ALABELCHECK :: IdnaFlags
pattern ALABELCHECK = IdnaFlags 0x0001

-- | Require Unicode Normalization Form C on labels with non-ASCII
-- content.  Labels with combining marks in non-canonical order,
-- or decomposed sequences with a precomposed equivalent, are
-- rejected.  Without this flag no normalization check is
-- performed.
pattern NFCCHECK :: IdnaFlags
pattern NFCCHECK = IdnaFlags 0x0002

-- | Diagnostic relaxation: codepoints that carry the Unicode
-- @Emoji=Yes@ property are admitted even if they're otherwise
-- rejected by the IDN allowed-codepoint rules.  Useful when
-- analysing real-world IDN data that includes emoji
-- registrations.  Off by default.
pattern EMOJIOK :: IdnaFlags
pattern EMOJIOK = IdnaFlags 0x0004

-- | Treat the East Asian period characters (the ideographic
-- period @U+3002@ and its fullwidth and halfwidth variants) as
-- label separators, just like @\'.\'@ (@U+002E@).  Off by
-- default; some user-input contexts need this.
pattern MAPDOTS :: IdnaFlags
pattern MAPDOTS = IdnaFlags 0x0008

-- | Combine letter-and-accent sequences into single characters
-- where possible, before validation.  An input with decomposed
-- letter+combining-mark sequences is normalised to its
-- precomposed equivalent.  Affects only labels with non-ASCII
-- content.
pattern MAPNFC :: IdnaFlags
pattern MAPNFC = IdnaFlags 0x0010

-- | Lowercase the input.  Without this flag the parser
-- preserves input case verbatim, so uppercase ASCII in a
-- pure-LDH label survives into the wire form, and an
-- uppercase letter in a label with non-ASCII content is
-- rejected as a @DISALLOWED@ codepoint under strict
-- IDNA2008.  With the flag set: ASCII A-Z is lowercased
-- before classification, and any other letter with a known
-- Unicode lowercase form is lowercased on the U-label path.
-- After mapping, validation runs against the lowercased
-- form.
pattern MAPCASE :: IdnaFlags
pattern MAPCASE = IdnaFlags 0x0020

-- | Convert fullwidth and halfwidth characters to their normal-
-- width form before validation.  Fullwidth Latin letters become
-- ordinary ASCII letters, halfwidth katakana becomes regular
-- katakana, etc.  Implies 'MAPDOTS' (lifted by
-- 'effectiveIdnaFlags') because two of the fullwidth\/halfwidth
-- forms are label-separator characters that must be recognised
-- before per-label processing.
pattern MAPWIDTH :: IdnaFlags
pattern MAPWIDTH = IdnaFlags 0x0040

-- | Apply the bidirectional-text rules.  When right-to-left
-- scripts (Hebrew, Arabic, ...) appear in a domain name, this
-- check enforces a small set of constraints (per label and
-- across labels) designed to prevent visual confusion in
-- mixed-direction text.  Off by default.
pattern BIDICHECK :: IdnaFlags
pattern BIDICHECK = IdnaFlags 0x0080

-- | Presentation-time policy: when 'BIDICHECK' would reject a
-- domain at presentation time (because the cross-label
-- constraints fail), render it as ASCII anyway, with each
-- label in its @\"xn--\"@-prefixed form.  Useful for
-- browser-flavoured display where the goal is \"show the user
-- something readable\" and a Bidi-violating name should
-- degrade to its ACE spelling rather than producing an error.
-- Implies 'BIDICHECK' (lifted by 'effectiveIdnaFlags').
pattern ASCIIFALLBACK :: IdnaFlags
pattern ASCIIFALLBACK = IdnaFlags 0x0100

-- | Presentation-time relaxation: when rendering an
-- @\"xn--\"@-prefixed label, skip the full A-label round-trip
-- and emit whatever codepoints the Punycode body decodes to,
-- as long as the decode itself succeeds.  A label whose
-- Punycode body fails to decode still falls back to its ACE
-- form.
--
-- This is the most permissive presentation-time setting for
-- the /per-label/ rendering: it bypasses every IDN-content
-- check on each label, including the Punycode round-trip-
-- equality check (a strict A-label requires the re-encoding
-- of the decoded form to match the input bytes byte-for-byte).
-- For finer-grained relaxations --- admit emoji but keep
-- everything else strict, for instance --- pass the relevant
-- content-level flag to 'domainToUnicodeOpt' instead;
-- @EMOJIOK@ at render time behaves the same way it does at
-- parse time.
--
-- Orthogonal to 'BIDICHECK' and 'ASCIIFALLBACK': those flags
-- gate the /cross-label/ check, which still fires whenever
-- 'BIDICHECK' is set (and uses 'LAXDECODE'-rendered output
-- when computing Bidi summaries, for consistency between the
-- visible output and the check it's being validated against).
--
-- /Warning/: like 'Text.IDNA2008.Internal.Parse.labelToUnicodeLax',
-- this is a debugging knob.  Output is not guaranteed to round-
-- trip through 'Text.IDNA2008.Internal.Parse.parseDomain', and a
-- malformed wire form (including an OCTET label whose bytes
-- happen to start with @\"xn--\"@, which can arrive from the
-- network) will be Punycode-decoded here even though the parser
-- correctly classified it as OCTET.  ASCII codepoints surfaced
-- by the lax decode that are syntactically significant in zone
-- files get the usual @\\C@ or @\\DDD@ escape treatment, so the
-- output is at least syntactically safe to embed, but trusted
-- presentation should use the strict path.
pattern LAXDECODE :: IdnaFlags
pattern LAXDECODE = IdnaFlags 0x0200

----------------------------------------------------------------------
-- Defaults and implications
----------------------------------------------------------------------

-- | Default flag set: 'ALABELCHECK', 'NFCCHECK', 'BIDICHECK'.
-- The spec-conformant choices: classify @\"xn--\"@ labels by
-- strict round-trip, require NFC, and enforce Bidi rules.
-- Callers wanting looser semantics pass 'mempty' or a custom
-- composition.
defaultIdnaFlags :: IdnaFlags
defaultIdnaFlags = ALABELCHECK <> NFCCHECK <> BIDICHECK

-- | Lift implied bits in an 'IdnaFlags' set:
--
--   * 'MAPWIDTH' implies 'MAPDOTS', because the wide\/narrow
--     decompositions of label separators (e.g. @U+FF0E@,
--     @U+FF61@) need to be recognised at split time, not after.
--   * 'ASCIIFALLBACK' implies 'BIDICHECK', because the fallback
--     policy only kicks in when a check has flagged a violation.
effectiveIdnaFlags :: IdnaFlags -> IdnaFlags
effectiveIdnaFlags !flags =
    let !f1 = if MAPWIDTH      `meetsIdnaFlags` flags
                then flags <> MAPDOTS  else flags
        !f2 = if ASCIIFALLBACK `meetsIdnaFlags` f1
                then f1    <> BIDICHECK else f1
    in f2
{-# INLINE effectiveIdnaFlags #-}

-- | Does the first 'IdnaFlags' value satisfy the requirement
-- expressed by the second?  Reads as
-- @flag \`meetsIdnaFlags\` flags@.
meetsIdnaFlags :: IdnaFlags -> IdnaFlags -> Bool
IdnaFlags a `meetsIdnaFlags` IdnaFlags b = a == a .&. b
{-# INLINE meetsIdnaFlags #-}

-- | @set \`withoutIdnaFlags\` flag@ removes every bit of @flag@
-- from @set@.  Symmetric companion to @('<>')@.
withoutIdnaFlags :: IdnaFlags -> IdnaFlags -> IdnaFlags
IdnaFlags a `withoutIdnaFlags` IdnaFlags b =
    IdnaFlags (a .&. complement b)
{-# INLINE withoutIdnaFlags #-}

----------------------------------------------------------------------
-- CLI token vocabulary
----------------------------------------------------------------------

-- | Single-bit token table for command-line vocabulary.
idnaFlagsTokens :: [TokenEntry IdnaFlags]
idnaFlagsTokens =
    [ TokenEntry ALABELCHECK   "alabel-check"   ["xncheck"]
    , TokenEntry NFCCHECK      "nfc-check"      []
    , TokenEntry EMOJIOK       "emoji-ok"       []
    , TokenEntry MAPDOTS       "map-dots"       ["dmap"]
    , TokenEntry MAPNFC        "map-nfc"        ["nmap"]
    , TokenEntry MAPCASE       "map-case"       ["cmap"]
    , TokenEntry MAPWIDTH      "map-width"      ["wmap"]
    , TokenEntry BIDICHECK     "bidi-check"     []
    , TokenEntry ASCIIFALLBACK "ascii-fallback" []
    , TokenEntry LAXDECODE     "lax-decode"     []
    ]

-- | Preset names that expand to multi-bit flag sets.  Useful as
-- shorthands on the command line; never appear in 'present'
-- output.  The @default@ token is /not/ in this list -- it's
-- supplied by the caller of 'parseIdnaFlags'.
idnaFlagsPresets :: [Preset IdnaFlags]
idnaFlagsPresets =
    [ ("map", MAPDOTS <> MAPCASE <> MAPWIDTH <> MAPNFC)
    ]

-- | Parse a comma-separated CLI value into an 'IdnaFlags' set.
-- The first argument is the application's notion of @default@:
-- the value the @default@ token (and its 3-character prefix
-- @def@) resolves to.
--
-- Each comma-separated token may optionally be prefixed with
-- @\'+\'@ (additive, the default) or @\'-\'@ (subtractive).
-- If the first token has a sign prefix, the running result is
-- seeded with the supplied default; otherwise the running
-- result starts empty and the tokens replace the default
-- cleanly.
--
-- Tokens are resolved against 'idnaFlagsTokens' and
-- 'idnaFlagsPresets' (plus the caller-supplied @default@),
-- case-insensitively, with unambiguous prefix matching
-- (3-character minimum) as a fallback.
parseIdnaFlags :: IdnaFlags -> ByteString -> Either String IdnaFlags
parseIdnaFlags !defF =
    parseTokens idnaFlagsTokens
                (("default", defF) : idnaFlagsPresets)
                defF
                withoutIdnaFlags

-- | 'String'-flavoured wrapper for 'parseIdnaFlags' suitable
-- for @optparse-applicative@'s 'eitherReader': validates that
-- the input is pure ASCII before delegating.
parseIdnaFlagsStr :: IdnaFlags -> String -> Either String IdnaFlags
parseIdnaFlagsStr !defF = asciiByteString >=> parseIdnaFlags defF
