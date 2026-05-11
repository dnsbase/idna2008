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
               , MAPUTS46
               )
    , allIdnaMappings
    , defaultIdnaFlags
    , effectiveIdnaFlags
    , meetsIdnaFlags
    , withoutIdnaFlags

      -- * Command-line option syntax
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
    deriving Eq

instance Semigroup IdnaFlags where
    IdnaFlags a <> IdnaFlags b = IdnaFlags (a .|. b)
    {-# INLINE (<>) #-}

instance Monoid IdnaFlags where
    mempty = IdnaFlags 0
    {-# INLINE mempty #-}

instance Show IdnaFlags where
    show o = case flagNames o of
        []  -> "mempty"
        ns  -> intercalate "," ns

flagNames :: IdnaFlags -> [String]
flagNames (IdnaFlags w) = go names w
  where
    names = [ "alabel-check", "nfc-check", "emoji-ok",
              "map-dots", "map-nfc", "map-case", "map-width",
              "bidi-check", "ascii-fallback",
              "map-uts46" ]
    go _ 0  = []
    go [] _ = []
    go (s:ss) n
        | odd n     = s : go ss (n `unsafeShiftR` 1)
        | otherwise = go ss (n `unsafeShiftR` 1)

----------------------------------------------------------------------
-- Pattern singletons
----------------------------------------------------------------------

-- | Strict A-label check.  An ACE-prefixed label whose
-- Punycode body doesn't decode to a valid IDN label, or whose
-- re-encoding doesn't match the input bytes, classifies as
-- 'Text.IDNA2008.Internal.LabelForm.FAKEA' rather than
-- 'Text.IDNA2008.Internal.LabelForm.ALABEL'.  Without this
-- flag, every well-formed ACE-prefixed LDH label is reported as
-- 'Text.IDNA2008.Internal.LabelForm.ALABEL'.  On in
-- 'defaultIdnaFlags'.
pattern ALABELCHECK :: IdnaFlags
pattern ALABELCHECK = IdnaFlags 0x0001

-- | Require Unicode Normalization Form C on labels with non-ASCII
-- content.  Labels with combining marks in non-canonical order,
-- or decomposed sequences with a precomposed equivalent, are
-- rejected.  Without this flag no normalization check is
-- performed, and the same display string can map to multiple
-- distinct A-labels on the wire.  On in 'defaultIdnaFlags'.
pattern NFCCHECK :: IdnaFlags
pattern NFCCHECK = IdnaFlags 0x0002

-- | Diagnostic relaxation: codepoints that carry the Unicode
-- @Emoji=Yes@ property are admitted even if they're otherwise
-- rejected by the IDN allowed-codepoint rules.  Useful when
-- analysing real-world IDN data that includes emoji
-- registrations.  Off by default.
--
-- /Exclusion/: emoji codepoints whose UTS #46 status is also
-- @mapped@ are /not/ admitted by this flag.  These codepoints
-- resolve ambiguously across the ecosystem: browsers following
-- UTS #46 lookup processing apply the fold and reach the mapped
-- target, while admit-as-is tooling reaches the unmapped form.
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
-- katakana, etc.  Implies 'MAPDOTS', because two of the
-- fullwidth\/halfwidth forms are label-separator characters that
-- must be recognised before per-label processing.
pattern MAPWIDTH :: IdnaFlags
pattern MAPWIDTH = IdnaFlags 0x0040

-- | Apply the bidirectional-text rules.  When right-to-left
-- scripts (Hebrew, Arabic, ...) appear in a domain name, this
-- check enforces a small set of constraints (per label and
-- across labels) designed to prevent visual confusion in
-- mixed-direction text.  On in 'defaultIdnaFlags'.
pattern BIDICHECK :: IdnaFlags
pattern BIDICHECK = IdnaFlags 0x0080

-- | Presentation-time policy: when 'BIDICHECK' would reject a
-- domain at presentation time (because the cross-label
-- constraints fail), render it as ASCII anyway, with each label
-- in its ACE-prefixed form.  Useful for displaying domain
-- names as part of running text where the goal is \"show the user
-- something readable\" and a Bidi-violating name should degrade
-- to its ACE-prefixed form rather than produce an error.
-- Implies 'BIDICHECK'.
pattern ASCIIFALLBACK :: IdnaFlags
pattern ASCIIFALLBACK = IdnaFlags 0x0100

-- | Apply a small hand-curated subset of UTS #46 character mappings
-- before validation, beyond the four RFC 5895 input mappings
-- (@MAPDOTS@, @MAPCASE@, @MAPNFC@, @MAPWIDTH@).  The adopted set is
-- the in\\/out classification documented at the top of
-- "Text.IDNA2008.Internal.UTS46"; in summary it covers the
-- Japanese era IME shortcuts (five codepoints at @U+32FF@ and
-- @U+337B..U+337E@) and the circled-CJK ideographs at
-- @U+3244..U+3247@ and @U+3280..U+32B0@ (with @U+3297@ and
-- @U+3299@ carved out) that fold to a single base ideograph each.
-- Every other UTS #46 mapping is rejected on principle; see the
-- rationale block for the OUT list.
--
-- /Implies/ 'allIdnaMappings': UTS #46 preprocessing subsumes the
-- RFC 5895 input mappings, so requesting @MAPUTS46@ alone enables
-- 'MAPNFC', 'MAPDOTS', 'MAPCASE', and 'MAPWIDTH' as well.
--
-- Beyond strict IDNA2008.  /Not/ included in 'allIdnaMappings';
-- callers must request it explicitly.
pattern MAPUTS46 :: IdnaFlags
pattern MAPUTS46 = IdnaFlags 0x0200

----------------------------------------------------------------------
-- Defaults and implications
----------------------------------------------------------------------

-- | Default flag set: 'ALABELCHECK', 'NFCCHECK', 'BIDICHECK'.
-- The spec-conformant choices: check whether ACE-prefixed
-- LDH-labels are actually A-labels, or just /fake/ A-labels,
-- require NFC form in U-labels, and enforce BIDI rules.  Callers
-- wanting looser semantics pass 'mempty' or a custom subset.
defaultIdnaFlags :: IdnaFlags
defaultIdnaFlags = ALABELCHECK <> NFCCHECK <> BIDICHECK

-- | All four optional RFC 5895 mappings.  To be used only after
-- reading the caveats in the
-- [Introduction](https://www.rfc-editor.org/rfc/rfc5895.html#section-1)
--
-- This expands to the union (@'mconcat'@) of 'MAPNFC', 'MAPDOTS',
-- 'MAPCASE', and 'MAPWIDTH'.  These mappings are outside the core
-- IDNA2008 specification but are part of RFC 5895; they are not
-- applied by default.
--
-- Deliberately /excludes/ 'MAPUTS46', which is a separate
-- beyond-IDNA2008 opt-in covering a hand-curated subset of UTS #46
-- compatibility mappings, on the same footing as 'EMOJIOK' rather
-- than the RFC 5895 set.
allIdnaMappings :: IdnaFlags
allIdnaMappings = MAPNFC <> MAPDOTS <> MAPCASE <> MAPWIDTH

-- | Lift implied bits in an 'IdnaFlags' set:
--
--   * 'MAPUTS46' implies 'allIdnaMappings' ('MAPNFC', 'MAPDOTS',
--     'MAPCASE', 'MAPWIDTH').  UTS #46-style preprocessing
--     subsumes the four RFC 5895 input mappings — a user
--     opting into the UTS #46 carve-out implicitly wants
--     case-folded, width-folded, NFC-normalised input with
--     East-Asian dot recognition; running MAPUTS46 without
--     those would leave the input partially pre-processed and
--     produce inconsistent fold outcomes.
--   * 'MAPWIDTH' implies 'MAPDOTS', because the wide\/narrow
--     decompositions of label separators (e.g. @U+FF0E@,
--     @U+FF61@) need to be recognised at split time, not after.
--   * 'ASCIIFALLBACK' implies 'BIDICHECK', because the fallback
--     policy only kicks in when a check has flagged a violation.
effectiveIdnaFlags :: IdnaFlags -> IdnaFlags
effectiveIdnaFlags !flags =
    let !f0 = if MAPUTS46      `meetsIdnaFlags` flags
                then flags <> allIdnaMappings else flags
        !f1 = if MAPWIDTH      `meetsIdnaFlags` f0
                then f0    <> MAPDOTS    else f0
        !f2 = if ASCIIFALLBACK `meetsIdnaFlags` f1
                then f1    <> BIDICHECK  else f1
    in f2
{-# INLINE effectiveIdnaFlags #-}

-- | Does the first 'IdnaFlags' value satisfy the requirement
-- expressed by the second?  Reads as
-- @flag \`meetsIdnaFlags\` flags@.
meetsIdnaFlags :: IdnaFlags -> IdnaFlags -> Bool
IdnaFlags a `meetsIdnaFlags` IdnaFlags b = a == a .&. b
{-# INLINE meetsIdnaFlags #-}

-- | @set \`withoutIdnaFlags\` flag@ removes every bit of @flag@
-- from @set@.
withoutIdnaFlags :: IdnaFlags -> IdnaFlags -> IdnaFlags
IdnaFlags a `withoutIdnaFlags` IdnaFlags b =
    IdnaFlags (a .&. complement b)
{-# INLINE withoutIdnaFlags #-}

----------------------------------------------------------------------
-- Command-line option syntax
----------------------------------------------------------------------

-- | Single-bit token table for the command-line option syntax.
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
    , TokenEntry MAPUTS46      "map-uts46"      []
    ]

-- | Preset names that expand to multi-bit flag sets.  Useful as
-- command line option shorthands.
idnaFlagsPresets :: [Preset IdnaFlags]
idnaFlagsPresets =
    [ ("map", allIdnaMappings)
    , ("umap", allIdnaMappings <> MAPUTS46)
    ]

-- | Parse a comma-separated CLI value into an 'IdnaFlags' set.
-- The first argument is the application-specific default value.
--
-- Each comma-separated token may optionally be prefixed with
-- @\'+\'@ (additive, the default) or @\'-\'@ (subtractive).
-- If the first token has a sign prefix, the running result is
-- seeded with the supplied default; otherwise the running
-- result starts empty and the tokens replace the default
-- cleanly.
--
-- Tokens are matched case-insensitively, with unambiguous prefix
-- matching (3-character minimum) as a fallback when there's no
-- exact match.  The token @default@ or an unambiguous prefix
-- matches the supplied default value.
parseIdnaFlags :: IdnaFlags   -- ^ Application-specific default
               -> ByteString  -- ^ Tokens to parse
               -> Either String IdnaFlags
parseIdnaFlags !defF =
    parseTokens idnaFlagsTokens
                (("default", defF) : idnaFlagsPresets)
                defF
                withoutIdnaFlags

-- | 'String'-based wrapper for 'parseIdnaFlags': validates that
-- the input is pure ASCII.
parseIdnaFlagsStr :: IdnaFlags -> String -> Either String IdnaFlags
parseIdnaFlagsStr !defF = asciiByteString >=> parseIdnaFlags defF
