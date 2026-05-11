-- |
-- Module      : Text.IDNA2008.Internal.UTS46
-- Description : Hand-curated allow-list of UTS #46 mappings adopted
--               by the @MAPUTS46@ option, beyond strict IDNA2008.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- A single lookup, 'uts46Lookup', returning the multi-codepoint
-- target of each source codepoint in the @MAPUTS46@ allow-list, or
-- 'Nothing' for codepoints outside the allow-list.  Used by the
-- pre-validation MAPUTS46 pass in "Text.IDNA2008.Internal.Parse"
-- when the @MAPUTS46@ option is set.  The full list of in\\/out
-- decisions and rationale lives in the code-comment block in the
-- source of this module, immediately above 'uts46Lookup'.
{-# LANGUAGE LambdaCase #-}

module Text.IDNA2008.Internal.UTS46
    ( uts46Lookup
    ) where

-- ---------------------------------------------------------------------------
-- Rationale -- scope of the MAPUTS46 allow-list
-- ---------------------------------------------------------------------------
--
-- Strict IDNA2008 has no notion of compatibility folding beyond the four
-- RFC 5895 input mappings (MAPDOTS, MAPCASE, MAPNFC, MAPWIDTH).  UTS #46
-- offers a much larger table of mappings (~6400 entries in Unicode
-- 17.0.0).  We adopt only the small subset that passes a joint test:
--
--    1. SOURCE.  An IME or normal text-entry path might inadvertently
--       produce this codepoint while a user attempts to type a domain-
--       name component.  Codepoints requiring a Unicode picker, math
--       editor, or typesetting tool are NOT in scope.
--
--    2. TARGET.  The mapping result is plausible content for a real
--       domain label — a name, word, or letter sequence in a script
--       that registrars actually accept — rather than an editorial
--       expansion of a symbol.
--
-- Both halves must pass.  Failure of either disqualifies the entry.
--
-- IN -- Japanese era / corporation IME shortcuts (6 entries)
-- ----------------------------------------------------------
-- Modern Japanese IMEs (Mozc, Microsoft IME, Google IME) propose these
-- single-codepoint squared forms as one-keystroke alternatives to typing
-- out the spelling.  The expanded targets are exactly what an attentive
-- user would have chosen.
--
--    U+32FF (SQUARE ERA NAME REIWA)   ->  U+4EE4 U+548C        (Reiwa)
--    U+337B (SQUARE ERA NAME HEISEI)  ->  U+5E73 U+6210        (Heisei)
--    U+337C (SQUARE ERA NAME SYOUWA)  ->  U+662D U+548C        (Showa)
--    U+337D (SQUARE ERA NAME TAISYOU) ->  U+5927 U+6B63        (Taisho)
--    U+337E (SQUARE ERA NAME MEIZI)   ->  U+660E U+6CBB        (Meiji)
--
-- IN -- Circled CJK ideographs at U+3244..U+3247 and U+3280..U+32B0
--       EXCLUDING U+3297 and U+3299 (51 entries)
-- ----------------------------------------------------------------
-- Each source codepoint maps to a single CJK ideograph (e.g. U+3280
-- CIRCLED IDEOGRAPH ONE -> U+4E00, U+329F CIRCLED IDEOGRAPH ATTENTION
-- -> U+6CE8).  These are pre-emoji codepoints from the CJK
-- Compatibility block that some Japanese/Chinese IMEs propose as a
-- stylistic option when marking lists or notes; the target ideograph
-- is everyday content.  Two codepoints in this range (U+3297 and
-- U+3299) are deliberately omitted on the same grounds as the
-- squared CJK ideograph emoji; see the OUT list below.
--
-- OUT -- Squared CJK ideograph emoji (U+1F210..U+1F23B, U+1F250..U+1F251)
-- ----------------------------------------------------------------------
-- The UTS #46 mapping table folds these to single CJK ideographs
-- (e.g. U+1F238 SQUARED CJK UNIFIED IDEOGRAPH-7533 maps to U+7533).
-- We do NOT adopt these mappings.
--
-- Empirical (May 2026, dataset of 6300 registered emoji-bearing IDNs):
-- the .ws registry has registered xn--q97h.ws (U+1F238 as a label
-- under .ws) as a parking-lot domain.  Safari, applying UTS #46
-- mapping, silently routes lookups of that label to xn--uny.ws
-- (U+7533 as a label under .ws), which is a SEPARATELY registered
-- domain belonging to a different operator (Quarken URL Shortener).
-- The two namespaces are distinct, the user's intended destination
-- and actual destination differ, and the user has no signal that the
-- rewrite occurred.  Future re-registrations could turn the kanji-
-- form target into a phishing site without the emoji-form
-- registrant's knowledge or consent.
--
-- libidn2 in non-transitional mode rejects these codepoints outright
-- ("character forbidden in non-transitional mode"); the mainstream
-- tooling landscape is split, with browsers folding and BIND/dig
-- refusing.  Our refusing-to-fold position is conservative but
-- defensible.  See the EMOJIOK pattern in
-- "Text.IDNA2008.Internal.Flags" for the parallel decision on the
-- admit-as-is side: these codepoints are excluded from EMOJIOK too,
-- so under no combination of flags will this library route a
-- U+1F238 label to a U+7533 label.
--
-- OUT -- U+3297 CIRCLED IDEOGRAPH CONGRATULATION and U+3299 CIRCLED
--        IDEOGRAPH SECRET
-- -------------------------------------------------------------
-- Although these two codepoints sit in the U+3280..U+32B0 BMP block
-- whose other members are in the IN list, they are also @Emoji=Yes@
-- and folded by UTS #46 -- the same cross-tool ambiguity shape as
-- the squared CJK ideograph emoji.  Their UTS #46 fold targets
-- (U+795D and U+79D8 respectively) could attract separate
-- registration by an opportunistic third party at any time;
-- empirically there is no live divergence today (the kanji-form
-- .ws labels resolve to NXDOMAIN), but folding by default would
-- bake in a future hijack vector with no signal to the user.
-- Refused symmetrically with the rest of the mappedEmojiRanges
-- set.
--
-- OUT -- Squared kana abbreviations (U+3300..U+3357, 88 entries)
-- -------------------------------------------------------------
-- Each source folds to a multi-character katakana loanword like
-- "calorie", "dollar", or "mansion" -- a complete Japanese word
-- rather than single-label content.  The source codepoints exist
-- primarily for vertical-typesetting space-economy in Japanese
-- newspapers, not IME output.
--
-- OUT -- Squared unit / math abbreviations (U+3382..U+33DF range)
-- ---------------------------------------------------------------
-- Each source folds to mixed Greek+Latin or unit-notation sequences
-- like "us", "mhz", "m/s", or "kohm", several of which contain
-- U+2215 (DIVISION SLASH, itself disallowed in IDNA2008).  Not
-- plausible label content.
--
-- OUT -- Circled Katakana (U+32D0..U+32FE, 50 entries)
-- ---------------------------------------------------
-- Each source folds to a single katakana.  Such labels appear in
-- domain registrations rarely enough that the IME-accident
-- probability for the circled form is lower than the false-positive
-- cost of folding.
--
-- OUT -- Circled Hangul (U+3260..U+327E, 35 entries)
-- --------------------------------------------------
-- Same rationale as circled Katakana; single-jamo / one-syllable
-- forms are not the kind of content registrars routinely accept as
-- whole labels.
--
-- OUT -- Arabic positional forms (U+FB50..U+FDFD and U+FE70..U+FEFC,
--        ~706 entries)
-- ----------------------------------------------------------------
-- Targets are natural Arabic base letters, but the sources are
-- presentation forms not produced by any modern Arabic IME.  They
-- appear in user input only from copy-paste out of legacy PDFs that
-- baked shaping into the encoding.  IDNA2008 explicitly disallows the
-- source codepoints; we honour that disposition.
--
-- OUT -- Latin compatibility ligatures (U+FB00..U+FB06, 7 entries)
-- ----------------------------------------------------------------
-- Sources fold to ASCII letter sequences (fi, fl, ffi, ffl, ff, st).
-- Copy-paste from typeset documents can produce these, but the
-- population producing such documents is not shopping for domain
-- names, and the targets are ASCII (we reject non-ASCII -> ASCII
-- mappings on principle).
--
-- OUT -- Latin digraphs (U+0132 U+01C4 U+01C7 U+01CA U+01F1 and their
--        title/lower variants, 11 entries)
-- -----------------------------------------------------------------
-- Even rarer than the ligatures; no auto-conversion path puts them
-- in user input.
--
-- OUT -- Math alphanumeric / font-variant Latin and Greek (~439
--        entries)
-- ------------------------------------------------------------
-- Sources are math-italic, math-bold, fraktur, script, and related
-- typesetting alphabets (e.g. U+210F PLANCK CONSTANT OVER TWO PI ->
-- U+0127, U+1D400 MATHEMATICAL BOLD CAPITAL A -> "a").  Math
-- typesetting only; never an IME or text-entry accident.
--
-- OUT -- Roman numerals, vulgar fractions, symbol expansions,
--        superscripts, subscripts, degree-Celsius/Fahrenheit --
--        everything else in the UTS #46 mapping table.
-- ----------------------------------------------------------
-- Examples: U+2122 TRADE MARK SIGN -> "tm", U+2116 NUMERO SIGN ->
-- "no", U+2121 TELEPHONE SIGN -> "tel", U+2175 SMALL ROMAN NUMERAL
-- SIX -> "vi", U+00BC VULGAR FRACTION ONE QUARTER -> "1" U+2044 "4",
-- U+00B2 SUPERSCRIPT TWO -> "2", U+2103 DEGREE CELSIUS -> U+00B0 "C".
-- Either the target isn't plausible domain content (fraction slash,
-- degree sign), or the source requires deliberate insertion via a
-- Unicode picker.
--
-- ---------------------------------------------------------------------------
-- Revision discipline
-- ---------------------------------------------------------------------------
-- Any change to the IN\\/OUT classifications above must carry, in the
-- same commit:
--
--   1. A clear rationale, written as an extension or amendment of the
--      relevant section of the comment block above — so the standing
--      record of why each entry is where it is stays current and
--      self-contained.
--
--   2. Test coverage in one or more of the test programs (the unit
--      tests under tests/, the conformance suite, or the idnaparse \\/
--      idnabench harnesses) exercising the new or revised behaviour.
-- ---------------------------------------------------------------------------

-- | Look up @cp@ in the @MAPUTS46@ allow-list.  Returns the list of
-- target codepoints to substitute (always non-empty), or 'Nothing'
-- if @cp@ is not in the allow-list.
--
-- The @'Int' -> 'Maybe' ['Int']@ shape mirrors the way UTS #46
-- mappings work: a single source codepoint may expand to one or
-- several target codepoints (era-name codepoints expand 1:2, the
-- circled CJK ideographs expand 1:1).
uts46Lookup :: Int -> Maybe [Int]
uts46Lookup = \case
    -- Japanese era
    0x32FF -> Just [0x4EE4, 0x548C]              -- Reiwa
    0x337B -> Just [0x5E73, 0x6210]              -- Heisei
    0x337C -> Just [0x662D, 0x548C]              -- Showa
    0x337D -> Just [0x5927, 0x6B63]              -- Taisho
    0x337E -> Just [0x660E, 0x6CBB]              -- Meiji

    -- Circled CJK ideographs in U+3244..U+3247
    0x3244 -> Just [0x554F]                       -- question
    0x3245 -> Just [0x5E7C]                       -- kindergarten
    0x3246 -> Just [0x6587]                       -- school
    0x3247 -> Just [0x7B8F]                       -- koto

    -- Circled CJK ideographs in U+3280..U+32B0
    0x3280 -> Just [0x4E00]                       -- one
    0x3281 -> Just [0x4E8C]                       -- two
    0x3282 -> Just [0x4E09]                       -- three
    0x3283 -> Just [0x56DB]                       -- four
    0x3284 -> Just [0x4E94]                       -- five
    0x3285 -> Just [0x516D]                       -- six
    0x3286 -> Just [0x4E03]                       -- seven
    0x3287 -> Just [0x516B]                       -- eight
    0x3288 -> Just [0x4E5D]                       -- nine
    0x3289 -> Just [0x5341]                       -- ten
    0x328A -> Just [0x6708]                       -- moon
    0x328B -> Just [0x706B]                       -- fire
    0x328C -> Just [0x6C34]                       -- water
    0x328D -> Just [0x6728]                       -- wood
    0x328E -> Just [0x91D1]                       -- metal
    0x328F -> Just [0x571F]                       -- earth
    0x3290 -> Just [0x65E5]                       -- sun
    0x3291 -> Just [0x682A]                       -- stock
    0x3292 -> Just [0x6709]                       -- have
    0x3293 -> Just [0x793E]                       -- society
    0x3294 -> Just [0x540D]                       -- name
    0x3295 -> Just [0x7279]                       -- special
    0x3296 -> Just [0x8CA1]                       -- financial
    -- U+3297, U+3299 deliberately excluded (also mappedEmoji);
    -- see the OUT-list rationale block above.
    0x3298 -> Just [0x52B4]                       -- labor
    0x329A -> Just [0x7537]                       -- male
    0x329B -> Just [0x5973]                       -- female
    0x329C -> Just [0x9069]                       -- suitable
    0x329D -> Just [0x512A]                       -- excellent
    0x329E -> Just [0x5370]                       -- print
    0x329F -> Just [0x6CE8]                       -- attention
    0x32A0 -> Just [0x9805]                       -- item
    0x32A1 -> Just [0x4F11]                       -- rest
    0x32A2 -> Just [0x5199]                       -- copy
    0x32A3 -> Just [0x6B63]                       -- correct
    0x32A4 -> Just [0x4E0A]                       -- high
    0x32A5 -> Just [0x4E2D]                       -- centre
    0x32A6 -> Just [0x4E0B]                       -- low
    0x32A7 -> Just [0x5DE6]                       -- left
    0x32A8 -> Just [0x53F3]                       -- right
    0x32A9 -> Just [0x533B]                       -- medicine
    0x32AA -> Just [0x5B97]                       -- religion
    0x32AB -> Just [0x5B66]                       -- study
    0x32AC -> Just [0x76E3]                       -- supervise
    0x32AD -> Just [0x4F01]                       -- enterprise
    0x32AE -> Just [0x8CC7]                       -- resource
    0x32AF -> Just [0x5354]                       -- alliance
    0x32B0 -> Just [0x591C]                       -- night

    _      -> Nothing
{-# INLINE uts46Lookup #-}
