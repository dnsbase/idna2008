-- |
-- Module      : Text.IDNA2008.Internal.LabelForm
-- Description : Per-label classification singleton.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- The 'LabelForm' singleton type names the nine possible
-- classifications the parser can assign to a single label, plus
-- a tenth sentinel 'NoLabel' returned by
-- 'Text.IDNA2008.Internal.LabelInfo.getLabelForm' for indices
-- outside the parsed domain's label count.  The parser never
-- produces 'NoLabel' for a real label.
--
-- The companion type 'Text.IDNA2008.Internal.LabelFormSet'
-- represents unordered /sets/ of label forms.
{-# LANGUAGE PatternSynonyms #-}

module Text.IDNA2008.Internal.LabelForm
    ( LabelForm(.., LDH, RLDH, FAKEA, ALABEL, ULABEL, ATTRLEAF, OCTET
               , WILDLABEL, LAXULABEL, NoLabel)
    ) where

import Data.Word (Word8)

-- | A single per-label classification.  Use the pattern
-- singletons to construct or pattern-match.
newtype LabelForm = LabelForm_ Word8
    deriving (Eq, Ord)

-- | Letter-digit-hyphen, the conventional hostname alphabet.
-- Lowercase ASCII letters, digits, and the hyphen, with no
-- leading or trailing hyphen and no double-hyphen at positions
-- 3 and 4 (which would make it a Reserved-LDH or A-label).
pattern LDH :: LabelForm
pattern LDH = LabelForm_ 0

-- | Reserved LDH: an LDH-shaped label with @--@ at positions 3
-- and 4 whose first two characters are not (case-folded)
-- @\"xn\"@.  In practice these are pre-IDN registrations like
-- @\"l---l\"@, @\"cd--storage-shelves\"@.
pattern RLDH :: LabelForm
pattern RLDH = LabelForm_ 1

-- | An ACE-prefixed LDH label that does /not/ strictly
-- round-trip: its Punycode body decodes to something invalid
-- as an IDN label, or the re-encoding doesn't match the input.
pattern FAKEA :: LabelForm
pattern FAKEA = LabelForm_ 2

-- | An ACE-prefixed LDH label that strictly round-trips:
-- the Punycode body decodes cleanly, the decoded codepoints are
-- valid in an IDN, and re-encoding produces the same input bytes.
pattern ALABEL :: LabelForm
pattern ALABEL = LabelForm_ 3

-- | A label whose source contained at least one non-ASCII
-- character.  The library's parser accepts non-ASCII input,
-- validates each codepoint, and encodes the label to its
-- ACE-prefixed form for the wire.
pattern ULABEL :: LabelForm
pattern ULABEL = LabelForm_ 4

-- | An LDH-shaped label whose first character is @\'_\'@.  Used
-- by service-discovery records and by attribute-leaf naming
-- conventions: @\"_25._tcp\"@, @\"_dmarc\"@,
-- @\"_acme-challenge\"@, etc.
pattern ATTRLEAF :: LabelForm
pattern ATTRLEAF = LabelForm_ 5

-- | A label whose bytes are not all in the LDH alphabet.
-- Includes labels admitted via @\\DDD@ or @\\C@ escapes
-- regardless of UTF-8 validity, labels with non-LDH ASCII
-- (e.g. @\'$\'@, @\'+\'@), and labels containing non-ASCII
-- codepoints that aren't valid for IDN classification.
pattern OCTET :: LabelForm
pattern OCTET = LabelForm_ 6

-- | The single-byte wildcard label @\'*\'@.  Distinct from
-- 'OCTET' so callers that want to admit @\"*.example\"@
-- queries can do so without admitting arbitrary non-LDH bytes.
pattern WILDLABEL :: LabelForm
pattern WILDLABEL = LabelForm_ 7

-- | Like 'ULABEL' but the non-ASCII codepoints fail strict
-- U-label validation.
pattern LAXULABEL :: LabelForm
pattern LAXULABEL = LabelForm_ 8

-- | Sentinel value returned by
-- 'Text.IDNA2008.Internal.LabelInfo.getLabelForm' when the
-- index is outside the parsed domain's label count.  The parser
-- never produces 'NoLabel' for an actual label, and it is not
-- a member of any 'Text.IDNA2008.Internal.LabelFormSet'.
pattern NoLabel :: LabelForm
pattern NoLabel = LabelForm_ 9

{-# COMPLETE LDH, RLDH, FAKEA, ALABEL, ULABEL, ATTRLEAF, OCTET,
             WILDLABEL, LAXULABEL, NoLabel #-}

instance Show LabelForm where
    show LDH       = "LDH"
    show RLDH      = "RLDH"
    show FAKEA     = "FAKEA"
    show ALABEL    = "ALABEL"
    show ULABEL    = "ULABEL"
    show ATTRLEAF  = "ATTRLEAF"
    show OCTET     = "OCTET"
    show WILDLABEL = "WILDLABEL"
    show LAXULABEL = "LAXULABEL"
    show NoLabel   = "NoLabel"
