-- |
-- Module      : Text.IDNA2008.Internal.LabelForm
-- Description : Per-label classification singleton.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- The 'LabelForm' singleton type names the eight possible
-- classifications the parser can assign to a single label, plus
-- a ninth sentinel 'NoLabel' returned by
-- 'Text.IDNA2008.Internal.LabelInfo.getLabelForm' for indices
-- outside the parsed domain's label range (including all
-- queries against the bare-root 'LabelInfo').  The parser
-- never produces 'NoLabel' for a real label.
--
-- The companion type 'Text.IDNA2008.Internal.LabelFormSet'
-- represents /sets/ of label forms (as bitmasks).  The two are
-- deliberately distinct: a 'LabelForm' is a single
-- classification, a 'LabelFormSet' is a permitted-set, and
-- the difference is enforced at the type level.
{-# LANGUAGE PatternSynonyms #-}

module Text.IDNA2008.Internal.LabelForm
    ( LabelForm( LDH
               , RLDH
               , FAKEA
               , ALABEL
               , ULABEL
               , ATTRLEAF
               , OCTET
               , WILDLABEL
               , NoLabel
               )
    , unLabelForm
    , wordLabelForm
    ) where

import Data.Word (Word8)

-- | A single per-label classification.  Stored as a small
-- 'Word8' tag: real labels use values @0..7@; the sentinel
-- 'NoLabel' uses value @8@.  The data constructor is not
-- exported; pattern singletons are the only legal way to
-- inspect or construct a value.
newtype LabelForm = LabelForm Word8
    deriving (Eq, Ord)

-- | Letter-digit-hyphen, the conventional hostname alphabet.
-- Lowercase ASCII letters, digits, and the hyphen, with no
-- leading or trailing hyphen and no double-hyphen at positions
-- 3 and 4 (which would make it a Reserved-LDH or A-label).
pattern LDH :: LabelForm
pattern LDH = LabelForm 0

-- | Reserved LDH: an LDH-shaped label with @--@ at positions 3
-- and 4 whose first two characters are not (case-folded)
-- @\"xn\"@.  In practice these are pre-IDN registrations like
-- @\"l---l\"@, @\"cd--storage-shelves\"@.
pattern RLDH :: LabelForm
pattern RLDH = LabelForm 1

-- | An @\"xn--\"@-prefixed LDH label that does /not/ strictly
-- round-trip: its Punycode body decodes to something invalid
-- as an IDN label, or the re-encoding doesn't match the input.
pattern FAKEA :: LabelForm
pattern FAKEA = LabelForm 2

-- | An @\"xn--\"@-prefixed LDH label that strictly round-trips:
-- the Punycode body decodes cleanly, the decoded codepoints are
-- valid in an IDN, and re-encoding produces the same input bytes.
pattern ALABEL :: LabelForm
pattern ALABEL = LabelForm 3

-- | A label whose source contained at least one non-ASCII
-- character.  The library's parser accepts non-ASCII input,
-- validates each codepoint, and encodes the label to its
-- @\"xn--\"@-prefixed form for the wire.
pattern ULABEL :: LabelForm
pattern ULABEL = LabelForm 4

-- | An LDH-shaped label whose first character is @\'_\'@.  Used
-- by service-discovery records and by attribute-leaf naming
-- conventions: @\"_25._tcp\"@, @\"_dmarc\"@,
-- @\"_acme-challenge\"@, etc.
pattern ATTRLEAF :: LabelForm
pattern ATTRLEAF = LabelForm 5

-- | A label whose bytes are not all in the LDH alphabet.
-- Includes labels admitted via @\\DDD@ or @\\C@ escapes
-- regardless of UTF-8 validity, labels with non-LDH ASCII
-- (e.g. @\'$\'@, @\'+\'@), and labels containing non-ASCII
-- codepoints that aren't valid for IDN classification.
pattern OCTET :: LabelForm
pattern OCTET = LabelForm 6

-- | The single-byte wildcard label @\'*\'@.  Distinct from
-- 'OCTET' so callers that want to admit @\"*.example\"@
-- queries can do so without admitting arbitrary non-LDH bytes.
pattern WILDLABEL :: LabelForm
pattern WILDLABEL = LabelForm 7

-- | Sentinel value returned by
-- 'Text.IDNA2008.Internal.LabelInfo.getLabelForm' when the
-- index is outside the parsed domain's label range, or when
-- the 'LabelInfo' is the bare root (zero labels).  The parser
-- never produces 'NoLabel' for an actual label, and it is not
-- a member of any 'Text.IDNA2008.Internal.LabelFormSet'.
pattern NoLabel :: LabelForm
pattern NoLabel = LabelForm 8

{-# COMPLETE LDH, RLDH, FAKEA, ALABEL, ULABEL, ATTRLEAF, OCTET,
             WILDLABEL, NoLabel #-}

-- | Extract the underlying tag.  Internal use only; callers
-- should pattern-match against the singletons rather than
-- compare integers.
unLabelForm :: LabelForm -> Word8
unLabelForm (LabelForm w) = w
{-# INLINE unLabelForm #-}

-- | Reconstruct a 'LabelForm' from a tag in @0..8@.  Internal
-- use only; the caller is expected to have produced @w@ from
-- 'unLabelForm' or from a verified storage representation.
-- Values out of range are wrapped without checking.
wordLabelForm :: Word8 -> LabelForm
wordLabelForm = LabelForm
{-# INLINE wordLabelForm #-}

instance Show LabelForm where
    show LDH       = "LDH"
    show RLDH      = "RLDH"
    show FAKEA     = "FAKEA"
    show ALABEL    = "ALABEL"
    show ULABEL    = "ULABEL"
    show ATTRLEAF  = "ATTRLEAF"
    show OCTET     = "OCTET"
    show WILDLABEL = "WILDLABEL"
    show NoLabel   = "NoLabel"
