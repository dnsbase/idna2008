-- |
-- Module      : Text.IDNA2008.Internal.Error
-- Description : Error type returned by the parser.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Errors raised by the IDNA-aware domain-name parser.  Failures
-- are reported with enough context (label index, byte offset
-- where useful) for callers to pinpoint the offending part of
-- the input without the library having to retain or echo back
-- the raw input bytes.
module Text.IDNA2008.Internal.Error
    ( BidiRuleViolation(..)
    ) where

-- | Which bidirectional-text rule a label violates, with enough
-- granularity for a caller to format a useful diagnostic
-- without having to consult the rule text.  Used as the
-- payload of 'LabelBidi' (per-label check) and
-- 'ErrCrossLabelBidi' (cross-label check).
--
-- The rules apply when a label is \"bidirectional-flavoured\":
-- it contains a right-to-left letter (Hebrew, Arabic, ...) or
-- an Arabic-Indic digit.
data BidiRuleViolation
    = BidiRule1FirstNotLRAL
      -- ^ The first codepoint of a bidirectional-flavoured label
      -- was not a letter from a known left-to-right or
      -- right-to-left script.  Most often catches digit-, mark-,
      -- or punctuation-leading labels in a name that also has a
      -- right-to-left component (e.g. @\"_tcp\"@, @\"123\"@).
    | BidiRule2RTLDisallowed
      -- ^ A right-to-left label (one whose first codepoint is a
      -- right-to-left letter) contained a codepoint that is not
      -- allowed in such a label -- typically a left-to-right
      -- letter, or an embedding\/override\/isolate format
      -- character.
    | BidiRule3RTLBadEnd
      -- ^ A right-to-left label's last non-mark codepoint was
      -- not a letter or digit.
    | BidiRule4ENANMix
      -- ^ A right-to-left label mixed European-style digits
      -- (ASCII or extended Arabic-Indic) with Arabic-Indic
      -- digits.  The two digit families cannot coexist in the
      -- same label.
    | BidiRule5LTRDisallowed
      -- ^ A left-to-right label (one whose first codepoint is a
      -- left-to-right letter) contained a codepoint that is not
      -- allowed in such a label -- typically a right-to-left
      -- letter or an Arabic-Indic digit.
    | BidiRule6LTRBadEnd
      -- ^ A left-to-right label's last non-mark codepoint was
      -- not a letter or a European-style digit.
    deriving (Eq, Show)
