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
    ( IdnaError(..)
    , LabelReason(..)
    , AceReason(..)
    , BidiRuleViolation(..)
    ) where

import Text.IDNA2008.Internal.LabelForm (LabelForm)

-- | Possible failure modes when parsing a presentation-form
-- domain name into wire form, including IDN encoding /
-- decoding errors.
data IdnaError
    = -- | An empty (non-final) label was encountered, e.g. @\"foo..bar\"@.
      ErrEmptyLabel
        !Int -- ^ Label index (0-based)
    | -- | A label exceeds 63 wire octets.  The 'Int' is the actual length.
      ErrLabelTooLong
        !Int -- ^ Label index
        !Int -- ^ Actual length
    | -- | The domain's total wire length would exceed 255 bytes.
      ErrNameTooLong
        !Int -- ^ Actual length.
    | -- | A backslash was followed by an invalid escape sequence.
      ErrBadEscape
        !Int -- ^ Label index
        !(Maybe Int) -- ^ The zero-based byte offset into the original input
                     -- where the offending escape begins, when known.
    | -- | The input contained an ill-formed UTF-8 byte sequence.
      ErrInvalidUtf8
        !Int -- ^ Label index
        !(Maybe Int) -- ^ The byte offset into the original input where the
                     -- ill-formed byte sequence begins, when known.  The label
                     -- index is often @-1@ because the error is detected
                     -- before any label boundary is recognised.
    | -- | The input includes a label that has no coherent presentation form: it
      -- mixes raw-byte escape syntax (@\'\\DDD\'@ or @\'\\X\'@, only allowed
      -- in @OCTET@ labels) with a codepoint outside the 8-bit range (which an
      -- @OCTET@ label cannot carry).
      ErrUnpresentableLabel
        !Int -- ^ Label index
    | -- | A codepoint outside the legal Unicode range (above @U+10FFFF@) or
      -- in the surrogate range was observed.
      ErrCodepointTooLarge
        !Int -- ^ Label index.
        !Int -- ^ Codepoint.
    | -- | The label was successfully classified, but its 'LabelForm' is not
      -- in the caller-supplied set of permitted forms.
      ErrFormNotAllowed
        !Int -- ^ Label index.
        !LabelForm -- ^ The form of the rejected label.
    | -- | A label that took the validation path failed it; see
      ErrLabelInvalid
        !Int -- ^ Label index.
        !LabelReason -- ^ The specific cause.
    | -- | A label looked like an ACE label (@\"xn--...\"@) but
      -- failed validation: Punycode decode failed, decoded form
      -- is not a valid IDN label, or re-encoding does not match.
      ErrAceInvalid
        !Int -- ^ Label index.
        !AceReason -- ^ Reason
    | -- | The label violated a cross-label bidirectional-text rule.
      -- Raised when one or more labels in the name contains right-to-left
      -- content; the rules then apply to /every/ label in the
      -- name, including pure left-to-right siblings.
      ErrCrossLabelBidi
        !Int -- ^ Label index.
        !BidiRuleViolation -- ^ Actual problem
    | -- | Internal Punycode arithmetic overflow during encode or
      -- decode.  Should not occur for compliant inputs of bounded
      -- length.
      ErrPunycodeOverflow
        !Int -- ^ Label index.
    deriving (Eq, Show)

-- | Why a label failed validation.
data LabelReason
    = DisallowedCodepoint !Int
      -- ^ A codepoint not permitted in any IDN label.  Carries
      -- the offending codepoint regardless of whether it's
      -- ASCII (e.g. an uppercase letter, an underscore) or
      -- non-ASCII (a symbol, dingbat, etc.).
    | ContextRule !Int
      -- ^ A codepoint admissible only in specific contexts (a
      -- joiner, an Arabic-Indic digit, etc.) appeared in a
      -- context where its rule isn't satisfied.
    | NotNFC
      -- ^ The label is not in Unicode Normalization Form C.
    | LabelBidi !BidiRuleViolation
      -- ^ A bidirectional-text rule was violated within the
      -- label.  See 'BidiRuleViolation' for the specific rule.
    | HyphenViolation
      -- ^ Per RFC 5891 section 4.2.3.1: the label has a leading
      -- hyphen, a trailing hyphen, or hyphens at both positions
      -- 3 and 4.  The latter form reserves those slots for the
      -- ACE prefix (@\"xn--\"@), so a U-label that contains
      -- @\"--\"@ at positions 3-4 would visually mimic an
      -- A-label without being one and is rejected.
    | LeadingCombiningMark !Int
      -- ^ The first codepoint of the label has
      -- @General_Category@ in @{Mn, Mc, Me}@.  Per RFC 5891
      -- section 4.2.3.2, a U-label must not begin with a
      -- combining mark.  Carries the offending codepoint.
    deriving (Eq, Show)

-- | Why an ACE-prefixed (@\"xn--\"@) label failed validation.
data AceReason
    = BadPunycode
      -- ^ Punycode body could not be decoded (truncated, bad
      -- delimiter, non-base-36 digit).
    | DecodedInvalid !LabelReason
      -- ^ Decoding succeeded but the result is not a valid IDN
      -- label.  See 'LabelReason' for the specific cause.
    | RoundTripMismatch
      -- ^ Decoded label re-encoded to a form that does not match
      -- the input ACE bytes.  The round-trip check is
      -- case-insensitive.
    deriving (Eq, Show)

-- | Which bidirectional-text rule a label violates, with enough
-- granularity for a caller to format a useful diagnostic
-- without having to consult the rule text.  Used as the
-- payload of 'LabelBidi' (per-label check) and
-- 'ErrCrossLabelBidi' (cross-label check).
--
-- The rules apply when a label contains right-to-left text
-- (Hebrew, Arabic, ...) or an Arabic-Indic digit.
data BidiRuleViolation
    = BidiRule1FirstNotLRAL
      -- ^ The first codepoint of a bidirectional label
      -- was not a letter from a known left-to-right or
      -- right-to-left script.  Most often catches digit-, mark-,
      -- or punctuation-leading labels in a name that also has a
      -- right-to-left component (e.g. @\"_tcp\"@, @\"123\"@).
    | BidiRule2RTLDisallowed
      -- ^ A right-to-left label (one whose first codepoint is a
      -- right-to-left letter) contained a codepoint that is not
      -- allowed in such a label — typically a left-to-right
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
      -- allowed in such a label — typically a right-to-left
      -- letter or an Arabic-Indic digit.
    | BidiRule6LTRBadEnd
      -- ^ A left-to-right label's last non-mark codepoint was
      -- not a letter or a European-style digit.
    deriving (Eq, Show)
