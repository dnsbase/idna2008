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
    , IdnaLoc(..)
    , LabelReason(..)
    , AceReason(..)
    , BidiRuleViolation(..)
    , noLoc
    , atLabel
    ) where

import Text.IDNA2008.Internal.LabelForm (LabelForm)

-- | Which label of the input the error is associated with.
-- Zero-based and best-effort: a value of @-1@ means \"not
-- associated with a particular label\" (e.g. ill-formed UTF-8
-- before the parser has identified any label boundary, or an
-- error about overall name length).
--
-- The library does /not/ surface input byte offsets for most
-- error cases.  Once per-label mappings -- @MAPNFC@,
-- @MAPWIDTH@, @MAPCASE@ -- have run, codepoints in the
-- validation buffer no longer correspond 1:1 to byte positions
-- in the original input, so reporting a sensible byte offset
-- would require a separately-maintained origin table and
-- doubling the per-codepoint cost across the parser.  The
-- offending codepoint (carried by 'DisallowedCodepoint',
-- 'ContextRule', 'LeadingCombiningMark', ...) plus the label
-- index together cover the vast majority of diagnostic needs.
-- The two early-stage errors that /do/ know an exact byte
-- offset -- 'ErrBadEscape' and 'ErrInvalidUtf8' -- carry it as
-- a separate constructor field.
newtype IdnaLoc = IdnaLoc { idnaLabelIndex :: Int }
    deriving (Eq, Show)

-- | A location representing \"not associated with any
-- particular label\".
noLoc :: IdnaLoc
noLoc = IdnaLoc (-1)

-- | Build a per-label location.
atLabel :: Int -> IdnaLoc
atLabel = IdnaLoc

-- | Possible failure modes when parsing a presentation-form
-- domain name into wire form, including IDN encoding /
-- decoding errors.
data IdnaError
    = ErrEmptyLabel !IdnaLoc
      -- ^ An empty (non-final) label was encountered, e.g.
      -- @\"foo..bar\"@.
    | ErrLabelTooLong !IdnaLoc !Int
      -- ^ A label exceeds 63 wire octets.  The 'Int' is the
      -- actual length.
    | ErrNameTooLong !Int
      -- ^ The total wire form (including the terminal empty
      -- label) exceeds 255 octets.  The 'Int' is the actual
      -- length.
    | ErrBadEscape !IdnaLoc !(Maybe Int)
      -- ^ A backslash was followed by an invalid escape sequence
      -- (decimal value > 255, fewer than three digits when
      -- digits started, or end-of-input).  The @'Maybe' 'Int'@
      -- is the zero-based byte offset into the original input
      -- where the offending escape begins, when known.
    | ErrInvalidUtf8 !IdnaLoc !(Maybe Int)
      -- ^ The input contained an ill-formed UTF-8 byte sequence.
      -- Only raised by entry points that expect UTF-8.  The
      -- @'Maybe' 'Int'@ is the zero-based byte offset into the
      -- original input where the ill-formed byte sequence
      -- begins, when known.  The 'IdnaLoc' label index is
      -- typically @-1@ because the error is detected before any
      -- label boundary is recognised.
    | ErrUnpresentableLabel !IdnaLoc
      -- ^ The input describes a single label that has no
      -- coherent presentation form: it mixes raw-byte escape
      -- syntax (@\'\\NNN\'@ or @\'\\X\'@, which signals OCTET
      -- intent) with a codepoint outside the 8-bit range
      -- (which an OCTET label cannot carry).  DNS master-file
      -- syntax offers two ways to spell a label -- LDH\/OCTET
      -- via ASCII plus byte escapes, or U-label via literal
      -- Unicode -- and the input straddles them, so the
      -- library refuses rather than silently picking one.
    | ErrCodepointTooLarge !IdnaLoc !Int
      -- ^ A codepoint outside the legal Unicode range (above
      -- @U+10FFFF@) or in the surrogate range was observed.
      -- The 'Int' is the codepoint.
    | ErrFormNotAllowed !IdnaLoc !LabelForm
      -- ^ The label was successfully classified, but its
      -- 'LabelForm' is not in the caller-supplied set of
      -- permitted forms.  The payload is the form that was
      -- produced for the rejected label.
    | ErrLabelInvalid !IdnaLoc !LabelReason
      -- ^ A label that took the validation path failed it; see
      -- 'LabelReason' for the specific cause.
    | ErrAceInvalid !IdnaLoc !AceReason
      -- ^ A label looked like an ACE label (@\"xn--...\"@) but
      -- failed validation: Punycode decode failed, decoded form
      -- is not a valid IDN label, or re-encoding does not match.
    | ErrPunycodeOverflow !IdnaLoc
      -- ^ Internal Punycode arithmetic overflow during encode or
      -- decode.  Should not occur for compliant inputs of bounded
      -- length.
    | ErrCrossLabelBidi !Int !BidiRuleViolation
      -- ^ The label at the given zero-based index violated a
      -- cross-label bidirectional-text rule.  Raised when one
      -- or more labels in the name contains right-to-left
      -- content; the rules then apply to /every/ label in the
      -- name, including pure left-to-right siblings.
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
      -- ACE escape prefix (@\"xn--\"@), so a U-label that
      -- contains @\"--\"@ at positions 3-4 would visually mimic
      -- an A-label without being one and is rejected.  Applies
      -- to both LDH-style and U-label classifications.
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
      -- the input ACE bytes (modulo the case of the @\"xn\"@
      -- prefix).
    deriving (Eq, Show)

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
