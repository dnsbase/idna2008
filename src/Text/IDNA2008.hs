-- |
-- Module      : Text.IDNA2008
-- Description : Strict IDNA2008 parser and renderer.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Public API for the @idna2008@ library.  Given a domain name as
-- the user typed it -- possibly mixing Latin, Greek, Hebrew,
-- Arabic, CJK, or any other Unicode script -- this library
-- checks that every label is well-formed, encodes any non-ASCII
-- labels into their @\"xn--\"@-prefixed Punycode form for the
-- wire, classifies each label, and (optionally) renders the
-- parsed name back into display form.
--
-- == Quick start
--
-- > import Text.IDNA2008
-- >
-- > -- Parse a name and get the wire-form bytes plus per-label
-- > -- classification.
-- > case parseDomain hostnameLabelForms "www.example.com" of
-- >   Right (dom, info) -> do
-- >     -- wireBytes dom         :: ByteString
-- >     -- getLabelForms info    :: [LabelForm]
-- >     -- getLabelForm info 0   :: LabelForm
-- >     ...
-- >   Left err -> ...
--
-- == Strictness
--
-- This library implements /strict/ IDNA2008 (RFCs 5891-5895 plus
-- RFC 5893 Bidi rules and RFC 3492 Punycode).  Some browsers and
-- language standard libraries use a more permissive variant that
-- accepts characters strict IDNA2008 rejects; this library does
-- not use that variant.
--
-- == Terminology: \"encode\" and \"decode\"
--
-- The library uses /encode/ and /decode/ in the IDNA-spec
-- sense, centred on the Punycode transformation between a
-- U-label and its @\"xn--\"@-prefixed ACE form:
--
--   * 'parseDomain' (presentation -> wire) is the /encoding/
--     side: a non-ASCII label that arrives in U-label form is
--     Punycode-encoded into its ACE form before being placed on
--     the wire.
--
--   * 'domainToUnicode' (wire -> presentation) is the /decoding/
--     side: an ACE label is Punycode-decoded back to its U-label
--     form for display.
--
-- The directional vocabulary is sharp only for labels that
-- involve a Punycode step.  ASCII LDH labels pass through both
-- directions byte-for-byte.  'OCTET' labels (non-LDH bytes
-- admitted as raw octets) round-trip through @\\DDD@-style
-- escapes rather than Punycode.  So /encode/ and /decode/ here
-- name the IDN-relevant transformation; outside the
-- Punycode-encoded labels there is no encoding happening, just
-- presentation framing.
--
-- The 'LAXDECODE' parser flag, which appears at the rendering
-- side, follows this convention: it relaxes the
-- Punycode-decode-and-validate step that 'domainToUnicode'
-- performs on each ACE label.
module Text.IDNA2008
    ( -- * Domain names
      Domain (Domain)
    , wireBytes
    , toLabels
    , isValidDomainWire

      -- * Parsing presentation form
      -- $parsing

      -- ** Returning wire form plus per-label classification
    , parseDomain
    , parseDomainOpts
    , parseDomainUtf8
    , parseDomainShort

      -- ** Total convenience wrappers
      -- $convenience
    , mkDomain
    , mkDomainStr
    , mkDomainUtf8
    , mkDomainShort

      -- ** Compile-time literals
      -- $literals
    , dnLit
    , dnLitAs

      -- * Rendering wire form back to presentation form
    , domainToAscii
    , domainToUnicode
    , domainToUnicodeOpt
    , domainToUnicodeLax
    , labelToAscii
    , labelToUnicode
    , labelToUnicodeLax

      -- * Per-label classification result
      -- $labelinfo
    , LabelInfo
    , getLabelForms
    , getLabelFormCount
    , getLabelForm
    , meetsLabelFormSet

      -- * Label form singletons
      -- $labelform
    , LabelForm(..)

      -- * Label form sets
      -- $labelformset
    , LabelFormSet
    , (<+>)
    , (<->)
    , labelFormSetFromList
    , memberLabelFormSet
    , withoutLabelFormSet
    , allLabelForms
    , idnLabelForms
    , hostnameLabelForms

      -- * Parser flags
      -- $flags
    , IdnaFlags(..)
    , defaultIdnaFlags
    , meetsIdnaFlags
    , withoutIdnaFlags

      -- * Errors
    , IdnaError(..)
    , IdnaLoc(..)
    , LabelReason(..)
    , AceReason(..)
    , BidiRuleViolation(..)

      -- * CLI vocabulary
      -- $cli
    , idnaFlagsTokens
    , idnaFlagsPresets
    , parseIdnaFlags
    , parseIdnaFlagsStr
    , labelFormSetTokens
    , labelFormSetPresets
    , parseLabelFormSet
    , parseLabelFormSetStr
    ) where

import Text.IDNA2008.Internal.Domain
    ( Domain (Domain)
    , isValidDomainWire
    , toLabels
    , wireBytes
    )
import Text.IDNA2008.Internal.Error
    ( AceReason(..)
    , BidiRuleViolation(..)
    , IdnaError(..)
    , IdnaLoc(..)
    , LabelReason(..)
    )
import Text.IDNA2008.Internal.Flags
    ( IdnaFlags(..)
    , defaultIdnaFlags
    , idnaFlagsPresets
    , idnaFlagsTokens
    , meetsIdnaFlags
    , parseIdnaFlags
    , parseIdnaFlagsStr
    , withoutIdnaFlags
    )
import Text.IDNA2008.Internal.LabelForm ( LabelForm(..) )
import Text.IDNA2008.Internal.LabelFormSet
    ( LabelFormSet
    , (<+>)
    , (<->)
    , allLabelForms
    , hostnameLabelForms
    , idnLabelForms
    , labelFormSetFromList
    , labelFormSetPresets
    , labelFormSetTokens
    , memberLabelFormSet
    , parseLabelFormSet
    , parseLabelFormSetStr
    , withoutLabelFormSet
    )
import Text.IDNA2008.Internal.LabelInfo
    ( LabelInfo
    , getLabelForm
    , getLabelFormCount
    , getLabelForms
    , meetsLabelFormSet
    )
import Text.IDNA2008.Internal.Parse
    ( dnLit
    , dnLitAs
    , domainToAscii
    , domainToUnicode
    , domainToUnicodeLax
    , domainToUnicodeOpt
    , labelToAscii
    , labelToUnicode
    , labelToUnicodeLax
    , mkDomain
    , mkDomainShort
    , mkDomainStr
    , mkDomainUtf8
    , parseDomain
    , parseDomainOpts
    , parseDomainShort
    , parseDomainUtf8
    )

-- $parsing
-- The four parsing entry points share a shape: pass the
-- 'LabelFormSet' the application accepts (typically
-- 'hostnameLabelForms' or 'idnLabelForms') plus the input,
-- get back either a parse error or the parsed 'Domain' paired
-- with the per-label classification ('LabelInfo').  They differ
-- only in input type, chosen to match what the caller already
-- has on hand:
--
--   * 'parseDomain' takes 'Data.Text.Text'.  Since
--     'Data.Text.Text' enforces well-formed UTF-8 at
--     construction, the parser views the underlying byte array
--     directly with no extra copy or re-validation.
--   * 'parseDomainOpts' is the same as 'parseDomain' but takes
--     an explicit 'IdnaFlags' bitmask (the other three use
--     'defaultIdnaFlags').
--   * 'parseDomainUtf8' takes a UTF-8-encoded
--     'Data.ByteString.ByteString' -- the convenient entry
--     point when the caller already has raw bytes (a network
--     read, a file, a protocol decoder) and would otherwise
--     pay a 'Data.Text.Encoding.decodeUtf8' just to reach
--     'parseDomain'.  One copy is unavoidable because
--     'Data.ByteString.ByteString' is pinned and the parser
--     wants an unpinned buffer.
--   * 'parseDomainShort' is the zero-copy variant for callers
--     who already have a 'Data.ByteString.Short.ShortByteString'.
--
-- The 'Data.ByteString.ByteString' and
-- 'Data.ByteString.Short.ShortByteString' variants are /not/
-- assumed to be well-formed UTF-8; the parser performs a
-- strict RFC 3629 decode and reports @'ErrInvalidUtf8'@ on any
-- ill-formed sequence.

-- $convenience
-- The 'mkDomain' family is for callers who only care whether a
-- name is well-formed: they use 'hostnameLabelForms' and
-- 'defaultIdnaFlags' implicitly, return 'Maybe' instead of
-- 'Either', and discard the classification.  Use these when
-- wrapping user input where any IDN failure is just \"bad
-- name\".  For diagnostics or stricter policies, prefer the
-- 'parseDomain' family.

-- $literals
-- Compile-time domain literals via Template Haskell.  An
-- ill-formed name fails to compile rather than at run time:
--
-- > example :: Domain
-- > example = $$(dnLit "www.example.com")
--
-- 'dnLitAs' is the same idea but takes an explicit
-- 'LabelFormSet' and 'IdnaFlags' bitmask, useful when a
-- specific application policy needs to apply to literals.

-- $labelinfo
-- 'LabelInfo' is the parser's per-label classification result.
-- The representation is opaque; inspect via the accessors.
-- 'getLabelForms' returns the list in label order;
-- 'getLabelFormCount' returns the label count;
-- 'getLabelForm' indexes the array (returning 'NoLabel' for
-- out-of-range indices, including any index against a
-- zero-label 'LabelInfo').  'meetsLabelFormSet' tests whether
-- every label belongs to a given 'LabelFormSet'.
--
-- The empty (root) 'LabelInfo' satisfies every 'LabelFormSet',
-- including 'mempty', by vacuous truth.  Applications that
-- want to reject the root should check 'getLabelFormCount'
-- separately.

-- $labelform
-- Each label is one of eight real classifications: 'LDH' (the
-- conventional letter-digit-hyphen alphabet), 'RLDH'
-- (LDH-shaped but with a non-IDN @--@ at positions 3 and 4),
-- 'FAKEA' (an @\"xn--\"@ label that doesn't round-trip),
-- 'ALABEL' (a valid Punycode-encoded IDN label), 'ULABEL'
-- (the parser accepted non-ASCII input directly), 'ATTRLEAF'
-- (an LDH label starting with an underscore, e.g. @_dmarc@),
-- 'OCTET' (anything with non-LDH bytes), or 'WILDLABEL' (the
-- single-byte @\'*\'@).
--
-- A ninth sentinel 'NoLabel' is returned by 'getLabelForm' for
-- out-of-range indices and against the bare-root 'LabelInfo'.
-- The parser never produces 'NoLabel' for a real label, and
-- it is not a member of any 'LabelFormSet'.

-- $labelformset
-- 'LabelFormSet' is a set of 'LabelForm' values, used as the
-- @allowed@ argument to the parser.  Build sets from
-- pre-defined named values ('allLabelForms', 'idnLabelForms',
-- 'hostnameLabelForms') or from 'mempty', combined with
-- individual 'LabelForm' values via '<+>' and '<->':
--
-- > mempty <+> LDH <+> ULABEL <+> ALABEL
-- > hostnameLabelForms <-> FAKEA
-- > idnLabelForms <+> WILDLABEL
--
-- Set-to-set composition uses '<>' (Monoid union) and
-- 'withoutLabelFormSet' (set difference).

-- $flags
-- 'IdnaFlags' is a bitmask of parser knobs.  The default
-- ('defaultIdnaFlags') is 'ALABELCHECK' '<>' 'NFCCHECK' '<>'
-- 'BIDICHECK': enforce strict A-label round-trip, require NFC,
-- and apply the cross-label Bidi rules.  Other flags (case-
-- folding, dot-mapping, width-mapping, NFC-mapping, emoji
-- relaxation, ASCII fallback on Bidi failure) are off by
-- default and opt-in.

-- $cli
-- These tables make it easy for downstream command-line tools
-- to expose the same flag and classification vocabulary
-- consistently.  'parseIdnaFlags' and 'parseLabelFormSet'
-- understand comma-separated lists with @+@\/@-@ prefixes and
-- accept either a canonical token name or any unambiguous
-- prefix of length three or more.
