-- |
-- Module      : Text.IDNA2008
-- Description : Strict IDNA2008 parser and renderer.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Given a domain name as the user typed it — possibly mixing
-- Latin, Greek, Hebrew, Arabic, CJK, or any other Unicode script
-- — this library checks that every label is well-formed, encodes
-- any non-ASCII labels into their ACE-prefixed Punycode
-- form for the wire, classifies each label, and (optionally)
-- renders the parsed name back into display form.
--
-- == Quick start
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > module Main(main) where
-- > import qualified Data.Text.IO as T
-- > import Text.IDNA2008
-- >
-- > main :: IO ()
-- > main = do
-- >     -- Strict-IDN parse + render two ways.  'mkDomain' uses
-- >     -- 'idnLabelForms' and 'defaultIdnaFlags'.
-- >     case mkDomain "www.αβγ.gr" of
-- >         Right dom -> do
-- >             T.putStrLn $ domainToUnicode dom
-- >             T.putStrLn $ domainToAscii  dom
-- >         Left err -> print err
-- >
-- >     -- The full parser returns the wire-form 'Domain' /and/ the
-- >     -- per-label classification.  'allLabelForms' admits every
-- >     -- label class a DNS zone file might carry plus 'ULABEL',
-- >     -- so '*' and '_tcp' coexist with the Unicode label.
-- >     case parseDomain allLabelForms "*._tcp.αβγ.gr" of
-- >         Right (_dom, info) -> print (getLabelForms info)
-- >         Left err           -> print err
--
-- Which produces:
--
-- > ghci> main
-- > www.αβγ.gr
-- > www.xn--mxacd.gr
-- > [WILDLABEL,ATTRLEAF,ULABEL,LDH]
--
-- == Strictness
--
-- This library implements /strict/ IDNA2008 (RFCs 5891-5894
-- including RFC 5893 Bidi rules and RFC 3492 Punycode).  Optional
-- RFC5895 mappings are available when parsing text inputs.  A
-- basic set of CJK mappings from UTS #46 can also be enabled, and
-- implies the full set of RFC5895 mappings.
--
-- == Terminology: \"encode\" and \"decode\"
--
-- The library uses /encode/ and /decode/ in the IDNA-spec
-- sense, centred on the Punycode transformation between a
-- U-label and its ACE-prefixed (A-label) encoded form:
--
--   * 'parseDomainOpts' (presentation -> wire) is the /encoding/
--     side: non-ASCII labels that arrive in U-label form are
--     Punycode-encoded to their ACE-prefixed forms used "on the wire"
--     in DNS queries.
--
--   * 'unparseDomainOpts' (wire -> presentation) is the /decoding/
--     side: ACE-prefixed labels are Punycode-decoded back to their
--     U-label forms for display.
--
-- This terminology is natural for transformations between
-- U-labels and A-labels.  ASCII LDH labels pass through both
-- directions byte-for-byte.  The /encoding/ of 'OCTET' labels
-- (non-LDH bytes admitted as raw octets) to wire form requires
-- decoding backslash-escaped characters or @\\DDD@ decimal
-- triples from their presentation form, and /decoding/ of these
-- labels from wire form to presentation form requires encoding
-- many non-LDH octets as escaped characters or decimal triples.
module Text.IDNA2008
    ( -- * Domain names
      Domain(..)
    , toLabels
    , isValidWireForm
    , wireBytes
    , wireBytesShort

      -- * Parsing presentation form
      -- $parsing

      -- ** Returning wire form plus per-label classification
    , parseDomain
    , parseDomainOpts
    , parseDomainUtf8
    , parseDomainShort

      -- ** Domain-only convenience wrappers
      -- $convenience
    , mkDomain
    , mkDomainStr
    , mkDomainUtf8
    , mkDomainShort

      -- ** Compile-time literals
      -- $literals
    , dnLit

      -- * Rendering wire form back to presentation form
    , domainToAscii
    , domainToUnicode
    , labelToAscii
    , labelToUnicode
    , unparseDomainOpts
    , unparseLabelOpts

      -- * Per-label classification result
      -- $labelinfo
    , LabelInfo
    , getLabelForms
    , getLabelFormCount
    , getLabelForm
    , allLabelFormsIn

      -- * Label form singletons
      -- $labelform
    , LabelForm
        ( LDH
        , RLDH
        , FAKEA
        , ALABEL
        , ULABEL
        , ATTRLEAF
        , OCTET
        , WILDLABEL
        , LAXULABEL
        , NoLabel
        )

      -- * Label form sets
      -- $labelformset
    , LabelFormSet
    , (<+>)
    , (<->)
    , labelFormToSet
    , memberLabelFormSet
    , withoutLabelFormSet
    , allLabelForms
    , idnLabelForms
    , hostnameLabelForms

      -- * Parser flags
      -- $flags
    , IdnaFlags
        ( ALABELCHECK
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
    , meetsIdnaFlags
    , withoutIdnaFlags

      -- * Errors
    , IdnaError(..)
    , LabelReason(..)
    , AceReason(..)
    , BidiRuleViolation(..)

      -- * CLI option parsers
      -- $cli
    , parseIdnaFlags
    , parseIdnaFlagsStr
    , parseLabelFormSet
    , parseLabelFormSetStr
    ) where

import Text.IDNA2008.Internal.Error
    ( AceReason(..)
    , BidiRuleViolation(..)
    , IdnaError(..)
    , LabelReason(..)
    )
import Text.IDNA2008.Internal.Flags
    ( IdnaFlags
        ( ALABELCHECK
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
    , meetsIdnaFlags
    , parseIdnaFlags
    , parseIdnaFlagsStr
    , withoutIdnaFlags
    )
import Text.IDNA2008.Internal.LabelForm
    ( LabelForm
        ( LDH
        , RLDH
        , FAKEA
        , ALABEL
        , ULABEL
        , ATTRLEAF
        , OCTET
        , WILDLABEL
        , LAXULABEL
        , NoLabel
        )
    )
import Text.IDNA2008.Internal.LabelFormSet
    ( LabelFormSet
    , (<+>)
    , (<->)
    , labelFormToSet
    , allLabelForms
    , hostnameLabelForms
    , idnLabelForms
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
    , allLabelFormsIn
    )
import Text.IDNA2008.Internal.Parse
    ( Domain(..)
    , dnLit
    , domainToAscii
    , domainToUnicode
    , labelToAscii
    , labelToUnicode
    , unparseDomainOpts
    , unparseLabelOpts
    , mkDomain
    , mkDomainShort
    , mkDomainStr
    , mkDomainUtf8
    , parseDomain
    , parseDomainOpts
    , parseDomainShort
    , parseDomainUtf8
    , isValidWireForm
    , toLabels
    , wireBytes
    , wireBytesShort
    )

-- $parsing
-- The four parsing entry points share a shape: pass the
-- 'LabelFormSet' the application accepts (typically
-- 'idnLabelForms' or 'hostnameLabelForms') plus the input,
-- get back either a parse error or the parsed 'Domain' paired
-- with the per-label classification ('LabelInfo').  They differ
-- only in input type, chosen to match what the caller already
-- has on hand:
--
-- * 'parseDomain' takes 'Data.Text.Text'.  Since
--   'Data.Text.Text' enforces well-formed UTF-8 at
--   construction, the parser views the underlying byte array
--   directly with no extra copy or re-validation.
-- * 'parseDomainOpts' is the same as 'parseDomain' but takes
--   an explicit 'IdnaFlags' bitmask (the other three use
--   'defaultIdnaFlags').
-- * 'parseDomainUtf8' takes a UTF-8-encoded
--   'Data.ByteString.ByteString' — the convenient entry
--   point when the caller already has raw bytes (a network
--   read, a file, a protocol decoder) and would otherwise
--   pay a 'Data.Text.Encoding.decodeUtf8' just to reach
--   'parseDomain'.  One copy is unavoidable because
--   'Data.ByteString.ByteString' is pinned and the parser
--   wants an unpinned buffer.
-- * 'parseDomainShort' is the zero-copy variant for callers
--   who already have a 'Data.ByteString.Short.ShortByteString'.
--
-- The 'Data.ByteString.ByteString' and
-- 'Data.ByteString.Short.ShortByteString' variants are /not/
-- assumed to be well-formed UTF-8; the parser performs a
-- strict RFC 3629 decode and reports @'ErrInvalidUtf8'@ on any
-- ill-formed sequence.

-- $convenience
-- The 'mkDomain' convenience functions use defaults for both the
-- allowed label forms and the validation flags.  They return just
-- the wire-form 'Domain' without the list of label classifications.

-- $literals
-- Compile-time literal domains via Template Haskell, by way of
-- the 'dnLit' splice primitive.  An invalid name becomes a
-- compile-time error rather than a runtime failure.

-- $labelinfo
-- 'LabelInfo' is the parser's per-label classification result.
-- The representation is opaque; inspect via the accessors.
--
-- * 'getLabelForms' returns the list in label order.
-- * 'getLabelFormCount' returns the label count.
-- * 'getLabelForm' indexes the list starting at @0@ for the first
--    label.
-- * 'allLabelFormsIn' tests whether every label belongs to a
--   given 'LabelFormSet'.
--   Vacuously returns 'True' for the root domain's 'LabelInfo'.
--   Applications that want to reject the root domain can check the
--   value of 'getLabelFormCount', or check whether the label list
--   returned by 'Text.IDNA2008.toLabels' is 'null'.

-- $labelform
-- Each label falls into one of nine classifications:
--
-- * 'LDH' — a valid label of letters, digits, and hyphens.
-- * 'RLDH' — a legacy reserved label with @--@ at positions 3-4.
-- * 'FAKEA' — an ACE-prefixed label that isn't a valid A-label.
-- * 'ALABEL' — an ACE-prefixed label that encodes a valid IDN label.
-- * 'ULABEL' — a non-ASCII label that can be part of a valid IDN.
-- * 'ATTRLEAF' — an underscore-prefixed label (e.g. @_25._tcp@).
-- * 'OCTET' — a label with characters outside the LDH alphabet.
-- * 'WILDLABEL' — the DNS wildcard label @*@.
-- * 'LAXULABEL' — a U-label that fails strict IDN validation;
--   not admitted unless the caller explicitly opts in.
--
-- A tenth sentinel ('NoLabel') is returned by 'getLabelForm' for
-- out-of-range indices (every index for the root domain which has
-- zero labels).  The parser never produces 'NoLabel' for a real
-- label, and it is not a member of any 'LabelFormSet'.

-- $labelformset
-- 'LabelFormSet' is the parser's @allowed@ argument: a set of
-- 'LabelForm' values the parser is willing to admit.

-- $flags
-- 'IdnaFlags' is a bitmask of parser options.
--
-- The default ('defaultIdnaFlags') is 'ALABELCHECK' '<>'
-- 'NFCCHECK' '<>' 'BIDICHECK': enforce strict A-label round-trip,
-- require NFC, and apply the cross-label Bidi rules.  Additional
-- non-default flags include:
--
-- * 'MAPCASE' - Fold input to lower case.
-- * 'MAPDOTS' - Accept additional Unicode label separators.
-- * 'MAPWIDTH' - Map wide/narrow codepoints.
-- * 'MAPNFC' - Normalise input to composed form (NFC).
-- * 'MAPUTS46' - A small set of UTS #46 mappings plus all the above.
-- * 'EMOJIOK' - Accept most emoji in labels.
-- * 'ASCIIFALLBACK' - Decode to A-labels when a domain violates BIDI rules

-- $cli
-- Convenience functions for command-line option parsers.
--
-- Parse comma-separated lists with @+@\/@-@ prefixes and accept
-- either an exact match or any unambiguous three-or-more letter
-- prefix.
