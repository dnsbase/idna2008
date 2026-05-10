-- |
-- Module      : Text.IDNA2008.Wire
-- Description : Renderers operating on DNS wire-form bytes, for
--               callers that already hold validated wire bytes.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- == Trust contract
--
-- Every function in this module accepts a 'ShortByteString' that
-- the caller asserts is a well-formed DNS wire-form domain name:
--
--   * Total length in @[1, 255]@.
--   * Each label preceded by a one-byte length in @[1, 63]@.
--   * Terminated by the zero-length root label.
--   * No DNS compression pointers, no reserved length-byte values.
--
-- /No validation is performed/.  Passing bytes that don't satisfy
-- these properties is undefined behaviour -- the renderers may
-- crash, loop, or return garbage.  Callers that have only just
-- read raw bytes from a network, file, or other untrusted source
-- should use the 'Domain'-taking variants in "Text.IDNA2008",
-- which validate at the boundary.
--
-- == When to use this module
--
-- The intended consumer is a downstream library or application
-- that has /already/ validated the wire bytes via its own type
-- system or parser -- for instance, a DNS protocol library that
-- carries its own wire-form 'Data.ByteString.Short.ShortByteString'
-- newtype and wants to render names through IDNA2008 without
-- re-validating on every call.  These callers know the bytes are
-- correct by construction, and pay no second-walk cost for the
-- safety the 'Domain'-taking versions would otherwise enforce.
--
-- == Symmetry with "Text.IDNA2008"
--
-- The four functions exported here correspond one-for-one to the
-- four 'Domain'-taking renderers in "Text.IDNA2008":
--
--   * 'toUnicode'     -- like 'Text.IDNA2008.domainToUnicode'
--   * 'toAscii'       -- like 'Text.IDNA2008.domainToAscii'
--   * 'toUnicodeOpt'  -- like 'Text.IDNA2008.domainToUnicodeOpt'
--   * 'toUnicodeLax'  -- like 'Text.IDNA2008.domainToUnicodeLax'
--
-- The 'Domain'-taking variants are thin wrappers around these:
-- they destructure the 'Domain', then call the corresponding
-- function here.  Output is byte-for-byte identical.
module Text.IDNA2008.Wire
    ( toUnicode
    , toAscii
    , toUnicodeOpt
    , toUnicodeLax
    ) where

import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)

import Text.IDNA2008.Internal.Error (IdnaError)
import Text.IDNA2008.Internal.Flags (IdnaFlags)
import qualified Text.IDNA2008.Internal.Parse as P

-- | Render DNS wire-form bytes as Unicode display 'Text',
-- decoding @\"xn--\"@-prefixed A-labels back to their U-label
-- codepoints.  No cross-label Bidi check is performed; use
-- 'toUnicodeOpt' for that.
toUnicode :: ShortByteString -> Text
toUnicode = P.unicodeFromWire

-- | Render DNS wire-form bytes as ASCII presentation 'Text', with
-- printable specials backslash-escaped and bytes outside the
-- printable-ASCII range emitted as decimal triples @\\DDD@.
-- A-labels are left in their literal @\"xn--\"@ form rather than
-- decoded.
toAscii :: ShortByteString -> Text
toAscii = P.asciiFromWire

-- | Render DNS wire-form bytes as Unicode display 'Text' under
-- caller-supplied flags.  Honours @BIDICHECK@ for cross-label
-- RFC 5893 enforcement and @ASCIIFALLBACK@ for downgrade-to-ASCII
-- on Bidi failure.  See 'Text.IDNA2008.domainToUnicodeOpt' for
-- the full flag semantics.
toUnicodeOpt :: IdnaFlags -> ShortByteString -> Either IdnaError Text
toUnicodeOpt = P.unicodeOptFromWire

-- | Lax counterpart to 'toUnicode': renders each label via
-- 'Text.IDNA2008.labelToUnicodeLax'.  Diagnostic display only;
-- the output is not guaranteed to round-trip through
-- 'Text.IDNA2008.parseDomain'.
toUnicodeLax :: ShortByteString -> Text
toUnicodeLax = P.unicodeLaxFromWire
