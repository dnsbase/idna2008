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
-- The input is checked to consist of length-byte-prefixed
-- wire-form labels of up to 63 bytes each, not exceeding 255 bytes
-- total, terminated by a final 0-length label.
--
-- == When to use this module
--
-- The intended consumer is a downstream library or application
-- that has /already/ validated the wire bytes via its own type
-- system or parser — for instance, a DNS protocol library that
-- carries its own wire-form 'Data.ByteString.Short.ShortByteString'
-- newtype and wants to render names through IDNA2008 without
-- re-validating on every call.
module Text.IDNA2008.Wire
    ( toUnicode
    , toAscii
    , unparseOpts
    ) where

import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)

import qualified Text.IDNA2008.Internal.Parse as P
import Text.IDNA2008.Internal.Error (IdnaError)
import Text.IDNA2008.Internal.Flags (IdnaFlags)
import Text.IDNA2008.Internal.LabelFormSet (LabelFormSet)
import Text.IDNA2008.Internal.LabelInfo (LabelInfo)

-- | Render DNS wire-form bytes as Unicode presentation 'Text'.
-- Behaviour is identical to that of 'P.domainToUnicode'.
toUnicode :: ShortByteString -> Text
toUnicode = P.domainToUnicode . P.Domain

-- | Render DNS wire-form bytes as ASCII presentation 'Text'.
-- Behaviour is identical to that of 'P.domainToAscii'.
toAscii :: ShortByteString -> Text
toAscii = P.domainToAscii . P.Domain

-- | Render DNS wire-form bytes under caller-chosen 'LabelFormSet'
-- and 'IdnaFlags', paired with the resulting 'LabelInfo'.
-- Behaviour is identical to that of 'P.unparseDomainOpts'.
unparseOpts
    :: LabelFormSet
    -> IdnaFlags
    -> ShortByteString
    -> Either IdnaError (Text, LabelInfo)
unparseOpts forms flags = P.unparseDomainOpts forms flags . P.Domain
