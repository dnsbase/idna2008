-- |
-- Module      : Text.IDNA2008.Internal.Util
-- Description : Tiny helpers shared across the internal modules
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Two coercions between primitive byte buffers that the parser
-- and the wire-form 'Text.IDNA2008.Internal.Domain.Domain' use
-- on the hot path.  Both are zero-copy: the underlying
-- 'ByteArray#' is the same in either direction.
module Text.IDNA2008.Internal.Util
    ( baToShortByteString
    , sbsToByteArray
    ) where

import Data.Array.Byte (ByteArray(..))
import Data.ByteString.Short (ShortByteString(SBS))

-- | Reinterpret a 'ByteArray' as a 'ShortByteString'.  Zero-copy.
baToShortByteString :: ByteArray -> ShortByteString
baToShortByteString (ByteArray ba) = SBS ba

-- | Reinterpret a 'ShortByteString' as a 'ByteArray'.  Zero-copy.
sbsToByteArray :: ShortByteString -> ByteArray
sbsToByteArray (SBS ba) = ByteArray ba
