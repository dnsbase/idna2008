-- |
-- Module      : Text.IDNA2008.Internal.Ranges
-- Description : Shared binary-search helper for sorted codepoint
--               range tables packed as @[start, end, start, end, ...]@
--               'Word32' entries in a 'ByteArray'.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- One canonical implementation of @inRanges@ -- a binary search
-- over a sorted, coalesced array of @[start, end]@ pairs packed
-- into a 'ByteArray' as alternating 'Word32' entries.  Consumed
-- by the property-table wrappers ("Text.IDNA2008.Internal.Emoji",
-- "Text.IDNA2008.Internal.Joining",
-- "Text.IDNA2008.Internal.NFC", "Text.IDNA2008.Internal.Script")
-- when their range data is shaped that way.
--
-- The function is intentionally marked @INLINE@; callers
-- partially-apply it to a specific @arr@ and @cnt@ at the top
-- level, and GHC specialises the worker per call site.
{-# LANGUAGE BangPatterns #-}

module Text.IDNA2008.Internal.Ranges
    ( inRanges
    ) where

import Data.Array.Byte (ByteArray)
import Data.Primitive.ByteArray (indexByteArray)
import Data.Word (Word32)

-- | Test whether @cp@ falls within any of the @cnt@
-- @[start_i, end_i]@ ranges packed into @arr@ as alternating
-- 'Word32' entries, sorted ascending by @start_i@.
--
-- Returns 'False' when @cnt == 0@, and short-circuits @'False'@
-- when @cp@ precedes the first range's start.
inRanges :: ByteArray -> Int -> Int -> Bool
inRanges !arr !cnt !cp
    | cnt == 0 || cp < startAt 0 = False
    | otherwise =
        let !idx = bsearch 0 (cnt - 1)
        in cp <= endAt idx
  where
    startAt :: Int -> Int
    startAt !i = fromIntegral (indexByteArray arr (2 * i) :: Word32)
    {-# INLINE startAt #-}

    endAt :: Int -> Int
    endAt !i = fromIntegral (indexByteArray arr (2 * i + 1) :: Word32)
    {-# INLINE endAt #-}

    bsearch :: Int -> Int -> Int
    bsearch !lo !hi
      | lo == hi = lo
      | otherwise =
          let !mid = (lo + hi + 1) `quot` 2
          in if startAt mid <= cp
               then bsearch mid hi
               else bsearch lo (mid - 1)
{-# INLINE inRanges #-}
