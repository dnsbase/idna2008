-- |
-- Module      : Text.IDNA2008.Internal.Emoji
-- Description : Typed wrapper over the Unicode @Emoji=Yes@ range
--               table consumed by the @ALLOWEMOJI@ option.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- A single lookup, 'isEmoji', backed by binary search over the
-- coalesced @[start, end]@ range table in
-- "Text.IDNA2008.Internal.Emoji.Data".  The table is consulted
-- only by the @ALLOWEMOJI@ branch in the IDNA U-label validator;
-- when that option is unset, the codepoint disposition table is
-- the sole arbiter and this module is dead code.
{-# LANGUAGE BangPatterns #-}

module Text.IDNA2008.Internal.Emoji
    ( isEmoji
    ) where

import Data.Primitive.ByteArray (indexByteArray)
import Data.Word (Word32)

import Text.IDNA2008.Internal.Emoji.Data (emojiRangeCount, emojiRanges)

-- | Test whether @cp@ has the Unicode @Emoji=Yes@ property.
-- Codepoints absent from the range table are not emoji.
isEmoji :: Int -> Bool
isEmoji !cp
    | emojiRangeCount == 0 || cp < startAt 0 = False
    | otherwise =
        let !idx = bsearch 0 (emojiRangeCount - 1)
        in cp <= endAt idx
  where
    startAt :: Int -> Int
    startAt !i = fromIntegral (indexByteArray emojiRanges (2 * i) :: Word32)
    {-# INLINE startAt #-}

    endAt :: Int -> Int
    endAt !i = fromIntegral (indexByteArray emojiRanges (2 * i + 1) :: Word32)
    {-# INLINE endAt #-}

    bsearch :: Int -> Int -> Int
    bsearch !lo !hi
      | lo == hi = lo
      | otherwise =
          let !mid = (lo + hi + 1) `quot` 2
          in if startAt mid <= cp
               then bsearch mid hi
               else bsearch lo (mid - 1)
{-# INLINE isEmoji #-}
