-- |
-- Module      : Text.IDNA2008.Internal.Combining
-- Description : Combining-mark predicate for the RFC 5891 section
--               4.2.3.2 leading-combining-mark check.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- RFC 5891 section 4.2.3.2 says:
--
--   /The Unicode string MUST NOT begin with a combining mark or
--   combining character (see The Unicode Standard, Section 2.11
--   for an exact definition)./
--
-- A combining mark is a codepoint whose @General_Category@ is one
-- of @Mn@ (Nonspacing_Mark), @Mc@ (Spacing_Mark), or @Me@
-- (Enclosing_Mark).  RFC 5892's disposition table classifies many
-- of those codepoints (Mn / Mc) as @PVALID@, so the per-codepoint
-- validator alone doesn't catch the "no combining mark at label
-- start" rule.  This module is the dedicated lookup the validator
-- consults before walking the codepoint buffer.

module Text.IDNA2008.Internal.Combining
    ( isCombiningMark
    ) where

import Data.Bits (unsafeShiftR)
import Data.Primitive.ByteArray (indexByteArray)
import Data.Word (Word8, Word32)

import Text.IDNA2008.Internal.Combining.Data
    ( combiningRangeCount
    , combiningRangeStarts
    , combiningRangeTags
    )

-- | True iff the codepoint has @General_Category@ in @{Mn, Mc,
-- Me}@.  Out-of-range inputs return 'False'.
isCombiningMark :: Int -> Bool
isCombiningMark !cp
    | cp < 0 || cp > 0x10FFFF = False
    | otherwise               = lookupTag cp /= 0
{-# INLINE isCombiningMark #-}

-- | Binary search for the largest index @i@ such that
-- @combiningRangeStarts[i] <= cp@; return that range's tag (0 or
-- 1).  The table starts at @0x000000@ so a hit is guaranteed for
-- any @cp >= 0@.
{-# INLINE lookupTag #-}
lookupTag :: Int -> Word8
lookupTag !cp = go 0 (combiningRangeCount - 1)
  where
    !target = fromIntegral cp :: Word32

    go !lo !hi
      | lo >= hi  = readTag lo
      | otherwise =
          let !mid    = (lo + hi + 1) `unsafeShiftR` 1
              !midKey = readStart mid
          in if midKey <= target
               then go mid hi
               else go lo (mid - 1)

    readStart i = indexByteArray combiningRangeStarts i :: Word32
    readTag   i = indexByteArray combiningRangeTags   i :: Word8
