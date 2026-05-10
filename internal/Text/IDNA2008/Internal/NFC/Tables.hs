-- |
-- Module      : Text.IDNA2008.Internal.NFC.Tables
-- Description : Typed wrappers over the Unicode normalization
--               tables consumed by the full NFC validator.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Three lookups, plus the Hangul algorithmic decomposition and
-- composition.  Each lookup is a binary search over a sorted
-- range or key array in
-- "Text.IDNA2008.Internal.NFC.Tables.Data".  Codepoints not in
-- the relevant table are reported as "no decomposition", "no
-- composition", or @CCC = 0@.
--
-- Hangul syllables are intentionally absent from the tables; they
-- are decomposed and recomposed by closed-form arithmetic
-- ('hangulDecompose', 'hangulCompose') because the table form
-- would be over 11000 entries.
{-# LANGUAGE BangPatterns #-}

module Text.IDNA2008.Internal.NFC.Tables
    ( -- * Decomposition
      canonDecompose
    , hangulDecompose

      -- * Composition
    , canonCompose
    , hangulCompose

      -- * Canonical_Combining_Class
    , cccOf

      -- * Hangul constants (re-exported for the algorithm)
    , hangulSBase
    , hangulSCount
    ) where

import Data.Primitive.ByteArray (indexByteArray)
import Data.Word (Word8, Word32)

import Text.IDNA2008.Internal.NFC.Tables.Data

----------------------------------------------------------------------
-- Hangul algorithmic constants and helpers
----------------------------------------------------------------------

hangulSBase, hangulSCount :: Int
hangulSBase  = 0xAC00
hangulSCount = 11172

hangulLBase, hangulLCount :: Int
hangulLBase  = 0x1100
hangulLCount = 19

hangulVBase, hangulVCount :: Int
hangulVBase  = 0x1161
hangulVCount = 21

hangulTBase, hangulTCount :: Int
hangulTBase  = 0x11A7
hangulTCount = 28

-- | Cluster size: @vCount * tCount@.
hangulNCount :: Int
hangulNCount = hangulVCount * hangulTCount

-- | Hangul algorithmic canonical decomposition.  A syllable in
-- @[U+AC00, U+D7A3]@ decomposes either into @(L, V)@ if its
-- TIndex is zero, or into @(L, V, T)@ otherwise.  For codepoints
-- outside the syllable range, 'Nothing'.
hangulDecompose :: Int -> Maybe (Int, Int, Maybe Int)
hangulDecompose !cp
    | cp < hangulSBase || sIndex >= hangulSCount = Nothing
    | tIndex == 0 = Just (l, v, Nothing)
    | otherwise   = Just (l, v, Just t)
  where
    !sIndex = cp - hangulSBase
    !lIndex = sIndex `quot` hangulNCount
    !vIndex = (sIndex `rem` hangulNCount) `quot` hangulTCount
    !tIndex = sIndex `rem` hangulTCount
    !l = hangulLBase + lIndex
    !v = hangulVBase + vIndex
    !t = hangulTBase + tIndex
{-# INLINE hangulDecompose #-}

-- | Hangul algorithmic primary composition.  Two cases: an L
-- jamo plus a V jamo composes into an LV syllable; an LV
-- syllable (one whose TIndex is zero) plus a T jamo composes
-- into an LVT syllable.  Anything else returns 'Nothing'.
hangulCompose :: Int -> Int -> Maybe Int
hangulCompose !first !second
    -- L + V -> LV
    | lIdx <- first - hangulLBase
    , vIdx <- second - hangulVBase
    , lIdx >= 0, lIdx < hangulLCount
    , vIdx >= 0, vIdx < hangulVCount
        = Just (hangulSBase + (lIdx * hangulVCount + vIdx) * hangulTCount)
    -- LV + T -> LVT (only if @first@ is an LV-shaped syllable
    -- and @second@ is a T jamo strictly after the T-zero filler).
    | sIdx <- first - hangulSBase
    , sIdx >= 0, sIdx < hangulSCount
    , sIdx `rem` hangulTCount == 0
    , tIdx <- second - hangulTBase
    , tIdx > 0, tIdx < hangulTCount
        = Just (first + tIdx)
    | otherwise = Nothing
{-# INLINE hangulCompose #-}

----------------------------------------------------------------------
-- Canonical decomposition (table)
----------------------------------------------------------------------

-- | Look up the canonical decomposition of @cp@ in the
-- 'decompKeys' table.  Returns @Just (a, b)@ for a two-codepoint
-- decomposition, or @Just (a, 0)@ for a singleton decomposition
-- (where @0@ is a sentinel meaning \"no second codepoint\").
-- Hangul syllables are absent from the table; use
-- 'hangulDecompose' instead.
canonDecompose :: Int -> Maybe (Int, Int)
canonDecompose !cp
    | decompCount == 0 = Nothing
    | cp < keyAt 0 || cp > keyAt (decompCount - 1) = Nothing
    | otherwise = case bsearchKey 0 (decompCount - 1) of
        Just i  -> Just (firstAt i, secondAt i)
        Nothing -> Nothing
  where
    keyAt :: Int -> Int
    keyAt !i = fromIntegral (indexByteArray decompKeys i :: Word32)
    {-# INLINE keyAt #-}

    firstAt :: Int -> Int
    firstAt !i = fromIntegral (indexByteArray decompFirst i :: Word32)
    {-# INLINE firstAt #-}

    secondAt :: Int -> Int
    secondAt !i = fromIntegral (indexByteArray decompSecond i :: Word32)
    {-# INLINE secondAt #-}

    -- Standard binary search for an exact match.
    bsearchKey :: Int -> Int -> Maybe Int
    bsearchKey !lo !hi
      | lo > hi = Nothing
      | otherwise =
          let !mid = (lo + hi) `quot` 2
              !k   = keyAt mid
          in if | k == cp -> Just mid
                | k <  cp -> bsearchKey (mid + 1) hi
                | otherwise -> bsearchKey lo (mid - 1)
{-# INLINE canonDecompose #-}

----------------------------------------------------------------------
-- Primary composition (table)
----------------------------------------------------------------------

-- | Look up the primary composition of @(first, second)@ in the
-- composition table.  Returns @Just result@ if such a composition
-- exists in Unicode (and was not excluded), else 'Nothing'.
-- Hangul handled separately via 'hangulCompose'.
canonCompose :: Int -> Int -> Maybe Int
canonCompose !first !second
    | compCount == 0 = Nothing
    | otherwise = case bsearchFirst 0 (compCount - 1) of
        Nothing -> Nothing
        Just (lo, hi) -> linearMatchSecond lo hi
  where
    firstAt :: Int -> Int
    firstAt !i = fromIntegral (indexByteArray compFirst i :: Word32)
    {-# INLINE firstAt #-}

    secondAt :: Int -> Int
    secondAt !i = fromIntegral (indexByteArray compSecond i :: Word32)
    {-# INLINE secondAt #-}

    resultAt :: Int -> Int
    resultAt !i = fromIntegral (indexByteArray compResult i :: Word32)
    {-# INLINE resultAt #-}

    -- Find the (inclusive) range of indices where firstAt i == first.
    -- Return 'Nothing' if no such range exists; otherwise (lo, hi).
    bsearchFirst :: Int -> Int -> Maybe (Int, Int)
    bsearchFirst !lo !hi
      | lo > hi = Nothing
      | otherwise =
          let !mid = (lo + hi) `quot` 2
              !k   = firstAt mid
          in if | k == first ->
                    -- Expand around mid to cover the full equal-first run.
                    let !rl = expandLeft mid
                        !rh = expandRight mid
                    in Just (rl, rh)
                | k <  first -> bsearchFirst (mid + 1) hi
                | otherwise  -> bsearchFirst lo (mid - 1)

    -- Walk left from `i` while firstAt is still equal to `first`.
    expandLeft :: Int -> Int
    expandLeft !i
      | i > 0, firstAt (i - 1) == first = expandLeft (i - 1)
      | otherwise = i

    expandRight :: Int -> Int
    expandRight !i
      | i + 1 < compCount, firstAt (i + 1) == first = expandRight (i + 1)
      | otherwise = i

    -- Within the equal-first range [lo, hi], scan for a matching
    -- second.  The range is small in practice (a starter composes
    -- with at most a handful of combining marks).
    linearMatchSecond :: Int -> Int -> Maybe Int
    linearMatchSecond !lo !hi
      | lo > hi = Nothing
      | secondAt lo == second = Just (resultAt lo)
      | otherwise = linearMatchSecond (lo + 1) hi
{-# INLINE canonCompose #-}

----------------------------------------------------------------------
-- Canonical_Combining_Class
----------------------------------------------------------------------

-- | Look up the Canonical_Combining_Class of @cp@.  Codepoints
-- absent from the CCC table are starters (CCC = 0).
cccOf :: Int -> Word8
cccOf !cp
    | cccCount == 0 || cp < startAt 0 = 0
    | otherwise =
        let !idx = bsearchStarts 0 (cccCount - 1)
        in if cp <= endAt idx
             then valueAt idx
             else 0
  where
    startAt :: Int -> Int
    startAt !i = fromIntegral (indexByteArray cccStarts i :: Word32)
    {-# INLINE startAt #-}

    endAt :: Int -> Int
    endAt !i = fromIntegral (indexByteArray cccEnds i :: Word32)
    {-# INLINE endAt #-}

    valueAt :: Int -> Word8
    valueAt !i = indexByteArray cccValues i
    {-# INLINE valueAt #-}

    bsearchStarts :: Int -> Int -> Int
    bsearchStarts !lo !hi
      | lo == hi = lo
      | otherwise =
          let !mid = (lo + hi + 1) `quot` 2
          in if startAt mid <= cp
               then bsearchStarts mid hi
               else bsearchStarts lo (mid - 1)
{-# INLINE cccOf #-}
