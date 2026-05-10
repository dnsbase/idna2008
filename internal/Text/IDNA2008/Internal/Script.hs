-- |
-- Module      : Text.IDNA2008.Internal.Script
-- Description : Predicates for the Unicode Scripts consulted by the
--               CONTEXTO contextual rules (RFC 5892 A.4-A.7).
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Membership predicates over the Unicode @sc@ (Script) property
-- for the four scripts named by RFC 5892 Appendix A.4-A.7:
-- Greek, Hebrew, and the Hiragana \/ Katakana \/ Han trio (the
-- last collapsed into one predicate because A.7 only ever needs
-- the union).  Backed by the compact range tables in
-- "Text.IDNA2008.Internal.Script.Data".
{-# LANGUAGE BangPatterns #-}

module Text.IDNA2008.Internal.Script
    ( isGreekCp
    , isHebrewCp
    , isHkhCp
    ) where

import Data.Array.Byte (ByteArray)
import Data.Primitive.ByteArray (indexByteArray)
import Data.Word (Word32)

import Text.IDNA2008.Internal.Script.Data
    ( greekRangeCount
    , greekRanges
    , hebrewRangeCount
    , hebrewRanges
    , hkhRangeCount
    , hkhRanges
    )

-- | Is @cp@ in the Greek script?  RFC 5892 A.4 consults this on
-- the codepoint immediately following @U+0375@ (Greek Lower
-- Numeral Sign).
isGreekCp :: Int -> Bool
isGreekCp = inRanges greekRanges greekRangeCount
{-# INLINE isGreekCp #-}

-- | Is @cp@ in the Hebrew script?  RFC 5892 A.5 \/ A.6 consult
-- this on the codepoint immediately preceding @U+05F3@ (Hebrew
-- Punctuation Geresh) or @U+05F4@ (Hebrew Punctuation
-- Gershayim).
isHebrewCp :: Int -> Bool
isHebrewCp = inRanges hebrewRanges hebrewRangeCount
{-# INLINE isHebrewCp #-}

-- | Is @cp@ in Hiragana, Katakana, or Han?  RFC 5892 A.7
-- requires that a label containing @U+30FB@ (Katakana Middle
-- Dot) also contain at least one codepoint in this union.
isHkhCp :: Int -> Bool
isHkhCp = inRanges hkhRanges hkhRangeCount
{-# INLINE isHkhCp #-}

----------------------------------------------------------------------
-- Range lookup
----------------------------------------------------------------------

-- | Binary-search a sorted run of @[start_i, end_i]@ ranges,
-- packed as alternating 'Word32's, for an interval covering the
-- given codepoint.  @cnt@ is the number of ranges (so the array
-- holds @2 * cnt@ Word32 entries).
--
-- The classic /largest start \<= cp/ binary search, with a final
-- @cp \<= end@ bounds check to guard against codepoints that fall
-- between ranges.
inRanges :: ByteArray -> Int -> Int -> Bool
inRanges !arr !cnt !cp
    | cnt == 0 || cp < startAt 0 = False
    | otherwise = let !idx = bsearch 0 (cnt - 1)
                  in cp <= endAt idx
  where
    startAt :: Int -> Int
    startAt !i = fromIntegral (indexByteArray arr (2 * i) :: Word32)
    {-# INLINE startAt #-}

    endAt :: Int -> Int
    endAt !i = fromIntegral (indexByteArray arr (2 * i + 1) :: Word32)
    {-# INLINE endAt #-}

    -- Classic upper-bound search: returns the largest @i@ in
    -- @[lo, hi]@ with @startAt i <= cp@.  Pre-condition:
    -- @startAt lo <= cp@ (guaranteed by the @cp < startAt 0@
    -- guard above).
    bsearch :: Int -> Int -> Int
    bsearch !lo !hi
      | lo == hi = lo
      | otherwise =
          let !mid = (lo + hi + 1) `quot` 2
          in if startAt mid <= cp
               then bsearch mid hi
               else bsearch lo (mid - 1)
{-# INLINE inRanges #-}
