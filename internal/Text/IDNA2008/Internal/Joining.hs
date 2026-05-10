-- |
-- Module      : Text.IDNA2008.Internal.Joining
-- Description : Joining_Type and Virama (Canonical_Combining_Class
--               = 9) predicates for the CONTEXTJ contextual rules
--               (RFC 5892 A.1, A.2).
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Predicates over the Unicode Joining_Type and
-- Canonical_Combining_Class properties consumed by the CONTEXTJ
-- contextual rules.  Backed by the compact range tables in
-- "Text.IDNA2008.Internal.Joining.Data".
{-# LANGUAGE BangPatterns #-}

module Text.IDNA2008.Internal.Joining
    ( -- * Joining_Type predicates
      jtIsLeftOrDual
    , jtIsRightOrDual
    , jtIsTransparent

      -- * Canonical_Combining_Class
    , isVirama
    ) where

import Data.Array.Byte (ByteArray)
import Data.Primitive.ByteArray (indexByteArray)
import Data.Word (Word8, Word32)

import Text.IDNA2008.Internal.Joining.Data
    ( jtRangeCount
    , jtRangeStarts
    , jtRangeEnds
    , jtRangeTags
    , jtTagL
    , jtTagR
    , jtTagD
    , jtTagT
    , viramaRangeCount
    , viramaRanges
    )

-- | Is @cp@'s Joining_Type either @L@ (Left_Joining) or @D@
-- (Dual_Joining)?  RFC 5892 A.1 consults this on the codepoint
-- to the left of @U+200C@ (after skipping any 'jtIsTransparent'
-- run).
jtIsLeftOrDual :: Int -> Bool
jtIsLeftOrDual !cp = case lookupJt cp of
    Nothing -> False
    Just t  -> t == jtTagL || t == jtTagD
{-# INLINE jtIsLeftOrDual #-}

-- | Is @cp@'s Joining_Type either @R@ (Right_Joining) or @D@
-- (Dual_Joining)?  RFC 5892 A.1 consults this on the codepoint
-- to the right of @U+200C@ (after skipping any 'jtIsTransparent'
-- run).
jtIsRightOrDual :: Int -> Bool
jtIsRightOrDual !cp = case lookupJt cp of
    Nothing -> False
    Just t  -> t == jtTagR || t == jtTagD
{-# INLINE jtIsRightOrDual #-}

-- | Is @cp@'s Joining_Type @T@ (Transparent)?  Transparent
-- codepoints (combining marks etc.) are skipped over by the
-- CONTEXTJ A.1 regex.
jtIsTransparent :: Int -> Bool
jtIsTransparent !cp = case lookupJt cp of
    Nothing -> False
    Just t  -> t == jtTagT
{-# INLINE jtIsTransparent #-}

-- | Is @cp@'s Canonical_Combining_Class equal to 9 (Virama)?
-- Both A.1 (ZWNJ) and A.2 (ZWJ) admit their codepoint when the
-- immediately preceding codepoint is a Virama.
isVirama :: Int -> Bool
isVirama = inRanges viramaRanges viramaRangeCount
{-# INLINE isVirama #-}

----------------------------------------------------------------------
-- Lookup helpers
----------------------------------------------------------------------

-- | Look up the Joining_Type tag for @cp@ in the (start, end, tag)
-- triple table.  Returns 'Nothing' for codepoints whose
-- Joining_Type is not in the relevant set @{L, R, D, T}@ (which
-- includes @C@, @U@, and unassigned codepoints).
lookupJt :: Int -> Maybe Word8
lookupJt !cp
    | jtRangeCount == 0      = Nothing
    | cp < startAt 0         = Nothing
    | otherwise =
        let !idx = bsearch 0 (jtRangeCount - 1)
        in if cp <= endAt idx
             then Just (tagAt idx)
             else Nothing
  where
    startAt :: Int -> Int
    startAt !i = fromIntegral (indexByteArray jtRangeStarts i :: Word32)
    {-# INLINE startAt #-}

    endAt :: Int -> Int
    endAt !i = fromIntegral (indexByteArray jtRangeEnds i :: Word32)
    {-# INLINE endAt #-}

    tagAt :: Int -> Word8
    tagAt !i = indexByteArray jtRangeTags i
    {-# INLINE tagAt #-}

    bsearch :: Int -> Int -> Int
    bsearch !lo !hi
      | lo == hi = lo
      | otherwise =
          let !mid = (lo + hi + 1) `quot` 2
          in if startAt mid <= cp
               then bsearch mid hi
               else bsearch lo (mid - 1)
{-# INLINE lookupJt #-}

-- | Test whether @cp@ falls within any of the @cnt@
-- @[start_i, end_i]@ ranges (sorted by start), encoded as
-- alternating Word32 entries.  Same shape as
-- 'Text.IDNA2008.Internal.Script.inRanges'; a copy lives here
-- to avoid cross-module dependency.
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
