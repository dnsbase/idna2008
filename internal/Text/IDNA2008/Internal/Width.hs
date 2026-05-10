-- |
-- Module      : Text.IDNA2008.Internal.Width
-- Description : Typed wrapper over the fullwidth\/halfwidth
--               decomposition table consumed by the @MAPWIDTH@
--               mapping (RFC 5895 section 2.2).
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- One lookup, 'widthMapCp', backed by binary search over the sorted
-- parallel arrays in "Text.IDNA2008.Internal.Width.Data".  The
-- table is consulted only by the @MAPWIDTH@ branch in 'uLabelPath';
-- when the option is unset this module is dead code.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Text.IDNA2008.Internal.Width
    ( widthMapCp
    ) where

import Data.Primitive.ByteArray (indexByteArray)
import Data.Word (Word32)

import Text.IDNA2008.Internal.Width.Data
    ( widthRangeCount
    , widthSources
    , widthTargets
    )

-- | Apply the RFC 5895 section 2.2 fullwidth\/halfwidth mapping to
-- a single codepoint.  Returns the input unchanged if it has no
-- @\<wide\>@ or @\<narrow\>@ decomposition.  Codepoints absent
-- from the table (which is the vast majority) are unaffected.
widthMapCp :: Int -> Int
widthMapCp !cp
    | widthRangeCount == 0          = cp
    | cp < srcAt 0                  = cp
    | cp > srcAt (widthRangeCount - 1) = cp
    | otherwise = case bsearch 0 (widthRangeCount - 1) of
        Just i  -> tgtAt i
        Nothing -> cp
  where
    srcAt :: Int -> Int
    srcAt !i = fromIntegral (indexByteArray widthSources i :: Word32)
    {-# INLINE srcAt #-}

    tgtAt :: Int -> Int
    tgtAt !i = fromIntegral (indexByteArray widthTargets i :: Word32)
    {-# INLINE tgtAt #-}

    -- Standard binary search for an exact match.
    bsearch :: Int -> Int -> Maybe Int
    bsearch !lo !hi
      | lo > hi = Nothing
      | otherwise =
          let !mid = (lo + hi) `quot` 2
              !k   = srcAt mid
          in if | k == cp -> Just mid
                | k <  cp -> bsearch (mid + 1) hi
                | otherwise -> bsearch lo (mid - 1)
{-# INLINE widthMapCp #-}
