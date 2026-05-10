-- |
-- Module      : Text.IDNA2008.Internal.Property
-- Description : IDNA2008 codepoint disposition lookup
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- The IDNA disposition of a Unicode codepoint per
-- <https://datatracker.ietf.org/doc/html/rfc5892 RFC 5892> (and as
-- summarised by the IANA IDNA Parameters registry).  Each codepoint
-- falls into exactly one of:
--
--   * @PVALID@      -- permitted in U-labels with no further conditions
--   * @CONTEXTJ@    -- permitted only when the per-codepoint contextual
--                     /joining/ rule is satisfied (currently U+200C
--                     ZWNJ and U+200D ZWJ; rules in RFC 5892 section A.1, section A.2)
--   * @CONTEXTO@    -- permitted only when the per-codepoint contextual
--                     /other/ rule is satisfied (rules in RFC 5892 section A.3+)
--   * @DISALLOWED@  -- not permitted in any U-label
--   * @UNASSIGNED@  -- not permitted; reserved for future Unicode
--                     assignments
--
-- The lookup is implemented as a binary search over a sorted array of
-- range starts.  Each entry says \"from this codepoint until the next
-- entry's start, the disposition is /D/\".  Total per-call cost is
-- @O(log N)@ array reads and zero allocations.
--
-- == Coverage
--
-- The full per-codepoint table is loaded from
-- "Text.IDNA2008.Internal.Property.Data", which is machine-generated
-- from the IANA @idna-tables-properties.csv@ registry.  As of
-- 2024-04-26 the most recent IANA-curated derived table is aligned with
-- /Unicode 12.0.0/; the IETF position is that applications which need
-- newer Unicode coverage may compute derived properties themselves per
-- RFC 5892 section 2 -- only the contextual rules are normative across versions.
--
-- Codepoints assigned in Unicode 12.1.0 or later that are not covered
-- by the 12.0.0 derivation will be reported as @UNASSIGNED@ here, which
-- this validator rejects as not-a-U-label.  That is consistent with the
-- IETF reference and conservative for DNS use; if you specifically need
-- to admit newer codepoints, regenerate the data module from a derived
-- table you compute against the desired Unicode version.
module Text.IDNA2008.Internal.Property
    ( -- * Disposition
      IdnaDisposition(..)
      -- * Lookup
    , idnaDisposition
    ) where

import Data.Bits (unsafeShiftR)
import Data.Primitive.ByteArray (indexByteArray)
import Data.Word (Word8, Word32)

import Text.IDNA2008.Internal.Property.Data
    ( rangeCount
    , rangeStarts
    , rangeTags
    )

----------------------------------------------------------------------
-- Disposition type
----------------------------------------------------------------------

-- | IDNA2008 codepoint disposition.
data IdnaDisposition
    = IdnaPVALID
    | IdnaCONTEXTJ
    | IdnaCONTEXTO
    | IdnaDISALLOWED
    | IdnaUNASSIGNED
    deriving (Eq, Show, Enum, Bounded)

-- | Map an internal 'Word8' tag back to a disposition.  Tags
-- @0..4@ correspond to the constructors in declaration order.
{-# INLINE word8ToDisp #-}
word8ToDisp :: Word8 -> IdnaDisposition
word8ToDisp w = case w of
    0 -> IdnaPVALID
    1 -> IdnaCONTEXTJ
    2 -> IdnaCONTEXTO
    3 -> IdnaDISALLOWED
    _ -> IdnaUNASSIGNED

----------------------------------------------------------------------
-- Public lookup
----------------------------------------------------------------------

-- | Return the IDNA2008 disposition of a codepoint.  Out-of-range
-- inputs (negative or greater than @0x10FFFF@) report
-- 'IdnaDISALLOWED'.
idnaDisposition :: Int -> IdnaDisposition
idnaDisposition !cp
    | cp < 0 || cp > 0x10FFFF = IdnaDISALLOWED
    | otherwise               = word8ToDisp (lookupTag cp)
{-# INLINE idnaDisposition #-}

----------------------------------------------------------------------
-- Binary search over the codegen'd range table
----------------------------------------------------------------------

-- | Binary search for the largest index @i@ in @[0 .. rangeCount-1]@
-- such that @rangeStarts[i] <= cp@; return the disposition tag at that
-- index.  Precondition: @cp >= 0 && cp <= 0x10FFFF@; combined with the
-- table starting at @0x000000@ this guarantees a hit.
{-# INLINE lookupTag #-}
lookupTag :: Int -> Word8
lookupTag !cp = go 0 (rangeCount - 1)
  where
    !target = fromIntegral cp :: Word32

    go !lo !hi
      | lo >= hi  = readTag lo
      | otherwise =
          let !mid     = (lo + hi + 1) `unsafeShiftR` 1
              !midKey  = readStart mid
          in if midKey <= target
               then go mid hi
               else go lo (mid - 1)

    readStart i = indexByteArray rangeStarts i :: Word32
    readTag   i = indexByteArray rangeTags   i :: Word8
