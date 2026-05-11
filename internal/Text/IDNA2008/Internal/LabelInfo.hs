-- |
-- Module      : Text.IDNA2008.Internal.LabelInfo
-- Description : Per-label classification result.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- A 'LabelInfo' is the parser's per-label classification
-- result, paired with the wire-form
-- 'Text.IDNA2008.Internal.Domain.Domain' in the
-- @parseDomain*@ return tuple.  It is /opaque/: the internal
-- representation is hidden behind four accessors.
--
-- Each label is one of eight real classifications, which fit
-- into three bits.  Storage is therefore a packed
-- @ShortByteString@ at three bits per label (LSB-first within
-- each byte), plus a separate label count: a five-label name
-- needs two bytes, a fifteen-label name six.  The ninth
-- sentinel 'NoLabel' is never stored; it is synthesised by
-- 'getLabelForm' for out-of-range indices.
--
-- Callers inspect a 'LabelInfo' through three accessors and
-- one set-conformance check:
--
--   * 'getLabelForms' returns the list of forms in label
--     order, one entry per label.
--   * 'getLabelFormCount' returns the number of labels.
--   * 'getLabelForm' indexes the array; out-of-range indices
--     (including any index against a zero-label 'LabelInfo')
--     return 'NoLabel'.
--   * 'meetsLabelFormSet' asks whether every label in the
--     'LabelInfo' belongs to a given 'LabelFormSet'.
--
-- The empty (root) 'LabelInfo' satisfies every
-- 'LabelFormSet', including 'mempty', by vacuous truth.
-- Applications that want to reject the root should check
-- 'getLabelFormCount' separately rather than relying on
-- 'meetsLabelFormSet' to filter it out.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.IDNA2008.Internal.LabelInfo
    ( -- * Opaque per-label result
      LabelInfo
    , mkLabelInfo

      -- * Accessors
    , getLabelForms
    , getLabelFormCount
    , getLabelForm

      -- * Conformance to a permitted-set
    , meetsLabelFormSet
    ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Bits ((.|.), (.&.), unsafeShiftL, unsafeShiftR)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.Primitive.ByteArray
    ( MutableByteArray
    , newByteArray
    , readByteArray
    , setByteArray
    , unsafeFreezeByteArray
    , writeByteArray
    )
import Data.Word (Word8)

import Text.IDNA2008.Internal.LabelForm
    ( LabelForm
    , pattern NoLabel
    , unLabelForm
    , wordLabelForm
    )
import qualified Text.IDNA2008.Internal.LabelFormSet as LFS
import Text.IDNA2008.Internal.LabelFormSet (LabelFormSet)
import Text.IDNA2008.Internal.Util (baToShortByteString)

-- | The parser's per-label classification result.  Opaque;
-- inspect via the accessors.
--
-- Stored as @(count, packed)@ where the count is a 'Word8' (a
-- DNS name has at most 127 labels, so the count never exceeds
-- that) and @packed@ is a @ShortByteString@ of
-- @ceil(3 * count / 8)@ bytes holding the label tags three bits
-- each, LSB-first.  A 15-label name fits in six bytes; a
-- 127-label name (the DNS-permitted maximum with single-byte
-- labels) fits in 48 bytes.
data LabelInfo = LabelInfo
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !ShortByteString

instance Show LabelInfo where
    show info = show (getLabelForms info)

-- | Number of packed bytes needed to hold @n@ three-bit values.
packedBytes :: Int -> Int
packedBytes !n = (3 * n + 7) `unsafeShiftR` 3
{-# INLINE packedBytes #-}

-- | Build a 'LabelInfo' from a list of 'LabelForm' values in
-- label order.  Internal-only; not re-exported by the public
-- API.  A 'LabelInfo' is normally produced by the parser, not
-- constructed by hand; the caller is expected to honour the DNS
-- 127-label cap, since the stored count is a 'Word8'.
mkLabelInfo :: [LabelForm] -> LabelInfo
mkLabelInfo forms =
    let !n = length forms
    in LabelInfo (fromIntegral n) (packForms n forms)

-- | Pack a list of 'LabelForm' tags into a 'ShortByteString',
-- three bits per label, byte-LSB-first.
packForms :: Int -> [LabelForm] -> ShortByteString
packForms !n forms = runST do
    let !nb = packedBytes n
    mba <- newByteArray nb
    setByteArray mba 0 nb (0 :: Word8)
    let loop !_ []     = pure ()
        loop !i (f:fs) = do
            writeForm mba i f
            loop (i + 1) fs
    loop 0 forms
    frozen <- unsafeFreezeByteArray mba
    pure (baToShortByteString frozen)

-- | Write a single 'LabelForm' at logical position @i@ into the
-- mutable buffer.  Assumes the buffer is zero-initialised at
-- bit positions @[i*3, i*3+3)@ and any straddled high byte; the
-- write OR-merges the three bits in.
writeForm :: forall s. MutableByteArray s -> Int -> LabelForm -> ST s ()
writeForm mba i form = do
    let !w      = unLabelForm form .&. 0x07
        !bitPos = i * 3
        !byteIx = bitPos `unsafeShiftR` 3
        !bitOff = bitPos .&. 7
    curLo <- readByteArray @Word8 mba byteIx
    writeByteArray mba byteIx (curLo .|. (w `unsafeShiftL` bitOff))
    when (bitOff > 5) $ do
        let !nextIx = byteIx + 1
        curHi <- readByteArray @Word8 mba nextIx
        writeByteArray mba nextIx
            (curHi .|. (w `unsafeShiftR` (8 - bitOff)))

-- | Read the three bits at logical position @i@ from the packed
-- buffer.  Assumes @0 <= i < count@.
readForm :: ShortByteString -> Int -> Word8
readForm sbs i =
    let !bitPos = i * 3
        !byteIx = bitPos `unsafeShiftR` 3
        !bitOff = bitPos .&. 7
        !lo     = SBS.index sbs byteIx
        !raw
          | bitOff <= 5 = lo `unsafeShiftR` bitOff
          | otherwise   =
              let !hi = SBS.index sbs (byteIx + 1)
              in (lo `unsafeShiftR` bitOff)
                 .|. (hi `unsafeShiftL` (8 - bitOff))
    in raw .&. 0x07
{-# INLINE readForm #-}

-- | The list of 'LabelForm' values, in input label order.
-- Returns an empty list for a bare-root 'LabelInfo'.  Lazy:
-- callers may consume a prefix without forcing the rest.
getLabelForms :: LabelInfo -> [LabelForm]
getLabelForms (LabelInfo n sbs) =
    [ wordLabelForm (readForm sbs i) | i <- [0 .. fromIntegral n - 1] ]

-- | The number of labels.  Zero for the bare root; capped at
-- 127 in practice (the DNS limit on labels per name).
getLabelFormCount :: LabelInfo -> Int
getLabelFormCount (LabelInfo n _) = fromIntegral n
{-# INLINE getLabelFormCount #-}

-- | Indexed access.  Returns 'NoLabel' for any index outside
-- @[0, 'getLabelFormCount' - 1]@, including all indices when
-- the count is zero (the bare-root case).
getLabelForm :: LabelInfo -> Int -> LabelForm
getLabelForm (LabelInfo n sbs) i
    | i < 0 || i >= fromIntegral n = NoLabel
    | otherwise                    = wordLabelForm (readForm sbs i)
{-# INLINE getLabelForm #-}

-- | Does every 'LabelForm' in the 'LabelInfo' belong to the
-- given 'LabelFormSet'?
--
-- Returns 'True' for a bare-root 'LabelInfo' against any
-- set, including 'mempty': the empty list of labels has no
-- elements to violate any restriction (vacuous truth).
-- Applications that need to reject the root should check
-- 'getLabelFormCount' separately.
meetsLabelFormSet :: LabelInfo -> LabelFormSet -> Bool
meetsLabelFormSet info allowed =
    all (`LFS.memberLabelFormSet` allowed) (getLabelForms info)
