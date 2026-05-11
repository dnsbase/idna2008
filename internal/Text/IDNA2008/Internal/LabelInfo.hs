-- |
-- Module      : Text.IDNA2008.Internal.LabelInfo
-- Description : Per-label classification result.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- A 'LabelInfo' is the parser's per-label classification result,
-- paired with the wire-form @Domain@ in the @parseDomain*@ return
-- tuple.  Opaque; inspect via the accessors:
--
--   * 'getLabelForms' returns the list of forms in label order.
--   * 'getLabelFormCount' returns the number of labels.
--   * 'getLabelForm' indexes the array; out-of-range indices
--     (including any index for the zero-label root domain's
--     'LabelInfo') return 'NoLabel'.
--   * 'allLabelFormsIn' asks whether every label belongs to a
--     given 'LabelFormSet'.
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
    , allLabelFormsIn
    ) where

import qualified Data.ByteString.Short as SBS
import Control.Monad.ST (ST, runST)
import Data.Array.Byte (ByteArray(..))
import Data.Bits ((.|.), (.&.), unsafeShiftL, unsafeShiftR)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short (ShortByteString(SBS))
import Data.Primitive.ByteArray
    ( MutableByteArray
    , newByteArray
    , readByteArray
    , setByteArray
    , unsafeFreezeByteArray
    , writeByteArray
    )
import Data.Word (Word8)

import Text.IDNA2008.Internal.LabelForm ( LabelForm(..) )
import qualified Text.IDNA2008.Internal.LabelFormSet as LFS
import Text.IDNA2008.Internal.LabelFormSet (LabelFormSet)

-- | The parser's per-label classification result.  Opaque;
-- inspect via the accessors.
data LabelInfo = LabelInfo
    {-# UNPACK #-} !Word8
    {-# UNPACK #-} !ShortByteString

instance Show LabelInfo where
    show info = show (getLabelForms info)

-- | Number of packed bytes needed to hold @n@ four-bit values.
packedBytes :: Int -> Int
packedBytes !n = (n + 1) `unsafeShiftR` 1
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
-- one nibble per label.
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
    (ByteArray frozen) <- unsafeFreezeByteArray mba
    pure (SBS frozen)

-- | Write a single 'LabelForm' at logical position @i@ into the
-- mutable buffer.  Assumes the buffer is zero-initialised at
-- the relevant nibble; the write OR-merges the four bits in.
-- Even indices land in the low nibble of byte @i/2@; odd
-- indices in the high nibble.
writeForm :: forall s. MutableByteArray s -> Int -> LabelForm -> ST s ()
writeForm mba i (LabelForm_ form) = do
    let !w      = form .&. 0x0F
        !byteIx = i `unsafeShiftR` 1
        !nibOff = (i .&. 1) `unsafeShiftL` 2  -- 0 (low) or 4 (high)
    cur <- readByteArray @Word8 mba byteIx
    writeByteArray mba byteIx (cur .|. (w `unsafeShiftL` nibOff))

-- | Read the four bits at logical position @i@ from the packed
-- buffer.  Assumes @0 <= i < count@.
readForm :: ShortByteString -> Int -> Word8
readForm sbs i =
    let !byteIx = i `unsafeShiftR` 1
        !nibOff = (i .&. 1) `unsafeShiftL` 2  -- 0 (low) or 4 (high)
        !byte   = SBS.index sbs byteIx
    in (byte `unsafeShiftR` nibOff) .&. 0x0F
{-# INLINE readForm #-}

-- | The list of 'LabelForm' values, in input label order.
-- Returns an empty list for the root domain's 'LabelInfo'.
-- Lazy: callers may consume a prefix without forcing the rest.
getLabelForms :: LabelInfo -> [LabelForm]
getLabelForms (LabelInfo n sbs) =
    [ LabelForm_ (readForm sbs i) | i <- [0 .. fromIntegral n - 1] ]

-- | The number of labels.  Zero for the root domain; capped at
-- 127 in practice (the DNS limit on labels per name).
getLabelFormCount :: LabelInfo -> Int
getLabelFormCount (LabelInfo n _) = fromIntegral n
{-# INLINE getLabelFormCount #-}

-- | Indexed access.  Returns 'NoLabel' for any index outside
-- @[0, 'getLabelFormCount' - 1]@, including all indices when
-- the count is zero (the root domain).
getLabelForm :: LabelInfo -> Int -> LabelForm
getLabelForm (LabelInfo n sbs) i
    | i < 0 || i >= fromIntegral n = NoLabel
    | otherwise                    = LabelForm_ (readForm sbs i)
{-# INLINE getLabelForm #-}

-- | Does every label's form match the given 'LabelFormSet'?
--
-- Vacuously returns 'True' for the root domain's 'LabelInfo'.
-- Applications that want to reject the root domain can check the
-- value of 'getLabelFormCount', or check whether the label list
-- returned by 'Text.IDNA2008.toLabels' is 'null'.
allLabelFormsIn :: LabelInfo -- ^ The per-label forms
                -> LabelFormSet -- ^ The target label form set
                -> Bool
allLabelFormsIn info allowed =
    all (`LFS.memberLabelFormSet` allowed) (getLabelForms info)
