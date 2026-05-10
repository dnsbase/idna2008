-- |
-- Module      : Text.IDNA2008.Internal.NFC
-- Description : Unicode NFC validation for IDNA U-labels
--               (RFC 5891 section 5.3).
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Two layers:
--
--   * 'nfcQuickCheck' / 'NfcQc' -- the Unicode @NFC_Quick_Check@
--     property as a trichotomy.  Cheap; the typical label has
--     every codepoint @NfcYes@ and the validator can short-circuit.
--
--   * 'isNFC' -- the precise validator.  Runs the QC fast path
--     first; on any non-@Yes@ codepoint it falls through to a
--     full normalize-and-compare using
--     "Text.IDNA2008.Internal.NFC.Tables" (canonical
--     decomposition + reorder + primary composition, including
--     the Hangul algorithmic short-circuit).
{-# LANGUAGE BangPatterns #-}

module Text.IDNA2008.Internal.NFC
    ( -- * Quick_Check trichotomy
      NfcQc(..)
    , nfcQuickCheck

      -- * Full NFC validation
    , isNFC

      -- * In-place NFC normalisation
    , normalizeNFC
    ) where

import Control.Monad.ST (ST)
import Control.Monad (when)
import Data.Primitive.PrimArray
    ( MutablePrimArray
    , copyMutablePrimArray
    , newPrimArray
    , readPrimArray
    , writePrimArray
    )

import Text.IDNA2008.Internal.Ranges (inRanges)
import Text.IDNA2008.Internal.NFC.Data
    ( nfcNoRangeCount
    , nfcNoRanges
    , nfcMaybeRangeCount
    , nfcMaybeRanges
    )
import Text.IDNA2008.Internal.NFC.Tables
    ( canonCompose
    , canonDecompose
    , cccOf
    , hangulCompose
    , hangulDecompose
    )

----------------------------------------------------------------------
-- Quick_Check
----------------------------------------------------------------------

-- | Three-valued result of the Unicode @NFC_Quick_Check@
-- property.  See <https://www.unicode.org/reports/tr15/ UAX #15>.
data NfcQc
    = NfcYes    -- ^ Codepoint is unconditionally in NFC.
    | NfcNo     -- ^ Codepoint is never in NFC; an NFC
                --   normaliser would always replace it.
    | NfcMaybe  -- ^ Codepoint may or may not be in NFC depending
                --   on its neighbours; deciding requires running
                --   the full normalisation algorithm.
    deriving (Eq, Show)

-- | Look up the @NFC_Quick_Check@ value for the given codepoint.
-- Codepoints absent from both range tables are 'NfcYes'.
nfcQuickCheck :: Int -> NfcQc
nfcQuickCheck !cp
    | inRanges nfcNoRanges    nfcNoRangeCount    cp = NfcNo
    | inRanges nfcMaybeRanges nfcMaybeRangeCount cp = NfcMaybe
    | otherwise                                     = NfcYes
{-# INLINE nfcQuickCheck #-}

----------------------------------------------------------------------
-- Full validator
----------------------------------------------------------------------

-- | Decide precisely whether the codepoint sequence
-- @cpBuf[0..cnt)@ is in Normalization Form C.
--
-- Strategy: a Quick_Check pass first.  If every codepoint is
-- 'NfcYes', the input is in NFC.  On the first non-@Yes@
-- codepoint we fall through to a full normalize-and-compare:
-- canonical decomposition (recursive, including Hangul
-- algorithmic decomposition) into a working buffer; in-place
-- canonical reorder by 'cccOf' (insertion sort within
-- combining-mark runs); in-place primary composition
-- (canonical-blocking rule, plus Hangul algorithmic
-- composition); compare the result to the input.
isNFC :: forall s. MutablePrimArray s Int -> Int -> ST s Bool
isNFC !cpBuf !cnt = do
    qcYes <- allYes cpBuf cnt 0
    if qcYes
      then pure True
      else do
        let !workCap = cnt * 4 + 32
        workBuf <- newPrimArray workCap
        nDecomp  <- decomposeInto cpBuf cnt workBuf
        reorderRun workBuf nDecomp
        nCompose <- composeRun workBuf nDecomp
        if nCompose /= cnt
          then pure False
          else equalRuns cpBuf workBuf cnt

-- | Normalise the codepoint buffer @cpBuf[0..cnt)@ to NFC, in
-- place, and return the new length.  Reuses the same decompose /
-- reorder / compose pipeline as 'isNFC', then copies the composed
-- run back to @cpBuf@.
--
-- The caller's @cpBuf@ must have room for the result.  In practice
-- NFC composition does not increase the codepoint count beyond
-- the input length, but the caller should size the buffer for the
-- worst case if it cannot guarantee that.
--
-- Fast path: if every input codepoint passes the @NFC_Quick_Check@
-- @Yes@ test, the buffer is already in NFC and the function
-- returns @cnt@ unchanged with no work done.
normalizeNFC
    :: forall s. MutablePrimArray s Int -> Int -> ST s Int
normalizeNFC !cpBuf !cnt = do
    qcYes <- allYes cpBuf cnt 0
    if qcYes
      then pure cnt
      else do
        let !workCap = cnt * 4 + 32
        workBuf <- newPrimArray workCap
        nDecomp  <- decomposeInto cpBuf cnt workBuf
        reorderRun workBuf nDecomp
        nCompose <- composeRun workBuf nDecomp
        when (nCompose > 0) $
            copyMutablePrimArray cpBuf 0 workBuf 0 nCompose
        pure nCompose

-- | True iff every codepoint in @cpBuf[0..cnt)@ passes the
-- @NFC_Quick_Check@ @Yes@ test (sufficient condition for NFC,
-- not necessary).
allYes :: forall s. MutablePrimArray s Int -> Int -> Int -> ST s Bool
allYes !cpBuf !cnt = go
  where
    go :: Int -> ST s Bool
    go !i
      | i >= cnt = pure True
      | otherwise = do
          cp <- readPrimArray cpBuf i
          case nfcQuickCheck cp of
            NfcYes -> go (i + 1)
            _      -> pure False

-- | Walk the input buffer and write each codepoint's full
-- canonical decomposition into the output buffer.  Returns the
-- number of codepoints written.
decomposeInto
    :: forall s
    .  MutablePrimArray s Int       -- input
    -> Int                          -- input length
    -> MutablePrimArray s Int       -- output (must have room)
    -> ST s Int
decomposeInto !inBuf !cnt !outBuf = go 0 0
  where
    go :: Int -> Int -> ST s Int
    go !inIdx !outIdx
      | inIdx >= cnt = pure outIdx
      | otherwise = do
          cp <- readPrimArray inBuf inIdx
          outIdx' <- decomposeCp cp outBuf outIdx
          go (inIdx + 1) outIdx'

-- | Recursively expand @cp@'s canonical decomposition into the
-- output buffer at @outIdx@.  Returns the next free output index.
decomposeCp
    :: forall s. Int -> MutablePrimArray s Int -> Int -> ST s Int
decomposeCp !cp !outBuf !outIdx
    | Just (l, v, mt) <- hangulDecompose cp = do
        writePrimArray outBuf outIdx       l
        writePrimArray outBuf (outIdx + 1) v
        case mt of
          Nothing -> pure (outIdx + 2)
          Just t  -> do
            writePrimArray outBuf (outIdx + 2) t
            pure (outIdx + 3)
    | Just (a, b) <- canonDecompose cp = do
        outIdx' <- decomposeCp a outBuf outIdx
        if b == 0
          then pure outIdx'
          else decomposeCp b outBuf outIdx'
    | otherwise = do
        writePrimArray outBuf outIdx cp
        pure (outIdx + 1)

-- | Canonical reorder: walk the buffer, and within every run of
-- consecutive codepoints whose CCC is non-zero, sort by CCC
-- ascending using insertion sort (which is naturally stable).
-- Codepoints with CCC=0 act as boundaries — no reordering
-- crosses them.
reorderRun :: forall s. MutablePrimArray s Int -> Int -> ST s ()
reorderRun !buf !n = go 1
  where
    -- Insertion-sort step: at index i, bubble the element backwards
    -- as long as its CCC is non-zero AND strictly less than the
    -- previous element's non-zero CCC.
    go :: Int -> ST s ()
    go !i
      | i >= n = pure ()
      | otherwise = do
          cur <- readPrimArray buf i
          let !curCCC = fromIntegral (cccOf cur) :: Int
          if curCCC == 0
            then go (i + 1)
            else do
              bubble i curCCC
              go (i + 1)

    bubble :: Int -> Int -> ST s ()
    bubble !i !curCCC
      | i == 0 = pure ()
      | otherwise = do
          prev <- readPrimArray buf (i - 1)
          let !prevCCC = fromIntegral (cccOf prev) :: Int
          if prevCCC /= 0 && prevCCC > curCCC
            then do
              cur <- readPrimArray buf i
              writePrimArray buf (i - 1) cur
              writePrimArray buf i prev
              bubble (i - 1) curCCC
            else pure ()

-- | In-place primary composition over a canonically-ordered
-- buffer.  Walks @[0, n)@ and emits the composed form into
-- @[0, returnedLength)@.  The walk maintains:
--
--   * @starterIdx@: index in the output of the most recently
--     emitted starter, or @-1@ if none yet.
--   * @lastCCC@: the CCC of the most recent /non-starter/ that
--     has been emitted /since/ that starter; used to determine
--     whether a subsequent non-starter is blocked from composing.
--
-- For each input codepoint:
--
--   * If it is a non-starter (@CCC > 0@) and not blocked
--     (@lastCCC < ccc@), and a primary composition with the
--     output starter exists (canonical or Hangul), the starter
--     is replaced by the composed codepoint and the input is
--     consumed.  Else the input is emitted; @lastCCC@ is updated.
--
--   * If it is a starter (@CCC = 0@), and the previous output
--     starter is unblocked (@lastCCC == 0@), and a primary
--     composition exists (this is the Hangul L+V \/ LV+T case),
--     the starter is replaced and the input is consumed.  Else
--     the input becomes the new output starter, and @lastCCC@
--     resets to @0@.
composeRun :: forall s. MutablePrimArray s Int -> Int -> ST s Int
composeRun !buf !n = go 0 0 (-1) 0
  where
    go :: Int -> Int -> Int -> Int -> ST s Int
    go !inIdx !outIdx !starterIdx !lastCCC
      | inIdx >= n = pure outIdx
      | otherwise = do
          cp <- readPrimArray buf inIdx
          let !cpCCC = fromIntegral (cccOf cp) :: Int
          if cpCCC == 0
            then handleStarter cp inIdx outIdx starterIdx lastCCC
            else handleNonStarter cp cpCCC inIdx outIdx starterIdx lastCCC

    handleStarter
      :: Int -> Int -> Int -> Int -> Int -> ST s Int
    handleStarter !cp !inIdx !outIdx !starterIdx !lastCCC
      | starterIdx >= 0 && lastCCC == 0 = do
          starter <- readPrimArray buf starterIdx
          case composePair starter cp of
            Just composed -> do
              writePrimArray buf starterIdx composed
              go (inIdx + 1) outIdx starterIdx 0
            Nothing -> emitStarter cp inIdx outIdx
      | otherwise = emitStarter cp inIdx outIdx

    emitStarter :: Int -> Int -> Int -> ST s Int
    emitStarter !cp !inIdx !outIdx = do
        writePrimArray buf outIdx cp
        go (inIdx + 1) (outIdx + 1) outIdx 0

    handleNonStarter
      :: Int -> Int -> Int -> Int -> Int -> Int -> ST s Int
    handleNonStarter !cp !cpCCC !inIdx !outIdx !starterIdx !lastCCC
      | starterIdx >= 0 && lastCCC < cpCCC = do
          starter <- readPrimArray buf starterIdx
          case composePair starter cp of
            Just composed -> do
              writePrimArray buf starterIdx composed
              go (inIdx + 1) outIdx starterIdx lastCCC
            Nothing -> emitNonStarter cp cpCCC inIdx outIdx starterIdx
      | otherwise = emitNonStarter cp cpCCC inIdx outIdx starterIdx

    emitNonStarter
      :: Int -> Int -> Int -> Int -> Int -> ST s Int
    emitNonStarter !cp !cpCCC !inIdx !outIdx !starterIdx = do
        writePrimArray buf outIdx cp
        go (inIdx + 1) (outIdx + 1) starterIdx cpCCC

-- | Try Hangul algorithmic composition first; fall back to the
-- canonical composition table.
composePair :: Int -> Int -> Maybe Int
composePair !a !b = case hangulCompose a b of
    Just c  -> Just c
    Nothing -> canonCompose a b
{-# INLINE composePair #-}

-- | Codepoint-by-codepoint equality of @a[0..n)@ and @b[0..n)@.
equalRuns
    :: forall s
    .  MutablePrimArray s Int
    -> MutablePrimArray s Int
    -> Int
    -> ST s Bool
equalRuns !a !b !n = go 0
  where
    go :: Int -> ST s Bool
    go !i
      | i >= n = pure True
      | otherwise = do
          x <- readPrimArray a i
          y <- readPrimArray b i
          if x == y then go (i + 1) else pure False

