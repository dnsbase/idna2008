-- |
-- Module      : Text.IDNA2008.Internal.Punycode
-- Description : RFC 3492 Punycode encoder and decoder
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- A self-contained implementation of RFC 3492 Punycode used by
-- IDNA A-label \(\Leftrightarrow\) U-label conversion.  The
-- encoder and decoder both write into caller-supplied mutable
-- buffers; no intermediate storage is allocated, and arithmetic
-- is performed in machine 'Int' with explicit overflow checks.
--
-- Inputs:
--
--   * The encoder consumes a 'PrimArray' of codepoint values
--     (each in @0..0x10FFFF@, surrogates excluded — the caller
--     is responsible for enforcing that) and writes the
--     corresponding base-36 ACE bytes into a 'MutableByteArray'.
--     The output does /not/ include the @\"xn--\"@ prefix;
--     callers add that.
--
--   * The decoder consumes a slice of an immutable 'ByteArray'
--     (the ACE body, /not/ including @\"xn--\"@) and writes the
--     recovered codepoints into a 'MutablePrimArray'.
--
-- Both routines return the number of elements written, or a
-- 'PunycodeErr' on failure.
module Text.IDNA2008.Internal.Punycode
    ( PunycodeErr(..)
    , punycodeEncode
    , punycodeDecode
    ) where

import Control.Monad.ST (ST)
import Data.Bits (unsafeShiftR)
import Data.Primitive.ByteArray
    ( ByteArray
    , MutableByteArray
    , indexByteArray
    , writeByteArray
    )
import Data.Primitive.PrimArray
    ( MutablePrimArray
    , PrimArray
    , indexPrimArray
    , readPrimArray
    , writePrimArray
    )
import Data.Word (Word8)

-- | Errors that can be produced by the encoder or decoder.
data PunycodeErr
    = PunycodeOverflow
      -- ^ Internal Punycode arithmetic exceeded the representable range.
    | PunycodeBadDigit
      -- ^ Decoder encountered a non-base-36 byte where a digit was required,
      -- or a non-basic byte in the basic prefix.
    | PunycodeTruncated
      -- ^ Decoder ran out of input mid-integer.
    | PunycodeOutputFull
      -- ^ The caller-provided output buffer is too small for the result.
    deriving (Eq, Show)

----------------------------------------------------------------------
-- RFC 3492 constants
----------------------------------------------------------------------

pBase, pTmin, pTmax, pSkew, pDamp, pInitialBias, pInitialN :: Int
pBase        =  36
pTmin        =   1
pTmax        =  26
pSkew        =  38
pDamp        = 700
pInitialBias =  72
pInitialN    = 0x80

-- | Threshold used by 'adapt': @((base - tmin) * tmax) / 2@.
adaptThreshold :: Int
adaptThreshold = ((pBase - pTmin) * pTmax) `unsafeShiftR` 1

-- | Maximum 'Int' value, used in the encoder to find the smallest
-- non-basic codepoint not yet processed in a given round.
maxInt :: Int
maxInt = maxBound

----------------------------------------------------------------------
-- Basic-codepoint <-> digit conversions
----------------------------------------------------------------------

-- | Map a base-36 digit value (0..35) to its ASCII byte.
--
-- @0..25@  -> @\'a\'..\'z\'@ (0x61..0x7a)
--
-- @26..35@ -> @\'0\'..\'9\'@ (0x30..0x39)
{-# INLINE digitToBasic #-}
digitToBasic :: Int -> Word8
digitToBasic d
    | d < 26    = fromIntegral (d + 0x61)
    | otherwise = fromIntegral (d + 22)   -- 26 + 22 = 0x30

-- | Map an ASCII byte to its base-36 digit value, or @-1@ if the byte is
-- not a base-36 digit.  Both upper- and lower-case ASCII letters are
-- accepted (case-insensitive on input, per RFC 3492 section 5).
{-# INLINE basicToDigit #-}
basicToDigit :: Word8 -> Int
basicToDigit w
    | l <- w - 0x61
    , l < 26 = fromIntegral l
    | d <- w - 0x30
    , d <= 9 = fromIntegral d + 26
    | l <- w - 0x41
    , l < 26 = fromIntegral l
    | otherwise            = -1

-- | Bias adaptation, RFC 3492 section 6.1.
adapt :: Int -> Int -> Bool -> Int
adapt d0 numpoints firstTime =
    let !d1 = if firstTime then d0 `quot` pDamp else d0 `unsafeShiftR` 1
        !d2 = d1 + d1 `quot` numpoints
    in adaptLoop d2 0
  where
    adaptLoop !d !k
        | d > adaptThreshold
            = adaptLoop (d `quot` (pBase - pTmin)) (k + pBase)
        | otherwise
            = k + ((pBase - pTmin + 1) * d) `quot` (d + pSkew)
{-# INLINE adapt #-}

----------------------------------------------------------------------
-- Encoder
----------------------------------------------------------------------

-- | Encode a sequence of codepoints to Punycode ASCII bytes (without the
-- @\"xn--\"@ prefix).  On success returns the number of bytes written.
punycodeEncode
    :: forall s
    .  PrimArray Int          -- ^ Input codepoints
    -> Int                    -- ^ Number of valid codepoints (@>= 0@)
    -> MutableByteArray s     -- ^ Output buffer
    -> Int                    -- ^ Output start offset
    -> Int                    -- ^ Output capacity in bytes
    -> ST s (Either PunycodeErr Int)
punycodeEncode !codepoints !inLen !outBuf !outStart !outCap
    | inLen <= 0 = pure (Right 0)
    | otherwise  = do
        -- Pass 1: emit basic (ASCII) codepoints in input order.
        r1 <- emitBasics 0 outStart 0
        case r1 of
          Left e -> pure (Left e)
          Right (off1, b)
            | b == 0
              -- No basics: omit the delimiter (RFC 3492 section
              -- 6.3: \"copy [basic code points] in order,
              -- followed by a delimiter if b > 0\").
              -> mainLoop pInitialN 0 pInitialBias 0 off1 True
            | b == inLen -> do
                -- All-basic input: emit a trailing delimiter and
                -- skip the main loop.  RFC 3492 section 6.3
                -- emits the delimiter whenever @b > 0@ -- including
                -- the all-basic case, even though there's no
                -- extension that follows.  IDNA doesn't exercise
                -- this branch in production (the U-label path
                -- requires at least one non-ASCII codepoint
                -- before calling the encoder), but the RFC
                -- 3492 conformance vectors -- e.g.  section 7.1
                -- example (S) @\"-> $1.00 <-\"@ -- pin this
                -- behaviour down, and the decoder requires it
                -- for round-tripping.
                r2 <- writeOne off1 0x2d
                case r2 of
                  Left e     -> pure (Left e)
                  Right off2 -> pure (Right (off2 - outStart))
            | otherwise -> do
                r2 <- writeOne off1 0x2d
                case r2 of
                  Left e     -> pure (Left e)
                  Right off2 -> mainLoop pInitialN 0 pInitialBias b off2 True
  where
    !endOff = outStart + outCap

    writeOne :: Int -> Word8 -> ST s (Either PunycodeErr Int)
    writeOne !off !w
        | off >= endOff = pure (Left PunycodeOutputFull)
        | otherwise = do
            writeByteArray outBuf off w
            pure (Right $! off + 1)
    {-# INLINE writeOne #-}

    -- Emit basic (ASCII < 0x80) codepoints in their original order; ignore
    -- non-basic codepoints in this pass.  Returns (new offset, basic count)
    -- or an output-full error.
    emitBasics :: Int -> Int -> Int -> ST s (Either PunycodeErr (Int, Int))
    emitBasics !i !off !b
        | i >= inLen = pure (Right (off, b))
        | otherwise =
            let !c = indexPrimArray codepoints i
            in if c < pInitialN
                 then if off >= endOff
                        then pure (Left PunycodeOutputFull)
                        else do
                          writeByteArray outBuf off (fromIntegral c :: Word8)
                          emitBasics (i+1) (off+1) (b+1)
                 else emitBasics (i+1) off b

    -- Main encoder loop, RFC 3492 section 6.3.  Each iteration handles one new
    -- "round" of non-basic codepoints sharing the same minimum value m.
    mainLoop :: Int   -- ^ n: current minimum codepoint
             -> Int   -- ^ delta: current delta value
             -> Int   -- ^ bias
             -> Int   -- ^ h: codepoints emitted so far
             -> Int   -- ^ output offset
             -> Bool  -- ^ firstTime flag for adapt
             -> ST s (Either PunycodeErr Int)
    mainLoop !n !delta !bias !h !off !firstTime
        | h >= inLen = pure (Right (off - outStart))
        | otherwise =
            let !m   = findMin n
                !mn  = m - n
                !hp1 = h + 1
            in if mn /= 0 && hp1 /= 0 && mn > maxInt `quot` hp1
                 then pure (Left PunycodeOverflow)
                 else
                    let !inc    = mn * hp1
                        !delta' = delta + inc
                    in if delta' < delta
                         then pure (Left PunycodeOverflow)
                         else processCp 0 m delta' bias h off firstTime

    -- Find the smallest codepoint in the input that is >= n.  Returns
    -- maxInt if none exists (shouldn't happen while h < inLen).
    findMin :: Int -> Int
    findMin !n0 = go 0 maxInt
      where
        go !i !m
          | i >= inLen = m
          | otherwise =
              let !c = indexPrimArray codepoints i
                  !m' = if c >= n0 && c < m then c else m
              in go (i+1) m'

    -- Inner per-codepoint loop, RFC 3492 section 6.3 step 3.  For each input
    -- codepoint c: bump delta if c < m, emit a varint and adapt if c == m.
    processCp :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
              -> ST s (Either PunycodeErr Int)
    processCp !i !m !delta !bias !h !off !firstTime
        | i >= inLen
          -- End of pass: bump delta, bump n, recurse to next round.
          = let !delta1 = delta + 1
            in if delta1 < delta
                 then pure (Left PunycodeOverflow)
                 else mainLoop (m + 1) delta1 bias h off firstTime
        | otherwise = do
            let !c = indexPrimArray codepoints i
            case compare c m of
              LT -> let !d' = delta + 1
                    in if d' < delta
                         then pure (Left PunycodeOverflow)
                         else processCp (i+1) m d' bias h off firstTime
              EQ -> do
                  rEmit <- emitVarint delta bias off
                  case rEmit of
                    Left e -> pure (Left e)
                    Right off' ->
                      let !bias' = adapt delta (h + 1) firstTime
                      in processCp (i+1) m 0 bias' (h+1) off' False
              GT -> processCp (i+1) m delta bias h off firstTime

    -- Emit the variable-length base-36 encoding of @q@ with the given bias.
    emitVarint :: Int -> Int -> Int -> ST s (Either PunycodeErr Int)
    emitVarint !q0 !bias !off0 = go q0 pBase off0
      where
        go !q !k !off =
            let !t = if k <= bias + pTmin then pTmin
                     else if k >= bias + pTmax then pTmax
                     else k - bias
            in if q < t
                 then writeOne off (digitToBasic q)
                 else do
                    let !d  = t + (q - t) `mod` (pBase - t)
                        !qn = (q - t) `quot` (pBase - t)
                    rW <- writeOne off (digitToBasic d)
                    case rW of
                      Left e     -> pure (Left e)
                      Right off' -> go qn (k + pBase) off'

----------------------------------------------------------------------
-- Decoder
----------------------------------------------------------------------

-- | Decode a Punycode ASCII body (no @\"xn--\"@ prefix) into
-- a sequence of codepoints, written into the caller-supplied
-- mutable codepoint buffer.  Returns the number of codepoints
-- produced.
--
-- The decoder accepts both upper- and lower-case base-36 digits;
-- callers performing strict A-label round-trip checks should
-- compare the /lower-cased/ input ACE bytes against a freshly
-- encoded form.
punycodeDecode
    :: forall s
    .  ByteArray                  -- ^ Input ASCII bytes (Punycode body)
    -> Int                        -- ^ Input start offset
    -> Int                        -- ^ Input length
    -> MutablePrimArray s Int     -- ^ Output codepoint buffer
    -> Int                        -- ^ Output start offset
    -> Int                        -- ^ Output capacity (codepoints)
    -> ST s (Either PunycodeErr Int)
punycodeDecode !inBuf !inStart !inLen !outBuf !outStart !outCap
    | inLen < 0 = pure (Left PunycodeTruncated)
    | otherwise = do
        -- Locate the last delimiter '-': everything before it is basic,
        -- everything after is base-36 encoded.  If no '-' is present, the
        -- whole input is encoded (no basic prefix).
        let !delimAt = lastDelim (inEnd - 1)
        if delimAt < inStart
          then -- No delimiter: whole input is base-36.
               decodeLoop pInitialN pInitialBias 0 outStart 0 True inStart
          else do
            -- Copy basics [inStart .. delimAt-1] verbatim as codepoints.
            r <- copyBasics inStart (delimAt - 1) outStart 0
            case r of
              Left e -> pure (Left e)
              Right b
                | b == 0    ->
                    -- RFC 3492: do not consume the delimiter when no basics
                    -- were copied; this means the input is malformed because
                    -- the next byte is the delimiter, which is not a digit.
                    decodeLoop pInitialN pInitialBias 0 outStart 0 True delimAt
                | otherwise ->
                    decodeLoop pInitialN pInitialBias 0 (outStart + b) b
                               True (delimAt + 1)
  where
    !inEnd  = inStart + inLen
    !outEnd = outStart + outCap

    -- Find the index of the last '-' in [inStart .. p], or inStart-1 if
    -- there isn't one.
    lastDelim :: Int -> Int
    lastDelim !p
        | p < inStart = inStart - 1
        | (indexByteArray inBuf p :: Word8) == 0x2d = p
        | otherwise = lastDelim (p - 1)

    -- Copy [from..toIncl] of basics into the output buffer.  Returns the
    -- number of codepoints written, or PunycodeOutputFull / PunycodeBadDigit
    -- on error.
    copyBasics :: Int -> Int -> Int -> Int
               -> ST s (Either PunycodeErr Int)
    copyBasics !i !iEnd !o !count
        | i > iEnd = pure (Right count)
        | o >= outEnd = pure (Left PunycodeOutputFull)
        | otherwise = do
            let !w = indexByteArray inBuf i :: Word8
            if w >= 0x80
              then pure (Left PunycodeBadDigit)
              else do
                writePrimArray outBuf o (fromIntegral w :: Int)
                copyBasics (i + 1) iEnd (o + 1) (count + 1)

    -- Main decode loop, RFC 3492 section 6.2.
    decodeLoop
        :: Int   -- ^ n: current codepoint base
        -> Int   -- ^ bias
        -> Int   -- ^ i: insertion-position accumulator (RFC 'i')
        -> Int   -- ^ output offset (== outStart + count)
        -> Int   -- ^ count of codepoints written so far
        -> Bool  -- ^ firstTime flag for adapt
        -> Int   -- ^ input offset
        -> ST s (Either PunycodeErr Int)
    decodeLoop !n !bias !i !out !count !firstTime !ip
        | ip >= inEnd =
            pure (Right (out - outStart))
        | otherwise = do
            r <- readVarint i bias 1 pBase ip
            case r of
              Left e            -> pure (Left e)
              Right (i', ip')   -> do
                let !numpts = count + 1
                    !bias'  = adapt (i' - i) numpts firstTime
                    !n'     = n + i' `quot` numpts
                    !pos    = i' `mod` numpts
                if n' > 0x10FFFF
                  then pure (Left PunycodeOverflow)
                  else if out >= outEnd
                    then pure (Left PunycodeOutputFull)
                    else do
                      shiftRight (outStart + pos) (out - 1)
                      writePrimArray outBuf (outStart + pos) n'
                      decodeLoop n' bias' (pos + 1) (out + 1)
                                 (count + 1) False ip'

    -- Read one base-36 variable-length integer starting at input offset 'ip',
    -- accumulating into 'i'.  Returns (new value of i, input offset after
    -- the integer's last digit) or an error.
    readVarint :: Int -> Int -> Int -> Int -> Int
               -> ST s (Either PunycodeErr (Int, Int))
    readVarint !i !bias !w !k !ip
        | ip >= inEnd = pure (Left PunycodeTruncated)
        | otherwise =
            let !digit = basicToDigit (indexByteArray inBuf ip)
            in if digit < 0
                 then pure (Left PunycodeBadDigit)
                 else
                   let !contribution = digit * w
                   in if w /= 0 && contribution `quot` w /= digit
                        then pure (Left PunycodeOverflow)
                        else
                          let !i' = i + contribution
                          in if i' < i
                               then pure (Left PunycodeOverflow)
                               else
                                 let !t = if k <= bias + pTmin then pTmin
                                          else if k >= bias + pTmax then pTmax
                                          else k - bias
                                 in if digit < t
                                      then pure (Right (i', ip + 1))
                                      else
                                        let !w' = w * (pBase - t)
                                        in if pBase - t /= 0
                                              && w' `quot` (pBase - t) /= w
                                             then pure (Left PunycodeOverflow)
                                             else readVarint i' bias w'
                                                              (k + pBase)
                                                              (ip + 1)

    -- Shift codepoints in [low .. lastIdx] one slot to the right.  Walks
    -- backwards so we don't trample data we still need to read.
    shiftRight :: Int -> Int -> ST s ()
    shiftRight !low !lastIdx = go lastIdx
      where
        go !p
          | p < low   = pure ()
          | otherwise = do
              !v <- readPrimArray outBuf p
              writePrimArray outBuf (p + 1) v
              go (p - 1)
