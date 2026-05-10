-- |
-- Module      : Text.IDNA2008.Internal.Bidi
-- Description : Typed wrapper over the Bidi_Class table consumed by
--               the per-label RFC 5893 check in the IDNA parser.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- One lookup, 'bidiClassCp', backed by binary search over the
-- generated range table in "Text.IDNA2008.Internal.Bidi.Data".
-- The table covers every codepoint in @[0, 0x10FFFF]@ with no gaps,
-- so 'bidiClassCp' is a total function.
--
-- The eleven classes named in RFC 5893 (L, R, AL, AN, EN, ES, CS,
-- ET, ON, BN, NSM) get their own pattern synonyms; everything else
-- (paragraph\/segment\/whitespace separators, embedding\/override
-- and isolate format characters) is reported as 'BidiOther', which
-- the parser treats as a Bidi-rule violation in any label where it
-- appears.
{-# LANGUAGE CPP #-}

module Text.IDNA2008.Internal.Bidi
    ( -- * Class lookup
      bidiClassCp
      -- * Per-label summary
    , BidiSummary
    , emptyBidiSummary
    , extendBidiSummary
    , buildBidiSummary
    , bidiSummaryFromCps
      -- * Cross-label trigger fold
    , addBidiTrigger
      -- * Rule check
    , checkBidiSummary
    ) where

import Control.Monad.ST (ST)
import Data.Bits ((.&.), (.|.), bit, testBit, unsafeShiftL, unsafeShiftR)
#if !MIN_VERSION_base(4,20,0)
import Data.Foldable(foldl')
#endif
import Data.Primitive.ByteArray (indexByteArray)
import Data.Primitive.PrimArray
    ( MutablePrimArray
    , readPrimArray
    )
import Data.Word (Word8, Word16, Word32)

import Text.IDNA2008.Internal.Bidi.Data
    ( bidiRangeCount
    , bidiRangeStarts
    , bidiRangeTags
    )
import Text.IDNA2008.Internal.Error (BidiRuleViolation(..))

----------------------------------------------------------------------
-- Class tag
----------------------------------------------------------------------

-- | Resolved Unicode @Bidi_Class@ for the purposes of RFC 5893.
-- Stored as a 'Word8' tag in @[0, 11]@; pattern synonyms below
-- expose the named classes.  Tag 11 ('BidiOther') aggregates the
-- classes that RFC 5893 forbids unconditionally (B, S, WS, the
-- embedding\/override\/isolate format characters), so any label
-- containing one of them fails the rule check.
newtype BidiClass = BidiClass_ Word8 deriving Eq

-- The patterns below cover the classes the parser inspects by
-- name; the underlying type also represents tags 5..9 for the
-- neutral classes (ES, CS, ET, ON, BN) that RFC 5893 permits in
-- both LTR and RTL labels but which never trigger a rule
-- decision -- they flow through the validation code via @_@
-- catch-alls.  Naming them as patterns would add nothing the
-- parser can use, so the tags stay unnamed.
pattern BidiL     :: BidiClass
pattern BidiL     = BidiClass_ 0    -- ^ Left_To_Right
pattern BidiR     :: BidiClass
pattern BidiR     = BidiClass_ 1    -- ^ Right_To_Left
pattern BidiAL    :: BidiClass
pattern BidiAL    = BidiClass_ 2    -- ^ Arabic_Letter
pattern BidiAN    :: BidiClass
pattern BidiAN    = BidiClass_ 3    -- ^ Arabic_Number
pattern BidiEN    :: BidiClass
pattern BidiEN    = BidiClass_ 4    -- ^ European_Number
pattern BidiNSM   :: BidiClass
pattern BidiNSM   = BidiClass_ 10   -- ^ Nonspacing_Mark
pattern BidiOther :: BidiClass
pattern BidiOther = BidiClass_ 11   -- ^ everything else: B, S, WS,
                                    --   embedding\/override\/isolate
                                    --   format characters.  Forbidden
                                    --   in any Bidi-rule-checked label.

-- | Wrap a 'Word8' tag from the generated table.  Tags @0..10@ map
-- to the named classes; anything else collapses to 'BidiOther' on
-- the principle of conservative classification (would-be-failing
-- values fail).
{-# INLINE word8ToClass #-}
word8ToClass :: Word8 -> BidiClass
word8ToClass w
    | w <= 10   = BidiClass_ w
    | otherwise = BidiOther

----------------------------------------------------------------------
-- Public lookup
----------------------------------------------------------------------

-- | Return the resolved 'BidiClass' of a codepoint.  Out-of-range
-- inputs (negative or greater than @0x10FFFF@) report 'BidiOther',
-- which causes any label containing them to fail the Bidi check.
bidiClassCp :: Int -> BidiClass
bidiClassCp !cp
    | cp < 0 || cp > 0x10FFFF = BidiOther
    | otherwise               = word8ToClass (lookupTag cp)
{-# INLINE bidiClassCp #-}

----------------------------------------------------------------------
-- Binary search over the codegen'd range table
----------------------------------------------------------------------

-- | Binary-search for the largest index @i@ in
-- @[0, bidiRangeCount - 1]@ such that @bidiRangeStarts[i] <= cp@.
-- Returns the tag at that index.  Pre-condition: the table starts
-- at @0x000000@, so a hit is guaranteed for any @cp >= 0@.
{-# INLINE lookupTag #-}
lookupTag :: Int -> Word8
lookupTag !cp = go 0 (bidiRangeCount - 1)
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

    readStart i = indexByteArray bidiRangeStarts i :: Word32
    readTag   i = indexByteArray bidiRangeTags   i :: Word8

----------------------------------------------------------------------
-- Per-label summary
----------------------------------------------------------------------

-- | Per-label Bidi summary, packed into a single 'Word16':
--
-- @
--   bits  0..3 : first-codepoint class    (0..11)
--   bits  4..7 : last non-NSM class       (0..11)
--   bit      8 : has any L
--   bit      9 : has any R or AL
--   bit     10 : has any AN
--   bit     11 : has any EN
--   bit     12 : has any Other
-- @
--
-- Captures everything Rules 1-6 of RFC 5893 need to decide a
-- label's verdict without retaining the codepoint sequence.
-- Accumulated by left-fold via 'extendBidiSummary' starting from
-- 'emptyBidiSummary'.  The bit layout is internal — inspect via
-- the accessor functions ('bsFirstClass', 'bsLastNonNSM',
-- 'bsHasL', 'bsHasRorAL', 'bsHasAN', 'bsHasEN', 'bsHasOther').
newtype BidiSummary = BidiSummary_ Word16 deriving Eq

-- Bit indices for the presence flags within a 'BidiSummary's
-- 'Word16'.  The low byte stores the first / last class slots
-- (bits 0..3 and 4..7); presence flags live above that.
bsHasLIdx, bsHasRorALIdx, bsHasANIdx, bsHasENIdx, bsHasOtherIdx :: Int
bsHasLIdx     = 8
bsHasRorALIdx = 9
bsHasANIdx    = 10
bsHasENIdx    = 11
bsHasOtherIdx = 12

-- | Class of the first codepoint.  Rule 1 requires this to be
-- 'BidiL', 'BidiR', or 'BidiAL'.
bsFirstClass :: BidiSummary -> BidiClass
bsFirstClass (BidiSummary_ w) = BidiClass_ (fromIntegral (w .&. 0x000F))
{-# INLINE bsFirstClass #-}

-- | Class of the last codepoint that is not 'BidiNSM'.  Rules 3
-- and 6 examine this to decide if the label ends correctly past
-- any trailing combining marks.
bsLastNonNSM :: BidiSummary -> BidiClass
bsLastNonNSM (BidiSummary_ w) =
    BidiClass_ (fromIntegral ((w `unsafeShiftR` 4) .&. 0x000F))
{-# INLINE bsLastNonNSM #-}

bsHasL, bsHasRorAL, bsHasAN, bsHasEN, bsHasOther :: BidiSummary -> Bool
bsHasL     (BidiSummary_ w) = testBit w bsHasLIdx
bsHasRorAL (BidiSummary_ w) = testBit w bsHasRorALIdx
bsHasAN    (BidiSummary_ w) = testBit w bsHasANIdx
bsHasEN    (BidiSummary_ w) = testBit w bsHasENIdx
bsHasOther (BidiSummary_ w) = testBit w bsHasOtherIdx
{-# INLINE bsHasL #-}
{-# INLINE bsHasRorAL #-}
{-# INLINE bsHasAN #-}
{-# INLINE bsHasEN #-}
{-# INLINE bsHasOther #-}

-- | OR a per-label summary's cross-label trigger contribution
-- into a running flag.  Designed for left-fold over a label
-- sequence: @'addBidiTrigger' trig sm@ is 'True' if either @trig@
-- was already 'True' or @sm@ contains an @R@, @AL@, or @AN@
-- codepoint — the conditions under which RFC 5893 fires
-- cross-label Rules 1-6.  Encapsulates the bit-level details of
-- which classes trigger global checks so callers don't need to
-- import the per-flag predicates.
addBidiTrigger :: Bool -> BidiSummary -> Bool
addBidiTrigger !trig (BidiSummary_ w) =
    trig || testBit w bsHasRorALIdx || testBit w bsHasANIdx
{-# INLINE addBidiTrigger #-}

-- | The summary of an empty (zero-codepoint) label.  Encodes
-- 'BidiOther' in both first and last slots and clears every
-- presence flag, so any rule check on it fails Rule 1 cleanly.
-- Empty labels shouldn't normally reach a per-label Bidi check
-- (the parser rejects them earlier) but the constant gives
-- 'extendBidiSummary' something to fold over.
--
-- The empty / initial-state predicate is exactly
-- @(w `.&.` 0xFF00) == 0 && (w `.&.` 0x000F) == 11@, which
-- 'extendBidiSummary' uses to distinguish the first update from
-- subsequent updates.  Encoded value: @(BidiOther << 4) |
-- BidiOther = 0xBB@.
emptyBidiSummary :: BidiSummary
emptyBidiSummary = BidiSummary_ 0x00BB
{-# INLINE emptyBidiSummary #-}

-- | Map a 'BidiClass' to the presence-flag bit it contributes
-- (zero for classes that don't carry a flag: ES, CS, ET, ON,
-- BN, NSM).
classFlagBit :: BidiClass -> Word16
classFlagBit BidiL     = bit bsHasLIdx
classFlagBit BidiR     = bit bsHasRorALIdx
classFlagBit BidiAL    = bit bsHasRorALIdx
classFlagBit BidiAN    = bit bsHasANIdx
classFlagBit BidiEN    = bit bsHasENIdx
classFlagBit BidiOther = bit bsHasOtherIdx
classFlagBit _         = 0
{-# INLINE classFlagBit #-}

-- | Update a 'BidiSummary' with one more codepoint's class.
-- Designed for left-fold over a sequence of codepoints starting
-- from 'emptyBidiSummary'.
extendBidiSummary :: BidiSummary -> BidiClass -> BidiSummary
extendBidiSummary !(BidiSummary_ w) !c@(BidiClass_ cw) =
    let !cw16 = fromIntegral cw :: Word16
        !flag = classFlagBit c
    in if (w .&. 0xFF00) == 0 && (w .&. 0x000F) == 11
         -- Initial state: first slot still BidiOther, no flags
         -- set.  Seed both first and last slots with this
         -- codepoint's class, plus its flag bit.
         then BidiSummary_ (cw16 .|. (cw16 `unsafeShiftL` 4) .|. flag)
         -- Subsequent update: always OR in the flag bit; update
         -- the last-non-NSM slot only if the codepoint is not
         -- NSM.
         else case c of
             BidiNSM -> BidiSummary_ (w .|. flag)
             _       -> BidiSummary_
                 ((w .&. 0xFF0F) .|. (cw16 `unsafeShiftL` 4) .|. flag)
{-# INLINE extendBidiSummary #-}

-- | Walk @cpBuf[0..cnt)@ accumulating a 'BidiSummary'.  Used by
-- the parser-time per-label check (which already holds a
-- 'MutablePrimArray' of decoded codepoints).
buildBidiSummary
    :: forall s. MutablePrimArray s Int -> Int -> ST s BidiSummary
buildBidiSummary !cpBuf !cnt = go 0 emptyBidiSummary
  where
    go :: Int -> BidiSummary -> ST s BidiSummary
    go !i !s
      | i >= cnt  = pure s
      | otherwise = do
          cp <- readPrimArray cpBuf i
          go (i + 1) (extendBidiSummary s (bidiClassCp cp))
{-# INLINE buildBidiSummary #-}

-- | Pure 'BidiSummary' computation over an arbitrary sequence of
-- codepoints; used by the presentation-time global check, which
-- consumes already-decoded U-label codepoints.
bidiSummaryFromCps :: [Int] -> BidiSummary
bidiSummaryFromCps =
    foldl' (\ !s !cp -> extendBidiSummary s (bidiClassCp cp))
           emptyBidiSummary
{-# INLINE bidiSummaryFromCps #-}

----------------------------------------------------------------------
-- Rule check
----------------------------------------------------------------------

-- | Validate a 'BidiSummary' against the per-label conditions of
-- RFC 5893 Rules 1-6.  Returns 'Nothing' if the label is clean,
-- or the offending 'BidiRuleViolation' otherwise.
--
-- The first 'Bool' argument controls the trigger short-circuit:
--
--   * 'False' (per-label mode) -- only check labels whose own
--     codepoints contain an @R@, @AL@, or @AN@ codepoint.
--     Labels without any such content (pure-LTR labels) always
--     return 'Nothing'.  This is the right semantic for the
--     parser, which validates each label in isolation.
--   * 'True' (global mode) -- check every label unconditionally.
--     This is the right semantic for the cross-label
--     presentation-time check, which fires Rules 1-6 on every
--     label of a domain once any label has RTL content.
--
-- In either mode, the rule application itself is identical:
-- Rule 1, then either the RTL trio (Rules 2 \/ 3 \/ 4) or the
-- LTR pair (Rules 5 \/ 6) depending on the first-codepoint
-- direction.  An out-of-set codepoint (anything @BidiOther@)
-- fails the corresponding allowed-set rule (2 in RTL, 5 in LTR).
checkBidiSummary :: Bool -> BidiSummary -> Maybe BidiRuleViolation
checkBidiSummary !forceCheck !s
    -- Per-label trigger: skip clean LTR-only labels unless we're
    -- in global mode.
    | not forceCheck
    , not (bsHasRorAL s || bsHasAN s) = Nothing
    -- Rule 1: first must be L, R, or AL.
    | first /= BidiL
    , first /= BidiR
    , first /= BidiAL = Just BidiRule1FirstNotLRAL
    | first == BidiL =
        -- LTR rules 5 and 6.  Either a forbidden codepoint class
        -- or a wrong-direction trailing class fails.
        if bsHasRorAL s || bsHasAN s || bsHasOther s
          then Just BidiRule5LTRDisallowed
          else case bsLastNonNSM s of
              BidiL  -> Nothing
              BidiEN -> Nothing
              _      -> Just BidiRule6LTRBadEnd
    | otherwise =
        -- 'first' is BidiR or BidiAL.  RTL rules 2, 3, 4.
        if bsHasL s || bsHasOther s
          then Just BidiRule2RTLDisallowed
          else if bsHasEN s && bsHasAN s
            then Just BidiRule4ENANMix
            else case bsLastNonNSM s of
                BidiR  -> Nothing
                BidiAL -> Nothing
                BidiEN -> Nothing
                BidiAN -> Nothing
                _      -> Just BidiRule3RTLBadEnd
  where
    !first = bsFirstClass s
{-# INLINE checkBidiSummary #-}
