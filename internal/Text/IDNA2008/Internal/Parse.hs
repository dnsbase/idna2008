-- |
-- Module      : Text.IDNA2008.Internal.Parse
-- Description : IDNA-aware parser from presentation Text/UTF-8 to wire form
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Top-level parser that walks a UTF-8 byte view of a presentation-form
-- domain name, splits it into labels (handling DNS-style backslash
-- escapes), classifies each label into one of the 'LabelForm' singletons,
-- and writes the wire form into a freshly allocated 'Domain'.
--
-- The parser shares a single output buffer and a single per-call
-- codepoint accumulator across all labels; allocations are constant in
-- the number of labels.
--
-- IDNA validation: each U-label codepoint is looked up in the IANA
-- IDNA2008 derived-property table (see "Text.IDNA2008.Internal.Property")
-- and rejected unless its disposition is @PVALID@.  @CONTEXTO@
-- codepoints are admitted when 'checkContextO' accepts their context
-- (all of RFC 5892 Appendix A.3-A.9 are implemented).  @CONTEXTJ@
-- codepoints are admitted when 'checkContextJ' accepts their context
-- (RFC 5892 Appendix A.1\/A.2).
--
-- With the 'NFCCHECK' flag, checks that the decoded codepoints are in NFC
-- (composed) form.  With the 'BIDICHECK' flag, adds the per-label subset of
-- RFC 5893 Rules 1-6; the cross-label part of the rule set is treated as a
-- presentation-time concern and intentionally not enforced in the parser.
--
-- Classification precedence: any codepoint @> 0xFF@ forces the U-label
-- path (since such a codepoint cannot fit in an octet); otherwise, any
-- non-LDH ASCII byte prefers the OCTET path (since the resulting
-- Punycode-encoded form would not be a valid LDH A-label); pure
-- Latin-1 input (codepoints @0x80..0xFF@ with no non-LDH ASCII) still
-- goes through the U-label path.
--
-- A-label classification: under 'ALABELCHECK' (set in
-- 'defaultIdnaFlags'), an ACE-prefixed LDH label is reported
-- as 'ALABEL' only if its Punycode body decodes to a valid IDN label
-- and re-encodes to the same input bytes; on failure the label is
-- 'FAKEA' (when 'FAKEA' is in the caller's 'LabelFormSet') or
-- rejected with the underlying 'AceReason'.  Without 'ALABELCHECK',
-- every syntactically valid ACE-prefixed LDH label is reported
-- as 'ALABEL'.
--
-- U-label classification: a non-ASCII label that passes strict
-- IDNA2008 validation is reported as 'ULABEL'.  When strict
-- validation fails (disposition, contextual rule, NFC, hyphen,
-- joining, or Bidi), the label is reported as 'LAXULABEL' if
-- 'LAXULABEL' is in the caller's 'LabelFormSet', else rejected.
-- 'LAXULABEL' and 'FAKEA' are the permissive companions to
-- 'ULABEL' and 'ALABEL': they admit labels that fail strict
-- validation in their respective input shapes.
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.IDNA2008.Internal.Parse
    ( -- * Domain type
      Domain(Domain)
    , isValidWireForm
    , toLabels
    , wireBytes
    , wireBytesShort

      -- * Domain parsers
    , parseDomain
    , parseDomainOpts
    , parseDomainUtf8
    , parseDomainShort

      -- * Domain helpers (default options)
    , mkDomain
    , mkDomainStr
    , mkDomainUtf8
    , mkDomainShort

      -- * Literal domains (TH splices)
    , dnLit

      -- * Domain display forms
    , domainToAscii
    , domainToUnicode
    , labelToAscii
    , labelToUnicode
    , unparseDomainOpts
    , unparseLabelOpts
    ) where

import qualified Data.ByteString.Short as SBS
import qualified Data.Primitive.ByteArray as A
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.IDNA2008.Internal.Case as Case
import Text.IDNA2008.Internal.UTS46 (uts46Lookup)
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Array.Byte (ByteArray(..))
import Data.Bits ((.|.), (.&.), testBit, unsafeShiftL, unsafeShiftR)
import Data.ByteString (ByteString)
import Data.ByteString.Short.Internal (ShortByteString(..))
import Data.Char (chr, ord)
import Data.Foldable (foldMap')
import Data.Primitive.ByteArray
    ( MutableByteArray
    , copyMutableByteArray
    , indexByteArray
    , newByteArray
    , readByteArray
    , resizeMutableByteArray
    , shrinkMutableByteArray
    , unsafeFreezeByteArray
    , writeByteArray
    )
import Data.Primitive.PrimArray
    ( MutablePrimArray
    , PrimArray
    , indexPrimArray
    , newPrimArray
    , readPrimArray
    , sizeofPrimArray
    , unsafeFreezePrimArray
    , writePrimArray
    )
import Data.Text.Internal (Text(Text))
import Data.Word (Word8, Word64)

import Text.IDNA2008.Internal.Bidi
    ( BidiSummary
    , addBidiTrigger
    , bidiClassCp
    , buildBidiSummary
    , checkBidiSummary
    , emptyBidiSummary
    , extendBidiSummary
    )
import Text.IDNA2008.Internal.Combining (isCombiningMark)
import Text.IDNA2008.Internal.Flags
    ( IdnaFlags(..)
    , defaultIdnaFlags
    , effectiveIdnaFlags
    , meetsIdnaFlags
    , withoutIdnaFlags
    )
import Text.IDNA2008.Internal.Emoji (isEmoji, isMappedEmoji)
import Text.IDNA2008.Internal.Error
import Text.IDNA2008.Internal.LabelForm
    ( LabelForm
    , pattern LDH, pattern RLDH, pattern FAKEA, pattern ALABEL
    , pattern ULABEL, pattern LAXULABEL, pattern ATTRLEAF, pattern OCTET
    , pattern WILDLABEL
    )
import Text.IDNA2008.Internal.LabelFormSet
    ( LabelFormSet, labelFormToSet, memberLabelFormSet
    , withoutLabelFormSet, idnLabelForms )
import Text.IDNA2008.Internal.LabelInfo
    ( LabelInfo, mkLabelInfo )
import Text.IDNA2008.Internal.Property
    ( IdnaDisposition(..), idnaDisposition )
import Text.IDNA2008.Internal.Joining
    ( isVirama
    , jtIsLeftOrDual
    , jtIsRightOrDual
    , jtIsTransparent
    )
import Text.IDNA2008.Internal.NFC (isNFC, normalizeNFC)
import Text.IDNA2008.Internal.Punycode
import Text.IDNA2008.Internal.Script
    ( isGreekCp
    , isHebrewCp
    , isHkhCp
    )
import Text.IDNA2008.Internal.Width (widthMapCp)

----------------------------------------------------------------------
-- Buffer sizes
----------------------------------------------------------------------

-- | Maximum wire form length, RFC 1035 section 3.1.
--
-- Encoding of domain names allocates a buffer of this size
-- shrinking it to the actual size used when done.
maxWireLen :: Int
maxWireLen = 255

-- | Maximum wire octets in a single label.
maxLabelLen :: Int
maxLabelLen = 63

-- | Maximum codepoints we admit in a single label before encoding.
--
-- This is the also the size of the mutable codepoint array given
-- to the Punycode decoder.  The number of decoded codepoints
-- can't exceed the number of input bytes.
maxCpsPerLabel :: Int
maxCpsPerLabel = maxLabelLen

-- | Output buffer capacity.
outBufSize :: Int
outBufSize = maxWireLen + 1

----------------------------------------------------------------------
-- Per-label statistics (computed once at flush time from cpBuf).
----------------------------------------------------------------------

data LabelStat = LabelStat
    { lsHasGt0xFF      :: !Bool   -- any codepoint > 0xFF
    , lsHasNonAsciiCp  :: !Bool   -- any codepoint > 0x7F
    , lsAllLdh         :: !Bool   -- every codepoint is LDH-ASCII
    , lsLdhPastFirst   :: !Bool   -- every codepoint at index >= 1 is LDH-ASCII
    , lsHasNonLdhAscii :: !Bool   -- any codepoint < 0x80 that is non-LDH
    , lsLeadingHyphen  :: !Bool
    , lsTrailingHyphen :: !Bool
    , lsHy34           :: !Bool   -- '-' at positions 3 AND 4
    , lsLeadingUnder   :: !Bool   -- first codepoint is '_'
    , lsXnPrefix       :: !Bool   -- first two cps are 'x'/'X', 'n'/'N'
    , lsIsWild         :: !Bool   -- the entire label is the single byte '*'
    }

----------------------------------------------------------------------
-- Public entry points
----------------------------------------------------------------------

-- | Parse a 'Text' as a presentation-form domain name with the
-- strict default parser options ('defaultIdnaFlags').  See
-- 'parseDomainOpts' to override the flag set.
parseDomain
    :: LabelFormSet                  -- ^ Permitted label forms
    -> Text                          -- ^ Presentation form
    -> Either IdnaError (Domain, LabelInfo)
parseDomain !allowed = parseDomainOpts allowed defaultIdnaFlags

-- | Like 'parseDomain' but with a caller-supplied 'IdnaFlags' record.
parseDomainOpts
    :: LabelFormSet                  -- ^ Permitted label forms
    -> IdnaFlags                     -- ^ Parser options
    -> Text                          -- ^ Presentation form
    -> Either IdnaError (Domain, LabelInfo)
parseDomainOpts !allowed !opts (Text arr off len) =
    parseDomainView (textArrayToByteArray arr) off len allowed opts
  where
    textArrayToByteArray :: TA.Array -> ByteArray
    textArrayToByteArray (TA.ByteArray b) = ByteArray b

-- | Parse a UTF-8 'ByteString' as a presentation-form domain name.
-- Ill-formed UTF-8 yields 'ErrInvalidUtf8'.
parseDomainUtf8
    :: LabelFormSet                  -- ^ Permitted label forms
    -> IdnaFlags                     -- ^ Parser options
    -> ByteString                    -- ^ UTF-8 presentation form
    -> Either IdnaError (Domain, LabelInfo)
parseDomainUtf8 !allowed !flags !bs =
    parseDomainShort allowed flags $! SBS.toShort bs

-- | Parse a UTF-8 'ShortByteString' as a presentation-form domain
-- name.  Ill-formed UTF-8 yields 'ErrInvalidUtf8'.
parseDomainShort
    :: LabelFormSet                  -- ^ Permitted label forms
    -> IdnaFlags                     -- ^ Parser options
    -> ShortByteString               -- ^ UTF-8 presentation form
    -> Either IdnaError (Domain, LabelInfo)
parseDomainShort !allowed !flags sb@(SBS sba) =
    parseDomainView (ByteArray sba) 0 (SBS.length sb) allowed flags

-- | Core view-based entry point.  Returns the parsed 'Domain' and a
-- per-label 'LabelInfo' classification result; fails if the input is
-- malformed or if any label's classification is not in the @allowed@
-- set.
parseDomainView
    :: ByteArray                   -- ^ Input UTF-8 bytes
    -> Int                         -- ^ Slice start
    -> Int                         -- ^ Slice length
    -> LabelFormSet                 -- ^ Permitted label forms
    -> IdnaFlags
    -> Either IdnaError (Domain, LabelInfo)
parseDomainView !input !inOff !inLen !allowed !opts0
    | inLen < 0 = Left (ErrInvalidUtf8 (-1) Nothing)
    | otherwise = runST do
        outBuf <- newByteArray outBufSize
        cpBuf  <- newPrimArray maxCpsPerLabel
        let !inEnd = inOff + inLen
            !opts  = effectiveIdnaFlags opts0
        res <- driver input cpBuf outBuf inEnd allowed opts
                      inOff 0 0 0 False 0 []
        case res of
          Left e -> pure (Left e)
          Right (outLen, produced) -> do
            let !finalLen = outLen + 1
            if finalLen > maxWireLen
              then pure (Left (ErrNameTooLong finalLen))
              else do
                writeByteArray outBuf outLen (0 :: Word8)
                resBA <- newByteArray finalLen
                copyMutableByteArray resBA 0 outBuf 0 finalLen
                (ByteArray frozen) <- unsafeFreezeByteArray resBA
                let !info = mkLabelInfo (reverse produced)
                -- The parser builds the wire form by construction
                -- (length bytes capped at 63, trailing NUL appended,
                -- total length <= maxWireLen).  Use the bare data
                -- constructor to skip the redundant validation that
                -- the 'Domain' pattern would perform.
                pure (Right (Domain_ (SBS frozen), info))

----------------------------------------------------------------------
-- Driver
----------------------------------------------------------------------

-- | Codepoints that the 'MAPDOTS' option treats as additional label
-- separators alongside @U+002E@.  See 'MAPDOTS' for the rationale.
isExtraDotCp :: Int -> Bool
isExtraDotCp !cp =
       cp == 0x3002    -- IDEOGRAPHIC FULL STOP
    || cp == 0xFF0E    -- FULLWIDTH FULL STOP
    || cp == 0xFF61    -- HALFWIDTH IDEOGRAPHIC FULL STOP
{-# INLINE isExtraDotCp #-}

driver
    :: ByteArray
    -> MutablePrimArray s Int
    -> MutableByteArray s
    -> Int                          -- inEnd
    -> LabelFormSet                  -- allowed
    -> IdnaFlags
    -> Int                          -- iPos
    -> Int                          -- oPos
    -> Int                          -- lStart
    -> Int                          -- cpCount
    -> Bool                         -- hasEscape
    -> Int                          -- lIdx
    -> [LabelForm]                   -- producedForms
    -> ST s (Either IdnaError (Int, [LabelForm]))
driver !input !cpBuf !outBuf !inEnd !allowed !opts = go
  where
    go !iPos !oPos !lStart !cpCount !hasEscape !lIdx !produced
      | iPos >= inEnd =
          if cpCount == 0
            then pure (Right (oPos, produced))
            else flushLabel input cpBuf outBuf allowed opts
                            lStart cpCount hasEscape lIdx produced
      | otherwise = do
          let !b = indexByteArray input iPos :: Word8
          if | b == 0x5C ->
                 handleEscape iPos oPos lStart cpCount lIdx produced
             | b == 0x2E ->
                 handleDot (iPos + 1) oPos lStart cpCount
                           hasEscape lIdx produced
             | b < 0x80 ->
                 appendCp (fromIntegral b :: Int) (iPos + 1)
                          oPos lStart cpCount hasEscape lIdx produced
             | otherwise ->
                 case decodeUtf8At input inEnd iPos of
                   Left e -> pure (Left e)
                   Right (cp, nextPos)
                     | MAPDOTS `meetsIdnaFlags` opts, isExtraDotCp cp ->
                         handleDot nextPos oPos lStart cpCount
                                   hasEscape lIdx produced
                     | otherwise ->
                         appendCp cp nextPos oPos lStart cpCount
                                  hasEscape lIdx produced

    appendCp !cp !iPos !oPos !lStart !cpCount !hasEscape !lIdx !produced
      | cpCount >= maxCpsPerLabel =
          pure (Left (ErrLabelTooLong lIdx (cpCount + 1)))
      | otherwise = do
          writePrimArray cpBuf cpCount cp
          go iPos oPos lStart (cpCount + 1) hasEscape lIdx produced

    handleEscape !iPos !oPos !lStart !cpCount !lIdx !produced
      | iPos + 1 >= inEnd =
          pure (Left (ErrBadEscape lIdx (Just iPos)))
      | otherwise =
          let !b1 = indexByteArray input (iPos + 1) :: Word8
          in case asciiDigit b1 of
               Just !v1
                 -- \DDD numeric escape
                 | iPos + 4 > inEnd ->
                     pure (Left (ErrBadEscape lIdx (Just iPos)))
                 | otherwise ->
                     let !b2 = indexByteArray input (iPos + 2) :: Word8
                         !b3 = indexByteArray input (iPos + 3) :: Word8
                     in case (asciiDigit b2, asciiDigit b3) of
                          (Just v2, Just v3) ->
                            let !v =   100 * fromIntegral v1
                                    +   10 * fromIntegral v2
                                    +        fromIntegral v3 :: Int
                            in if v > 0xFF
                                 then pure (Left (ErrBadEscape lIdx (Just iPos)))
                                 else
                                    appendCp v (iPos + 4) oPos lStart cpCount
                                             True lIdx produced
                          _ -> pure (Left (ErrBadEscape lIdx (Just iPos)))
               Nothing
                 | b1 < 0x80 ->
                     appendCp (fromIntegral b1) (iPos + 2)
                              oPos lStart cpCount True lIdx produced
                 | otherwise ->
                     case decodeUtf8At input inEnd (iPos + 1) of
                       Left e -> pure (Left e)
                       Right (cp, nextPos)
                         | cp > 0xFF ->
                             pure (Left (ErrBadEscape lIdx (Just iPos)))
                         | otherwise ->
                             appendCp cp nextPos oPos lStart cpCount True
                                      lIdx produced

    handleDot !nextPos !oPos !lStart !cpCount !hasEscape !lIdx !produced
      | cpCount == 0 =
          -- Empty label.  Always an error here, with one special case:
          -- the input is exactly a single separator codepoint, which
          -- represents the root domain.  We detect that as: no labels
          -- emitted yet (oPos == 0, lIdx == 0) and this separator is
          -- the last codepoint in the input.  Trailing separators
          -- after a real label are handled by the end-of-input branch
          -- (where cpCount has already been reset to 0 by a previous
          -- flush), so they never reach this function.
          if oPos == 0 && lIdx == 0 && nextPos == inEnd
            then pure (Right (oPos, produced))
            else pure (Left (ErrEmptyLabel lIdx))
      | otherwise = do
          r <- flushLabel input cpBuf outBuf allowed opts
                          lStart cpCount hasEscape lIdx produced
          case r of
            Left e -> pure (Left e)
            Right (oPos', produced') ->
              go nextPos oPos' oPos' 0 False (lIdx + 1) produced'

----------------------------------------------------------------------
-- UTF-8 decoder
----------------------------------------------------------------------

-- | Decode the UTF-8 sequence starting at byte offset @p@.  Returns
-- @(codepoint, advance)@ or an error.  The @lIdx@ for error location
-- isn't threaded through; we use @-1@ here since the byte offset alone
-- already pinpoints the issue.
decodeUtf8At
    :: ByteArray                    -- input
    -> Int                          -- inEnd
    -> Int                          -- p (offset to read)
    -> Either IdnaError (Int, Int)
decodeUtf8At !input !inEnd !p
    | p >= inEnd = Left (ErrInvalidUtf8 (-1) (Just p))
    | otherwise =
        let !b0 = indexByteArray input p :: Word8
        in if | b0 < 0x80 -> Right (fromIntegral b0, p + 1)
              | b0 < 0xC2 -> Left  (ErrInvalidUtf8 (-1) (Just p))
              | b0 < 0xE0 -> two p b0
              | b0 < 0xF0 -> three p b0
              | b0 < 0xF5 -> four p b0
              | otherwise -> Left  (ErrInvalidUtf8 (-1) (Just p))
  where
    contBad b = b < 0x80 || b >= 0xC0

    two !p0 !b0
        | p0 + 1 >= inEnd =
            Left (ErrInvalidUtf8 (-1) (Just p0))
        | otherwise =
            let !b1 = indexByteArray input (p0 + 1) :: Word8
            in if contBad b1
                 then Left (ErrInvalidUtf8 (-1) (Just p0))
                 else
                   let !cp = ((fromIntegral b0 - 0xC0) `unsafeShiftL` 6)
                          .|. (fromIntegral b1 - 0x80) :: Int
                   in Right (cp, p0 + 2)

    three !p0 !b0
        | p0 + 2 >= inEnd =
            Left (ErrInvalidUtf8 (-1) (Just p0))
        | otherwise =
            let !b1 = indexByteArray input (p0 + 1) :: Word8
                !b2 = indexByteArray input (p0 + 2) :: Word8
            in if contBad b1 || contBad b2
                 then Left (ErrInvalidUtf8 (-1) (Just p0))
                 else
                   let !cp = ((fromIntegral b0 - 0xE0) `unsafeShiftL` 12)
                          .|. ((fromIntegral b1 - 0x80) `unsafeShiftL` 6)
                          .|. (fromIntegral b2 - 0x80) :: Int
                   in if cp < 0x800 || (cp >= 0xD800 && cp <= 0xDFFF)
                        then Left (ErrInvalidUtf8 (-1) (Just p0))
                        else Right (cp, p0 + 3)

    four !p0 !b0
        | p0 + 3 >= inEnd =
            Left (ErrInvalidUtf8 (-1) (Just p0))
        | otherwise =
            let !b1 = indexByteArray input (p0 + 1) :: Word8
                !b2 = indexByteArray input (p0 + 2) :: Word8
                !b3 = indexByteArray input (p0 + 3) :: Word8
            in if contBad b1 || contBad b2 || contBad b3
                 then Left (ErrInvalidUtf8 (-1) (Just p0))
                 else
                   let !cp = ((fromIntegral b0 - 0xF0) `unsafeShiftL` 18)
                          .|. ((fromIntegral b1 - 0x80) `unsafeShiftL` 12)
                          .|. ((fromIntegral b2 - 0x80) `unsafeShiftL` 6)
                          .|. (fromIntegral b3 - 0x80) :: Int
                   in if cp < 0x10000 || cp > 0x10FFFF
                        then Left (ErrInvalidUtf8 (-1) (Just p0))
                        else Right (cp, p0 + 4)

----------------------------------------------------------------------
-- Label flushing: classify, validate, emit.
--
-- Decision tree, in order:
--
--   1. Escape produced any byte: OCTET (defensive: gt0xFF here is
--      impossible since \DDD is range-checked and \C requires cp <= 0xFF,
--      so a non-LDH ASCII byte that came from an escape simply lands
--      in OCTET).
--
--   2. Any codepoint > 0xFF: U-label path forced.  Such a codepoint
--      cannot fit in a single octet, so neither OCTET nor LDH is an
--      option; failure to validate as a U-label is the only way out.
--
--   3. All-LDH and not bordered by hyphens: LDH (or RLDH/ALABEL via
--      the hy34 + xn prefix discriminator).
--
--   4. Leading underscore with the rest LDH: ATTRLEAF.
--
--   5. Has non-ASCII codepoints (0x80..0xFF) AND no non-LDH ASCII
--      byte: U-label path.  Pure Latin-1 input.
--
--   6. Otherwise: OCTET.  Catches "non-LDH ASCII present but no
--      codepoint > 0xFF" -- a label that cannot form a valid A-label
--      and is byte-fittable, so OCTET semantics apply.
----------------------------------------------------------------------

flushLabel
    :: ByteArray                    -- input (unused; kept for future error context)
    -> MutablePrimArray s Int       -- cpBuf
    -> MutableByteArray s           -- outBuf
    -> LabelFormSet                  -- allowed
    -> IdnaFlags
    -> Int                          -- lStart
    -> Int                          -- cpCount
    -> Bool                         -- hasEscape
    -> Int                          -- lIdx
    -> [LabelForm]                   -- producedForms accumulator
    -> ST s (Either IdnaError (Int, [LabelForm]))
flushLabel !_input !cpBuf !outBuf !allowed !opts
           !lStart !cpCount !hasEscape !lIdx !produced
    | cpCount == 0 = pure (Left (ErrEmptyLabel lIdx))
    | otherwise = do
        -- RFC 5895 section 2.1 mapping (ASCII portion): apply unconditional
        -- lower-casing of A-Z to z+0x20 before classification.  This
        -- doesn't shift any label between forms (uppercase letters
        -- are already part of the LDH alphabet for classification
        -- purposes; cf. 'isLdhCp'), but it does ensure the wire form
        -- emits lowercase ASCII regardless of input case.  Unicode
        -- toLower for non-ASCII codepoints is applied later, only on
        -- the U-label path (see 'uLabelPath').
        when (MAPCASE `meetsIdnaFlags` opts)
             (asciiDownCaseBuf cpBuf cpCount)
        stat <- scanCpBuf cpBuf cpCount
        -- WILDLABEL is a property of the wire content (the single byte
        -- '*', 0x2A), not of the input spelling: '*', '\*', and '\042'
        -- all produce the same wire byte and all classify the same way.
        if lsIsWild stat
          then commitBytes cpBuf outBuf allowed
                           lStart cpCount lIdx produced WILDLABEL
          else if hasEscape
            then if lsHasGt0xFF stat
                   -- Escape syntax (signalling OCTET-form
                   -- intent) appears alongside codepoints >
                   -- 0xFF that an OCTET label can't carry.
                   -- The label has no single coherent
                   -- presentation form; see 'ErrUnpresentableLabel'.
                   then pure (Left (ErrUnpresentableLabel lIdx))
                   else commitBytes cpBuf outBuf allowed
                                    lStart cpCount lIdx produced OCTET
            else if lsHasGt0xFF stat
              then uLabelPath cpBuf outBuf allowed opts lStart cpCount lIdx
                              produced
              else if lsAllLdh stat
                      && not (lsLeadingHyphen stat || lsTrailingHyphen stat)
                then if lsHy34 stat
                       then ldhRldhPath cpBuf outBuf allowed opts
                                        lStart cpCount lIdx produced stat
                       else commitBytes cpBuf outBuf allowed
                                        lStart cpCount lIdx produced LDH
                else if lsLeadingUnder stat
                        && lsLdhPastFirst stat
                        && not (lsTrailingHyphen stat)
                  then commitBytes cpBuf outBuf allowed
                                   lStart cpCount lIdx produced ATTRLEAF
                  else if lsHasNonAsciiCp stat
                          && not (lsHasNonLdhAscii stat)
                    then uLabelPath cpBuf outBuf allowed opts lStart cpCount lIdx
                                    produced
                    else commitBytes cpBuf outBuf allowed
                                     lStart cpCount lIdx produced OCTET

-- | Scan cpBuf computing label characteristics in one pass.
scanCpBuf :: MutablePrimArray s Int -> Int -> ST s LabelStat
scanCpBuf !cpBuf !cnt = do
    base <- scanBody 0 (LabelStat False False True True False
                                  False False False False False False)
    -- Boundary-position flags.
    first <- if cnt >= 1 then readPrimArray cpBuf 0 else pure 0
    last_ <- if cnt >= 1 then readPrimArray cpBuf (cnt - 1) else pure 0
    p1    <- if cnt >= 2 then readPrimArray cpBuf 1 else pure 0
    p2    <- if cnt >= 4 then readPrimArray cpBuf 2 else pure 0
    p3    <- if cnt >= 4 then readPrimArray cpBuf 3 else pure 0
    let !leadHyp  = first == 0x2D
        !trailHyp = last_ == 0x2D
        !hy34     = cnt >= 4 && p2 == 0x2D && p3 == 0x2D
        !leadUnd  = first == 0x5F
        !xn       = cnt >= 2
                  && first .|. 0x20 == 0x78
                  && p1 .|. 0x20 == 0x6E
        !isWild   = cnt == 1 && first == 0x2A
    pure base { lsLeadingHyphen  = leadHyp
              , lsTrailingHyphen = trailHyp
              , lsHy34           = hy34
              , lsLeadingUnder   = leadUnd
              , lsXnPrefix       = xn
              , lsIsWild         = isWild
              }
  where
    scanBody !i !st
      | i >= cnt = pure st
      | otherwise = do
          cp <- readPrimArray cpBuf i
          let !pastFirst = i >= 1
              !st' =
                if | cp > 0xFF ->
                      st { lsHasGt0xFF      = True
                         , lsHasNonAsciiCp  = True
                         , lsAllLdh         = False
                         , lsLdhPastFirst   = lsLdhPastFirst st && not pastFirst
                         }
                   | cp >= 0x80 ->
                      st { lsHasNonAsciiCp  = True
                         , lsAllLdh         = False
                         , lsLdhPastFirst   = lsLdhPastFirst st && not pastFirst
                         }
                   | isLdhCp cp -> st
                   | otherwise ->
                      st { lsAllLdh         = False
                         , lsLdhPastFirst   = lsLdhPastFirst st && not pastFirst
                         , lsHasNonLdhAscii = True
                         }
          scanBody (i + 1) st'

-- | Copy cpBuf[0..cnt) as raw bytes into outBuf starting at lStart+1,
-- check forms / lengths / wire-form caps, write the length byte, and
-- return the new output offset.
commitBytes
    :: MutablePrimArray s Int
    -> MutableByteArray s
    -> LabelFormSet
    -> Int                  -- lStart
    -> Int                  -- cpCount
    -> Int                  -- lIdx
    -> [LabelForm]           -- produced (in)
    -> LabelForm             -- form to assign
    -> ST s (Either IdnaError (Int, [LabelForm]))
commitBytes !cpBuf !outBuf !allowed !lStart !cpCount !lIdx !produced !form
    | not (form `memberLabelFormSet` allowed) =
        pure (Left (ErrFormNotAllowed lIdx form))
    | otherwise =
        let !newOff = lStart + 1 + cpCount
        in if newOff >= maxWireLen
             then pure (Left (ErrNameTooLong (newOff + 1)))
             else do
               writeCpBufAsBytes cpBuf outBuf (lStart + 1) cpCount
               writeByteArray outBuf lStart (fromIntegral cpCount :: Word8)
               pure (Right (newOff, form : produced))

writeCpBufAsBytes
    :: MutablePrimArray s Int
    -> MutableByteArray s
    -> Int                  -- starting output offset
    -> Int                  -- count
    -> ST s ()
writeCpBufAsBytes !cpBuf !outBuf !off !cnt = go 0
  where
    go !i
      | i >= cnt = pure ()
      | otherwise = do
          cp <- readPrimArray cpBuf i
          writeByteArray outBuf (off + i) (fromIntegral cp :: Word8)
          go (i + 1)

----------------------------------------------------------------------
-- LDH / R-LDH / A-label decision.  Loose by default; strict round-
-- trip classification (FAKEA on Punycode failure or non-round-trip)
-- is opt-in via the 'ALABELCHECK' option.
----------------------------------------------------------------------

ldhRldhPath
    :: forall s
    .  MutablePrimArray s Int
    -> MutableByteArray s
    -> LabelFormSet
    -> IdnaFlags
    -> Int                  -- lStart
    -> Int                  -- cpCount
    -> Int                  -- lIdx
    -> [LabelForm]           -- produced
    -> LabelStat
    -> ST s (Either IdnaError (Int, [LabelForm]))
ldhRldhPath !cpBuf !outBuf !allowed !opts !lStart !cpCount !lIdx
            !produced !stat
    | not (lsXnPrefix stat) =
        commitBytes cpBuf outBuf allowed lStart cpCount lIdx produced RLDH
    | not (ALABELCHECK `meetsIdnaFlags` opts) =
        -- Loose: every well-formed xn-- LDH label is ALABEL without
        -- further checks.
        commitBytes cpBuf outBuf allowed lStart cpCount lIdx produced ALABEL
    | otherwise = do
        -- Strict: copy the Punycode body (cpBuf[4..cpCount)) into
        -- a fresh ByteArray, lower-casing any uppercase ASCII
        -- letters, then run alabelRoundTrip.  ALABEL on success;
        -- on failure, FAKEA when the caller allows it, else the
        -- specific 'AceReason' is surfaced via 'ErrAceInvalid'.
        -- The case-normalisation here ensures all case variants
        -- of the same ACE body decode identically through
        -- Punycode (which is case-sensitive on basic codepoints).
        let !bodyLen = cpCount - 4
        bodyBufM <- newByteArray bodyLen
        let copyLc !i
              | i >= bodyLen = pure ()
              | otherwise = do
                  cp <- readPrimArray cpBuf (4 + i)
                  let !b
                        | cp >= 0x41 && cp <= 0x5A =
                            fromIntegral (cp + 0x20)
                        | otherwise =
                            fromIntegral cp
                  writeByteArray bodyBufM i (b :: Word8)
                  copyLc (i + 1)
        copyLc 0
        body <- unsafeFreezeByteArray bodyBufM
        rt <- alabelRoundTrip body 0 bodyLen opts
        case rt of
          Right _ ->
              commitBytes cpBuf outBuf allowed lStart cpCount lIdx
                          produced ALABEL
          Left reason
              | FAKEA `memberLabelFormSet` allowed ->
                  commitBytes cpBuf outBuf allowed lStart cpCount lIdx
                              produced FAKEA
              | otherwise ->
                  pure (Left (ErrAceInvalid lIdx reason))

----------------------------------------------------------------------
-- U-label path: validate codepoints, Punycode-encode, emit "xn--...".
----------------------------------------------------------------------

-- | Outcome of inspecting the codepoint buffer after the RFC 5895
-- per-label mappings (case-fold + width + NFC) have run.  An input
-- that originally had at least one non-ASCII codepoint may, after
-- mapping, contain only ASCII (typically when 'MAPWIDTH' collapsed
-- fullwidth Latin to ASCII).  This type captures the four ways the
-- post-mapping buffer can shape up.
data PostMappingShape
    = HasNonAscii         -- ^ At least one codepoint @>= 0x80@; carry
                          --   on with the U-label path.
    | AsciiNRLDH          -- ^ All-ASCII, all-LDH, no leading or
                          --   trailing hyphen, no @\"--\"@ at
                          --   positions 3-4; reclassify as plain
                          --   LDH and emit bytes.
    | AsciiBadCp !Int     -- ^ All-ASCII so far, but at this codepoint
                          --   we hit a non-LDH ASCII byte (uppercase,
                          --   underscore, control byte, ...).  The
                          --   carried 'Int' is the offending
                          --   codepoint.
    | AsciiBadHyphens     -- ^ All-ASCII LDH, but the hyphen pattern
                          --   disqualifies the buffer as plain LDH
                          --   (leading or trailing hyphen, or
                          --   @\"--\"@ at positions 3 and 4).
    deriving (Eq, Show)

-- | Single forward pass over @cpBuf[0..cnt)@ classifying the
-- post-mapping shape.  Bails as soon as a non-ASCII codepoint or a
-- non-LDH ASCII codepoint is seen; otherwise inspects the hyphen
-- pattern at the boundary positions.
examinePostMapping
    :: forall s. MutablePrimArray s Int -> Int -> ST s PostMappingShape
examinePostMapping !buf !cnt = scan 0
  where
    scan :: Int -> ST s PostMappingShape
    scan !i
      | i >= cnt = checkShape
      | otherwise = do
          cp <- readPrimArray buf i
          if | cp >= 0x80 -> pure HasNonAscii
             | isLdhCp cp -> scan (i + 1)
             | otherwise  -> pure (AsciiBadCp cp)

    -- All-ASCII-LDH confirmed; check the boundary-position hyphen
    -- rules.  Caller's @cnt > 0@ invariant (flushLabel rejects
    -- empty labels earlier) lets us read positions 0 and cnt-1
    -- unconditionally.
    checkShape :: ST s PostMappingShape
    checkShape = do
        first <- readPrimArray buf 0
        last' <- readPrimArray buf (cnt - 1)
        if first == 0x2D || last' == 0x2D
          then pure AsciiBadHyphens
          else if cnt >= 4
            then do
              p2 <- readPrimArray buf 2
              p3 <- readPrimArray buf 3
              if p2 == 0x2D && p3 == 0x2D
                then pure AsciiBadHyphens
                else pure AsciiNRLDH
            else pure AsciiNRLDH

uLabelPath
    :: MutablePrimArray s Int
    -> MutableByteArray s
    -> LabelFormSet
    -> IdnaFlags
    -> Int                  -- lStart
    -> Int                  -- cpCount
    -> Int                  -- lIdx
    -> [LabelForm]           -- produced
    -> ST s (Either IdnaError (Int, [LabelForm]))
-- All RFC 5891 section 4.2.3.1 ("Hyphen Restrictions") prongs --
-- leading, trailing, and @\"--\"@ at positions 3 and 4 -- are
-- enforced /after/ the per-label mappings, so that a fullwidth
-- hyphen U+FF0D that collapsed to ASCII @\'-\'@ under MAPWIDTH
-- (or any analogous mapping artifact) is caught.
-- 'examinePostMapping' handles them on the all-ASCII-after-
-- mapping branch; 'validateULabelCps' on the still-non-ASCII
-- branch.  That keeps the check aligned with \"the Unicode
-- string\" as the RFC defines it (the post-mapping U-label,
-- not the raw input).
uLabelPath !cpBuf !outBuf !allowed !opts
           !lStart !cpCount !lIdx !produced = do
        -- RFC 5895 section 2.1 mapping (non-ASCII portion): under
        -- 'MAPCASE', walk the codepoint buffer and apply Unicode
        -- toLower to any non-ASCII codepoint with a defined case
        -- mapping.  ASCII A-Z were already lower-cased pre-
        -- classification by 'flushLabel'; here we cover the rest.
        when (MAPCASE `meetsIdnaFlags` opts)
             (unicodeDownCaseBuf cpBuf cpCount)
        -- RFC 5895 section 2.2 mapping: under 'MAPWIDTH', walk the
        -- codepoint buffer and replace each fullwidth\/halfwidth
        -- codepoint with its single-codepoint decomposition target.
        -- Bits in the codepoint buffer that are not in the wide\/
        -- narrow set are left alone.  Label-separator targets
        -- (U+002E, U+3002) are handled at split time; 'MAPDOTS'
        -- is implied by 'effectiveIdnaFlags' so they cannot reach
        -- this pass.
        when (MAPWIDTH `meetsIdnaFlags` opts)
             (unicodeWidthBuf cpBuf cpCount)
        -- Hand-curated UTS #46 subset (beyond IDNA2008): under
        -- 'MAPUTS46', walk the codepoint buffer and expand each
        -- source codepoint in 'uts46Lookup' to its target sequence.
        -- 1:n expansion (era-name codepoints expand 1:2); buffer
        -- growth past 'maxCpsPerLabel' raises 'ErrLabelTooLong'.
        -- 'effectiveIdnaFlags' lifts 'MAPUTS46' to also enable
        -- 'MAPCASE', 'MAPWIDTH', 'MAPDOTS', 'MAPNFC', so by the
        -- time we get here the buffer has already been case-
        -- folded and width-normalised.
        !cnt0 <-
            if MAPUTS46 `meetsIdnaFlags` opts
              then unicodeUTS46Buf cpBuf cpCount
              else pure cpCount
        if cnt0 > maxCpsPerLabel
          then pure (Left (ErrLabelTooLong lIdx cnt0))
          else uLabelMapped cpBuf outBuf allowed opts lStart cnt0
                            lIdx produced

-- | The remainder of 'uLabelPath' after the input-mapping passes:
-- apply 'MAPNFC', examine the post-mapping shape, and encode as
-- either ASCII-LDH or a U-label.  Lifted out of 'uLabelPath' so
-- that 'uLabelPath' can short-circuit on 'MAPUTS46' overflow
-- without nesting the rest of the body in an @if-else@.
uLabelMapped
    :: MutablePrimArray s Int
    -> MutableByteArray s
    -> LabelFormSet
    -> IdnaFlags
    -> Int                  -- lStart
    -> Int                  -- cnt (post-MAPUTS46)
    -> Int                  -- lIdx
    -> [LabelForm]
    -> ST s (Either IdnaError (Int, [LabelForm]))
uLabelMapped !cpBuf !outBuf !allowed !opts !lStart !cnt0 !lIdx !produced = do
        -- RFC 5895 section 2.3 mapping: under 'MAPNFC', normalise
        -- the codepoint buffer to NFC in place before validation,
        -- so a decomposed input (e.g. @"a" + U+0301@) parses as
        -- the precomposed equivalent (here @U+00E1@).  Off by
        -- default; when unset 'normalizeNFC' is not invoked.
        --
        -- After normalising we strip 'NFCCHECK' from the opts
        -- handed to 'validateULabelCps': the buffer is now in NFC
        -- by construction, so the validator's 'isNFC' check would
        -- be a no-op.  A-labels (whose codepoints come from
        -- 'alabelRoundTrip', which receives the original opts)
        -- still get the 'NFCCHECK' check on their decoded form.
        (!cnt, !opts') <-
            if MAPNFC `meetsIdnaFlags` opts
              then do
                  n <- normalizeNFC cpBuf cnt0
                  pure (n, opts `withoutIdnaFlags` NFCCHECK)
              else pure (cnt0, opts)
        -- Post-mapping shape examination.  If the mappings have
        -- collapsed the buffer to all-ASCII, we either reclassify
        -- as plain LDH (clean NR-LDH shape: a-z\/0-9\/hyphen, no
        -- leading or trailing hyphen, no @\"--\"@ at positions
        -- 3-4) or reject (any non-LDH ASCII codepoint, or a
        -- disqualifying hyphen pattern).  We do not try to
        -- recover into ALABEL or RLDH from this position --
        -- that's a bridge too far for what was originally a
        -- U-label.  Buffers that still contain at least one
        -- non-ASCII codepoint continue down the U-label path.
        shape <- examinePostMapping cpBuf cnt
        case shape of
          AsciiBadCp cp ->
              pure (Left (ErrLabelInvalid lIdx (DisallowedCodepoint cp)))
          AsciiBadHyphens ->
              pure (Left (ErrLabelInvalid lIdx HyphenViolation))
          AsciiNRLDH ->
              commitBytes cpBuf outBuf allowed lStart cnt lIdx
                          produced LDH
          HasNonAscii -> uLabelEncode cpBuf outBuf allowed opts'
                                      lStart cnt lIdx produced

uLabelEncode
    :: forall s
    .  MutablePrimArray s Int
    -> MutableByteArray s
    -> LabelFormSet
    -> IdnaFlags
    -> Int                  -- lStart
    -> Int                  -- cnt (post-mapping codepoint count)
    -> Int                  -- lIdx
    -> [LabelForm]           -- produced
    -> ST s (Either IdnaError (Int, [LabelForm]))
uLabelEncode !cpBuf !outBuf !allowed !opts !lStart !cnt !lIdx !produced = do
    rv  <- validateULabelCps cpBuf cnt opts
    rv' <- case rv of
      Just _  -> pure rv
      Nothing
        | BIDICHECK `meetsIdnaFlags` opts -> do
            !summary <- buildBidiSummary cpBuf cnt
            pure (checkBidiLabel summary)
        | otherwise -> pure Nothing
    case rv' of
      Just reason
        -- 'LAXULABEL' not even in the set: nothing to consider,
        -- propagate the rejection unchanged.
        | not (LAXULABEL `memberLabelFormSet` allowed) ->
            pure (Left (ErrLabelInvalid lIdx reason))
        -- First failure is already an ASCII codepoint: we know
        -- the label has non-LDH ASCII content (the offending
        -- codepoint itself).  Hard reject; 'LAXULABEL' must not
        -- rescue --- such a byte cannot appear in a valid
        -- Punycode-encoded wire form.
        | DisallowedCodepoint cp <- reason
        , cp < 0x80 ->
            pure (Left (ErrLabelInvalid lIdx reason))
        -- First failure is non-ASCII (or non-codepoint-specific):
        -- scan the rest of the post-mapping buffer for any
        -- non-LDH ASCII that would also disqualify 'LAXULABEL'
        -- admission.
        | otherwise -> do
            !badAscii <- hasNonLdhAsciiCps cpBuf cnt
            if badAscii
              then pure (Left (ErrLabelInvalid lIdx reason))
              else encodeAs LAXULABEL
      Nothing
        | not (ULABEL `memberLabelFormSet` allowed) ->
            pure (Left (ErrFormNotAllowed  lIdx ULABEL))
        | otherwise -> encodeAs ULABEL
  where
    -- Punycode-encode the post-mapping codepoint buffer and emit
    -- the ACE-prefixed wire form, tagging the result with
    -- the caller-chosen 'LabelForm' (either 'ULABEL' for a label
    -- that passed strict validation, or 'LAXULABEL' for one
    -- admitted via the permissive fallback).
    encodeAs :: LabelForm -> ST s (Either IdnaError (Int, [LabelForm]))
    encodeAs !tag = do
        let !prefAt = lStart + 1
        writeByteArray @Word8 outBuf  prefAt      0x78 -- 'x'
        writeByteArray @Word8 outBuf (prefAt + 1) 0x6E -- 'n'
        writeByteArray @Word8 outBuf (prefAt + 2) 0x2D -- '-'
        writeByteArray @Word8 outBuf (prefAt + 3) 0x2D -- '-'
        cps <- freezeCps cpBuf cnt
        let !bodyAt  = prefAt + 4
            !bodyCap = maxLabelLen - 4
        enc <- punycodeEncode cps cnt outBuf bodyAt bodyCap
        case enc of
          Left _ ->
            pure (Left (ErrPunycodeOverflow lIdx))
          Right encLen ->
            let !contentLen = 4 + encLen
                !newOff     = lStart + 1 + contentLen
            in if contentLen > maxLabelLen
                 then pure (Left (ErrLabelTooLong lIdx contentLen))
                 else if newOff >= maxWireLen
                   then pure (Left (ErrNameTooLong (newOff + 1)))
                   else do
                     writeByteArray outBuf lStart
                                    (fromIntegral contentLen :: Word8)
                     pure (Right (newOff, tag : produced))

freezeCps :: MutablePrimArray s Int -> Int -> ST s (PrimArray Int)
freezeCps !cpBuf !cnt = do
    tmp <- newPrimArray cnt
    copyN tmp 0
    unsafeFreezePrimArray tmp
  where
    copyN dst !i
      | i >= cnt = pure ()
      | otherwise = do
          v <- readPrimArray cpBuf i
          writePrimArray dst i v
          copyN dst (i + 1)

----------------------------------------------------------------------
-- U-label codepoint validation
----------------------------------------------------------------------

-- | U-label codepoint validator.  Walks the codepoint sequence and
-- consults 'idnaDisposition' for each.
--
-- Currently enforced:
--
--   * Each codepoint must have IDNA disposition @PVALID@.
--   * @CONTEXTO@ codepoints are admitted only when 'checkContextO'
--     accepts their context.  All seven contextual rules from
--     RFC 5892 Appendix A.3-A.9 are implemented (the four
--     Script-based rules consult
--     "Text.IDNA2008.Internal.Script").
--   * @CONTEXTJ@ codepoints are admitted only when 'checkContextJ'
--     accepts their context (RFC 5892 Appendix A.1 ZWNJ and A.2
--     ZWJ).  Both rules consult
--     "Text.IDNA2008.Internal.Joining" for Virama membership;
--     A.1 also consults Joining_Type.
--   * Under 'NFCCHECK', full canonical normalisation is performed
--     and the result compared against the input; the label is
--     rejected if it differs.  Off by default.
--   * @DISALLOWED@ \/ @UNASSIGNED@ codepoints are rejected.  Under
--     'EMOJIOK', a non-ASCII @DISALLOWED@ codepoint with
--     @Emoji=Yes@ is admitted as if @PVALID@ (a diagnostic
--     relaxation; @UNASSIGNED@ is still rejected).
--
-- Note: the per-label RFC 5893 (Bidi) check, gated on 'BIDICHECK',
-- runs separately from this function — see 'checkBidiLabel' in
-- the Bidi section below.  It is invoked from 'uLabelEncode'
-- after this validator returns clean.
validateULabelCps
    :: forall s
    .  MutablePrimArray s Int
    -> Int
    -> IdnaFlags
    -> ST s (Maybe LabelReason)
validateULabelCps !cpBuf !cnt !opts
    | cnt <= 0 =
        if NFCCHECK `meetsIdnaFlags` opts
          then nfcCheck
          else pure Nothing
    | otherwise = do
        -- RFC 5891 section 4.2.3.2: a U-label must not begin
        -- with a combining mark (General_Category in {Mn, Mc,
        -- Me}).  Many such codepoints are PVALID per RFC 5892,
        -- so the per-codepoint validator alone wouldn't catch
        -- this; we check the first codepoint explicitly here.
        first <- readPrimArray cpBuf 0
        if isCombiningMark first
          then pure (Just (LeadingCombiningMark first))
          else do
            -- RFC 5891 section 4.2.3.1 ("Hyphen Restrictions"),
            -- all three prongs, enforced /post-mapping/ so that
            -- a fullwidth-hyphen U+FF0D that collapsed to ASCII
            -- @\'-\'@ under MAPWIDTH is caught here even though
            -- 'scanCpBuf' (which runs pre-mapping) did not see
            -- it.  The all-ASCII-after-mapping branch is
            -- handled in 'examinePostMapping'; this is the
            -- still-non-ASCII branch.
            --
            --   1. Leading hyphen: @cpBuf[0] == 0x2D@.
            --   2. Trailing hyphen: @cpBuf[cnt-1] == 0x2D@.
            --   3. @\"--\"@ at positions 3 and 4: a U-label
            --      that would visually mimic an A-label
            --      (e.g. Cyrillic @\"\\x445n----...\"@ vs Latin
            --      @\"xn----...\"@) without being one.  The
            --      ACE-escape namespace is reserved for that
            --      shape.
            last' <- readPrimArray cpBuf (cnt - 1)
            hy34  <- if cnt >= 4
              then do c2 <- readPrimArray cpBuf 2
                      c3 <- readPrimArray cpBuf 3
                      pure (c2 == 0x2D && c3 == 0x2D)
              else pure False
            if first == 0x2D || last' == 0x2D || hy34
              then pure (Just HyphenViolation)
              else go 0
  where
    go :: Int -> ST s (Maybe LabelReason)
    go !i
      | i >= cnt =
          if NFCCHECK `meetsIdnaFlags` opts
            then nfcCheck
            else pure Nothing
      | otherwise = do
          cp <- readPrimArray cpBuf i
          case idnaDisposition cp of
            IdnaPVALID     -> go (i + 1)
            IdnaCONTEXTJ   -> do
              ok <- checkContextJ cpBuf cnt i cp
              if ok
                then go (i + 1)
                else pure (Just (ContextRule cp))
            IdnaCONTEXTO   -> do
              ok <- checkContextO cpBuf cnt i cp
              if ok
                then go (i + 1)
                else pure (Just (ContextRule cp))
            IdnaDISALLOWED
              -- Diagnostic relaxation: under 'EMOJIOK' admit a
              -- non-ASCII DISALLOWED codepoint that carries the
              -- Unicode @Emoji=Yes@ property AND is not in the
              -- UTS #46-mapped subset of emoji codepoints.
              --
              -- Restricted to non-ASCII so that ASCII punctuation
              -- that happens to be @Emoji=Yes@ (e.g. @\'#\'@
              -- U+0023, @\'*\'@ U+002A) stays disallowed.
              --
              -- The @isMappedEmoji@ exclusion drops the small
              -- subset of emoji codepoints whose UTS #46 status
              -- is @mapped@: these resolve ambiguously across the
              -- ecosystem (browsers following UTS #46 mapping reach
              -- the fold target; this library would admit the
              -- codepoint as-is).  When the mapped form and the
              -- admit-as-is form belong to separately registered
              -- domains under different operators — e.g. (xn--q97h.ws)
              -- and its UTS #46 target (xn--uny.ws), seen in the wild
              -- silently routing to either is a security concern.  We
              -- refuse both interpretations under 'EMOJIOK'; a
              -- caller who needs to address such a label can
              -- construct its wire form directly.
              --
              -- Emoji codepoints are not @CONTEXTJ@/@CONTEXTO@,
              -- so no contextual check is needed on the relaxation
              -- path.
              | cp >= 0x80, EMOJIOK `meetsIdnaFlags` opts
              , isEmoji cp, not (isMappedEmoji cp) ->
                  go (i + 1)
              | otherwise -> pure (Just (DisallowedCodepoint cp))
            IdnaUNASSIGNED -> pure (Just (DisallowedCodepoint cp))

    -- | Precise NFC pass via 'isNFC': Quick_Check fast-path on
    -- the all-@Yes@ common case, otherwise full canonical
    -- decompose / reorder / compose and byte-compare against
    -- the input.
    nfcCheck :: ST s (Maybe LabelReason)
    nfcCheck = do
        ok <- isNFC cpBuf cnt
        if ok then pure Nothing else pure (Just NotNFC)

----------------------------------------------------------------------
-- CONTEXTO contextual rules (RFC 5892 Appendix A.3-A.9)
----------------------------------------------------------------------

-- | Decide whether a 'IdnaCONTEXTO' codepoint at position @i@ in a
-- candidate U-label is admissible in its surrounding context.
-- Returns 'True' iff the codepoint's contextual rule is satisfied.
--
-- All seven contextual rules from RFC 5892 Appendix A.3-A.9 are
-- implemented:
--
--   * A.3  Middle Dot @U+00B7@ -- preceded /and/ followed by
--          @U+006C@ (lowercase ASCII @\'l\'@); the Catalan
--          @\"l\\u00B7l\"@ (ela geminada).
--   * A.4  Greek Lower Numeral Sign @U+0375@ -- followed by a
--          codepoint in the Greek script.
--   * A.5  Hebrew Punctuation Geresh @U+05F3@ -- preceded by a
--          codepoint in the Hebrew script.
--   * A.6  Hebrew Punctuation Gershayim @U+05F4@ -- preceded by
--          a codepoint in the Hebrew script.
--   * A.7  Katakana Middle Dot @U+30FB@ -- the label contains at
--          least one codepoint in Hiragana, Katakana, or Han.
--   * A.8  Arabic-Indic Digits @U+0660..U+0669@ -- the label
--          contains no Extended Arabic-Indic digit.
--   * A.9  Extended Arabic-Indic Digits @U+06F0..U+06F9@ -- the
--          label contains no Arabic-Indic digit.
--
-- The Script-based rules (A.4-A.7) consult
-- "Text.IDNA2008.Internal.Script".
checkContextO
    :: MutablePrimArray s Int
    -> Int                              -- cnt
    -> Int                              -- position of cp
    -> Int                              -- cp
    -> ST s Bool
checkContextO !cpBuf !cnt !i !cp
    -- A.3: Middle Dot, surrounded by 'l' on both sides.
    | cp == 0x00B7 =
        if i == 0 || i + 1 >= cnt
          then pure False
          else do
            prev <- readPrimArray cpBuf (i - 1)
            next <- readPrimArray cpBuf (i + 1)
            pure (prev == 0x6C && next == 0x6C)
    -- A.4: Greek Lower Numeral Sign, followed by a Greek codepoint.
    | cp == 0x0375 =
        if i + 1 >= cnt
          then pure False
          else do
            next <- readPrimArray cpBuf (i + 1)
            pure (isGreekCp next)
    -- A.5 / A.6: Hebrew Geresh / Gershayim, preceded by a Hebrew
    -- codepoint.
    | cp == 0x05F3 || cp == 0x05F4 =
        if i == 0
          then pure False
          else do
            prev <- readPrimArray cpBuf (i - 1)
            pure (isHebrewCp prev)
    -- A.7: Katakana Middle Dot, with at least one Hira / Kata / Han
    -- codepoint somewhere in the label.
    | cp == 0x30FB = labelHasAny cpBuf cnt isHkhCp
    -- A.8: Arabic-Indic digits and Extended Arabic-Indic digits
    -- must not appear in the same label.
    | cp >= 0x0660 && cp <= 0x0669 =
        labelLacks cpBuf cnt 0x06F0 0x06F9
    -- A.9: same rule, mirrored.
    | cp >= 0x06F0 && cp <= 0x06F9 =
        labelLacks cpBuf cnt 0x0660 0x0669
    -- No other CONTEXTO codepoints are defined by RFC 5892.
    -- Reject defensively in case the IDNA disposition table
    -- starts to advertise one we do not yet recognise.
    | otherwise = pure False

-- | Walk a codepoint buffer and return 'True' iff no codepoint in
-- @[lo, hi]@ appears anywhere in @[0, cnt)@.
labelLacks
    :: MutablePrimArray s Int
    -> Int                              -- cnt
    -> Int                              -- forbidden lo
    -> Int                              -- forbidden hi
    -> ST s Bool
labelLacks !cpBuf !cnt !lo !hi = go 0
  where
    go !j
      | j >= cnt = pure True
      | otherwise = do
          c <- readPrimArray cpBuf j
          if c >= lo && c <= hi
            then pure False
            else go (j + 1)

-- | Walk a codepoint buffer and return 'True' iff @p@ accepts
-- any codepoint in @[0, cnt)@.
labelHasAny
    :: MutablePrimArray s Int
    -> Int                              -- cnt
    -> (Int -> Bool)                    -- predicate
    -> ST s Bool
labelHasAny !cpBuf !cnt !p = go 0
  where
    go !j
      | j >= cnt = pure False
      | otherwise = do
          c <- readPrimArray cpBuf j
          if p c
            then pure True
            else go (j + 1)

----------------------------------------------------------------------
-- CONTEXTJ contextual rules (RFC 5892 Appendix A.1, A.2)
----------------------------------------------------------------------

-- | Decide whether a 'IdnaCONTEXTJ' codepoint at position @i@ in a
-- candidate U-label is admissible in its surrounding context.
-- Returns 'True' iff the codepoint's contextual rule is satisfied.
--
--   * A.1  ZERO WIDTH NON-JOINER @U+200C@ -- admitted when either
--          (a) the immediately preceding codepoint is a Virama
--          (Canonical_Combining_Class = 9), or (b) the label
--          structure matches the regex
--          @(L|D) T* U+200C T* (R|D)@ over the Joining_Type
--          property.
--   * A.2  ZERO WIDTH JOINER @U+200D@ -- admitted only when the
--          immediately preceding codepoint is a Virama.
--
-- Both arms consult "Text.IDNA2008.Internal.Joining".
checkContextJ
    :: MutablePrimArray s Int
    -> Int                              -- cnt
    -> Int                              -- position of cp
    -> Int                              -- cp
    -> ST s Bool
checkContextJ !cpBuf !cnt !i !cp
    -- A.2: ZWJ.  Only the Virama-prefix arm.
    | cp == 0x200D = viramaPrefix cpBuf i
    -- A.1: ZWNJ.  Virama-prefix arm OR the joining-type regex.
    | cp == 0x200C = do
        vp <- viramaPrefix cpBuf i
        if vp
          then pure True
          else zwnjJoinContext cpBuf cnt i
    -- No other CONTEXTJ codepoints are defined by RFC 5892;
    -- reject defensively if the disposition table grows one.
    | otherwise = pure False

-- | Is the codepoint immediately preceding position @i@ a Virama
-- (Canonical_Combining_Class = 9)?  False at @i == 0@.
viramaPrefix
    :: MutablePrimArray s Int
    -> Int                              -- position
    -> ST s Bool
viramaPrefix !cpBuf !i
    | i == 0 = pure False
    | otherwise = do
        prev <- readPrimArray cpBuf (i - 1)
        pure (isVirama prev)

-- | The A.1 ZWNJ join-context regex:
--
-- > (Joining_Type:{L,D}) (Joining_Type:T)*  U+200C  (Joining_Type:T)*  (Joining_Type:{R,D})
--
-- Walk left from @i - 1@ skipping any 'jtIsTransparent' codepoints
-- and require the first non-transparent to be 'jtIsLeftOrDual';
-- walk right from @i + 1@ skipping any 'jtIsTransparent' and
-- require the first non-transparent to be 'jtIsRightOrDual'.
-- Either side hitting the label boundary fails the rule.
zwnjJoinContext
    :: forall s
    .  MutablePrimArray s Int
    -> Int                              -- cnt
    -> Int                              -- position of ZWNJ
    -> ST s Bool
zwnjJoinContext !cpBuf !cnt !i = do
    leftOk  <- scanLeft (i - 1)
    if not leftOk
      then pure False
      else scanRight (i + 1)
  where
    scanLeft :: Int -> ST s Bool
    scanLeft !j
      | j < 0 = pure False
      | otherwise = do
          c <- readPrimArray cpBuf j
          if jtIsTransparent c
            then scanLeft (j - 1)
            else pure (jtIsLeftOrDual c)

    scanRight :: Int -> ST s Bool
    scanRight !j
      | j >= cnt = pure False
      | otherwise = do
          c <- readPrimArray cpBuf j
          if jtIsTransparent c
            then scanRight (j + 1)
            else pure (jtIsRightOrDual c)

----------------------------------------------------------------------
-- Per-label Bidi check (RFC 5893)
----------------------------------------------------------------------

-- | Per-label RFC 5893 check.  Thin wrapper over
-- 'checkBidiSummary' (in trigger-on mode — per-label semantics)
-- that lifts the rule-violation payload into 'LabelReason' for
-- the parser-time error path.  See 'checkBidiSummary' for the
-- semantics of the rule application.
checkBidiLabel :: BidiSummary -> Maybe LabelReason
checkBidiLabel !s = LabelBidi <$> checkBidiSummary False s

----------------------------------------------------------------------
-- A-label strict round-trip
----------------------------------------------------------------------

-- | Strict A-label round-trip check.  Given the lower-cased Punycode
-- body of an ACE-prefixed label (i.e. the bytes /after/ the
-- @\"xn--\"@ prefix), return the decoded codepoints if they form a
-- valid U-label whose re-encoding matches the input body
-- byte-for-byte.  Otherwise return the reason the label failed:
--
--   * 'BadPunycode' -- the body is not valid Punycode
--     (truncated, bad delimiter, non-base-36 byte, internal overflow).
--   * 'DecodedInvalid' -- the decoded codepoints have an
--     IDNA2008 disposition that is not @PVALID@.  The wrapped
--     'LabelReason' identifies the first offending codepoint.
--   * 'RoundTripMismatch' -- the decoded form is a valid
--     U-label, but its re-encoding to ACE does not match the input
--     body byte-for-byte.
--
-- The input body is expected to be lower-cased by the caller; the
-- comparison after re-encoding is exact byte-equality.  Punycode
-- encoding always emits lower-case digits, and basic codepoints in
-- a valid U-label are themselves already lower-case ASCII (uppercase
-- ASCII has IDNA2008 disposition @DISALLOWED@), so a lower-cased
-- input body will round-trip exactly when the label is a real
-- A-label.
alabelRoundTrip
    :: forall s
    .  ByteArray                -- ^ Lower-cased Punycode body bytes
    -> Int                      -- ^ Body start offset
    -> Int                      -- ^ Body length
    -> IdnaFlags                -- ^ Threaded through to the U-label
                                --   validator (e.g., 'NFCCHECK', when
                                --   set, also constrains the decoded
                                --   form).
    -> ST s (Either AceReason (PrimArray Int))
alabelRoundTrip !body !bStart !bLen !opts = do
    cpBuf <- newPrimArray maxCpsPerLabel
    rDec <- punycodeDecode body bStart bLen cpBuf 0 maxCpsPerLabel
    case rDec of
      Left _ -> pure (Left BadPunycode)
      Right cpCount -> do
        rVal <- validateULabelCps cpBuf cpCount opts
        rVal' <- case rVal of
          Just _  -> pure rVal
          Nothing
            | BIDICHECK `meetsIdnaFlags` opts -> do
                !summary <- buildBidiSummary cpBuf cpCount
                pure (checkBidiLabel summary)
            | otherwise -> pure Nothing
        case rVal' of
          Just reason -> pure (Left (DecodedInvalid reason))
          Nothing -> do
            cps <- freezeCps cpBuf cpCount
            outBuf <- newByteArray maxLabelLen
            rEnc <- punycodeEncode cps cpCount outBuf 0 maxLabelLen
            case rEnc of
              Left _ -> pure (Left RoundTripMismatch)
              Right encLen
                | encLen /= bLen ->
                    pure (Left RoundTripMismatch)
                | otherwise -> do
                    eq <- byteRangeEqMutImm outBuf 0 body bStart bLen
                    if eq
                      then pure (Right cps)
                      else pure (Left RoundTripMismatch)

-- | Compare @[mOff, mOff+len)@ of a 'MutableByteArray' against
-- @[iOff, iOff+len)@ of a 'ByteArray' for exact byte equality.
byteRangeEqMutImm
    :: forall s
    .  MutableByteArray s
    -> Int
    -> ByteArray
    -> Int
    -> Int
    -> ST s Bool
byteRangeEqMutImm !mBuf !mOff !iBuf !iOff !len = go 0
  where
    go !k
      | k >= len = pure True
      | otherwise = do
          mb <- readByteArray @Word8 mBuf (mOff + k)
          let !ib = indexByteArray iBuf (iOff + k) :: Word8
          if mb /= ib
            then pure False
            else go (k + 1)

----------------------------------------------------------------------
-- Domain -> Unicode display form
----------------------------------------------------------------------

-- | Render a 'Domain' as 'Text' in Unicode presentation form.
-- Valid labels decode to U-labels.  Any other labels render as
-- ASCII (with escapes for bytes that need them).  The root domain
-- renders as @\".\"@, other domains have /no/ trailing dots.
--
-- See 'unparseDomainOpts' for finer control.
domainToUnicode :: Domain -> Text
domainToUnicode dom = case unparseDomainOpts forms flags dom of
    Right (t, _) -> t
    Left  _      -> error "domainToUnicode: unreachable"
  where
    !flags = defaultIdnaFlags <> ASCIIFALLBACK
    !forms = foldMap' labelFormToSet
        [LDH, RLDH, ULABEL, FAKEA, ATTRLEAF, WILDLABEL, OCTET]

-- | Render a 'Domain' as 'Text' in ASCII presentation form.
-- Every A-label label stays in its ASCII form (no Unicode
-- decoding).  Labels with non-LDH characters are output with
-- escapes as needed.  The root domain renders as @\".\"@,
-- other domains have /no/ trailing dots.
--
-- See 'unparseDomainOpts' for finer control.
domainToAscii :: Domain -> Text
domainToAscii dom = case unparseDomainOpts forms mempty dom of
    Right (t, _) -> t
    Left  _      -> error "domainToAscii: unreachable"
  where
    !forms = foldMap' labelFormToSet
        [LDH, RLDH, ALABEL, FAKEA, ATTRLEAF, WILDLABEL, OCTET]

-- | Grow if needed, then write a single raw byte at the cursor.
writeRawByte
    :: MutableByteArray s -> Int -> Int -> Word8
    -> ST s (MutableByteArray s, Int, Int)
writeRawByte !mba !cap !cur !b = do
    (mba', cap') <- ensureCapacity mba cap cur 1
    writeByteArray mba' cur b
    pure (mba', cap', cur + 1)

-- | Reserve at least @need@ bytes of headroom in the buffer,
-- growing it via 'resizeMutableByteArray' if necessary.  The
-- growth branch adds 50% more capacity (or jumps directly to
-- @cur + need@, whichever is larger).
ensureCapacity
    :: MutableByteArray s -> Int -> Int -> Int
    -> ST s (MutableByteArray s, Int)
ensureCapacity !mba !cap !cur !need
    | cur + need <= cap = pure (mba, cap)
    | otherwise = do
        let !cap' = max (cur + need) (cap + cap `div` 2)
        mba' <- resizeMutableByteArray mba cap'
        pure (mba', cap')

-- | Walk a label's wire-bytes slice @sbs[off .. off+len)@,
-- writing each byte (with presentation-form escaping as needed)
-- into the buffer.  The slice form lets the domain-level walker
-- emit each label in place against the parent @Domain@'s wire
-- bytes without per-label allocation.
writeEscapedLabel
    :: MutableByteArray s -> Int -> Int
    -> ShortByteString -> Int -> Int
    -> ST s (MutableByteArray s, Int, Int)
writeEscapedLabel !mba0 !cap0 !cur0 (SBS sba) !off !len =
    step mba0 cap0 cur0 0
  where
    !arr = ByteArray sba
    step !mba !cap !cur !i
      | i >= len = pure (mba, cap, cur)
      | otherwise = do
          let !b = indexByteArray arr (off + i) :: Word8
          (mba', cap', cur') <- writeEscapedByte mba cap cur b
          step mba' cap' cur' (i + 1)

-- | Escape one byte into the buffer.  Printable ASCII syntactic
-- specials (per 'special') become @\\C@ (two bytes); controls,
-- whitespace, DEL, and high bytes become @\\DDD@ (four bytes);
-- other printable ASCII passes through unchanged.  This is the
-- single source of truth for the byte-level escape policy --
-- both 'writeEscapedLabel' and the codepoint emitter
-- 'writeCpUtf8' route ASCII bytes through here.
writeEscapedByte
    :: MutableByteArray s -> Int -> Int -> Word8
    -> ST s (MutableByteArray s, Int, Int)
writeEscapedByte !mba !cap !cur !b
    | special b = do
        (mba', cap') <- ensureCapacity mba cap cur 2
        writeByteArray mba' cur       (0x5C :: Word8)
        writeByteArray mba' (cur + 1) b
        pure (mba', cap', cur + 2)
    | b <= 0x20 || b >= 0x7F = do
        (mba', cap') <- ensureCapacity mba cap cur 4
        let !d100 = b `quot` 100
            !d10  = (b `quot` 10) `rem` 10
            !d1   = b `rem` 10
        writeByteArray mba' cur       (0x5C :: Word8)
        writeByteArray mba' (cur + 1) (0x30 + d100)
        writeByteArray mba' (cur + 2) (0x30 + d10)
        writeByteArray mba' (cur + 3) (0x30 + d1)
        pure (mba', cap', cur + 4)
    | otherwise = do
        (mba', cap') <- ensureCapacity mba cap cur 1
        writeByteArray mba' cur b
        pure (mba', cap', cur + 1)

special :: Word8 -> Bool
special b = testBit tab $ fromIntegral (b - 0x22)
  where
    -- Status of 59 bytes from '"' to '\\', bit set means "special".
    tab :: Word64
    tab = 0b0100_0000_0000_0000_0000_0000_0000_0100_0010_0000_0000_0001_0000_1100_0101

-- | Walk a 'PrimArray' of codepoints and emit each as 1--4 UTF-8
-- bytes into the buffer.
writeCodepointsUtf8
    :: MutableByteArray s -> Int -> Int -> PrimArray Int
    -> ST s (MutableByteArray s, Int, Int)
writeCodepointsUtf8 !mba0 !cap0 !cur0 !cps = step mba0 cap0 cur0 0
  where
    !n = sizeofPrimArray cps
    step !mba !cap !cur !i
      | i >= n = pure (mba, cap, cur)
      | otherwise = do
          let !cp = indexPrimArray cps i
          (mba', cap', cur') <- writeCpUtf8 mba cap cur cp
          step mba' cap' cur' (i + 1)

-- | Encode a single codepoint as UTF-8 into the buffer.  Up to
-- four bytes for codepoints @<= U+10FFFF@.  Caller guarantees
-- the codepoint is in range (the decoder enforces this).
--
-- Codepoints @< 0x80@ are routed through 'writeEscapedByte' so
-- that ASCII bytes with zone-file significance get the usual
-- @\\C@ or @\\DDD@ treatment.  This matters whenever a FAKEA
-- Punycode body decodes (via 'LAXULABEL' admission) to codepoints
-- like @\'.\'@, @\';\'@, or @\'\@\'@ that strict round-trip would
-- have rejected.  Multi-byte UTF-8 encodings of codepoints
-- @>= 0x80@ are emitted verbatim; continuation bytes are all
-- @>= 0x80@ and carry no per-byte zone-file meaning.
writeCpUtf8
    :: MutableByteArray s -> Int -> Int -> Int
    -> ST s (MutableByteArray s, Int, Int)
writeCpUtf8 !mba !cap !cur !cp
    | cp < 0x80 = writeEscapedByte mba cap cur (fromIntegral cp)
    | cp < 0x800 = do
        (mba', cap') <- ensureCapacity mba cap cur 2
        writeByteArray mba' cur
            (fromIntegral (0xC0 .|. (cp `unsafeShiftR` 6)) :: Word8)
        writeByteArray mba' (cur + 1)
            (fromIntegral (0x80 .|. (cp .&. 0x3F)) :: Word8)
        pure (mba', cap', cur + 2)
    | cp < 0x10000 = do
        (mba', cap') <- ensureCapacity mba cap cur 3
        writeByteArray mba' cur
            (fromIntegral (0xE0 .|. (cp `unsafeShiftR` 12)) :: Word8)
        writeByteArray mba' (cur + 1)
            (fromIntegral (0x80 .|. ((cp `unsafeShiftR` 6) .&. 0x3F)) :: Word8)
        writeByteArray mba' (cur + 2)
            (fromIntegral (0x80 .|. (cp .&. 0x3F)) :: Word8)
        pure (mba', cap', cur + 3)
    | otherwise = do
        (mba', cap') <- ensureCapacity mba cap cur 4
        writeByteArray mba' cur
            (fromIntegral (0xF0 .|. (cp `unsafeShiftR` 18)) :: Word8)
        writeByteArray mba' (cur + 1)
            (fromIntegral (0x80 .|. ((cp `unsafeShiftR` 12) .&. 0x3F)) :: Word8)
        writeByteArray mba' (cur + 2)
            (fromIntegral (0x80 .|. ((cp `unsafeShiftR` 6) .&. 0x3F)) :: Word8)
        writeByteArray mba' (cur + 3)
            (fromIntegral (0x80 .|. (cp .&. 0x3F)) :: Word8)
        pure (mba', cap', cur + 4)

----------------------------------------------------------------------
-- Wire-bytes label classification (unparse side)
----------------------------------------------------------------------

-- | Byte-shape classification of a wire-form label, before any
-- @\"xn--\"@ round-trip check.  Used by 'unparseDomainOpts' to
-- decide which 'LabelForm' applies to each wire label.
data WireShape
    = WSWild        -- ^ Single byte @\'*\'@.  Renders as 'WILDLABEL'.
    | WSLdh         -- ^ All-LDH, no leading or trailing hyphen, no
                    --   @\"--\"@ at positions 3-4.  Renders as 'LDH'.
    | WSRldh        -- ^ LDH with @\"--\"@ at positions 3-4 and a
                    --   non-@\"xn\"@ first pair.  Renders as 'RLDH'.
    | WSXnPrefix    -- ^ LDH with @\"--\"@ at positions 3-4 and an
                    --   @\"xn\"@ first pair.  The 'ALABEL' /
                    --   'FAKEA' decision is made by 'alabelRoundTrip'
                    --   under the active 'IdnaFlags'.
    | WSAttrLeaf    -- ^ Leading @\'_\'@, bytes @1..n-1@ all LDH,
                    --   no trailing hyphen.  Renders as 'ATTRLEAF'.
    | WSOctet       -- ^ Anything else.  Renders as 'OCTET'.
    deriving (Eq, Show)

-- | Classify a wire-form label's raw bytes into a 'WireShape'.
-- Pure, single pass over @sbs[off .. off+len)@.  The slice form
-- lets a single 'ShortByteString' carry either a stand-alone
-- label (called with @off = 0@ and @len = 'SBS.length' sbs@) or
-- the full wire form of a 'Domain' (the domain-level walker
-- supplies each label's offset and length in turn), so no
-- per-label 'ShortByteString' allocation is needed.
--
-- The case of LDH letters is preserved on the wire and matched
-- case-insensitively here, so an uppercase @\"XN--ABC\"@ slice
-- classifies as 'WSXnPrefix' just like the canonical lowercase
-- form.
classifyWireBytes :: ShortByteString -> Int -> Int -> WireShape
classifyWireBytes (SBS sba) !off !len = case len of
    0 -> WSOctet
    1 | byteAt 0 == 0x2A -> WSWild
    n -> classify n
  where
    !arr      = ByteArray sba
    byteAt !i = indexByteArray arr (off + i) :: Word8
    classify !n
      | b0 == 0x5F =
          -- Leading underscore: ATTRLEAF iff every byte past the
          -- underscore is LDH and the last byte is not a hyphen.
          if allLdh 1 n && bn1 /= 0x2D
            then WSAttrLeaf
            else WSOctet
      | b0 /= 0x2D && bn1 /= 0x2D && allLdh 0 n =
          -- LDH-shaped: distinguish RLDH / xn-- / plain LDH by the
          -- positions 3-4 hyphen pair and the first-pair letters.
          if n >= 4 && byteAt 2 == 0x2D && byteAt 3 == 0x2D
            then if isXnPair then WSXnPrefix else WSRldh
            else WSLdh
      | otherwise = WSOctet
      where
        !b0       = byteAt 0
        !bn1      = byteAt (n - 1)
        !isXnPair =  (b0 .|. 0x20) == 0x78 && (byteAt 1 .|. 0x20 == 0x6E)
    allLdh !i !lim
      | i >= lim  = True
      | otherwise = isLdhByte (byteAt i) && allLdh (i + 1) lim

-- | The LDH alphabet on the wire: @a-z@, @A-Z@, @0-9@, @-@.
-- Lowercase is the canonical wire form; uppercase letters can
-- appear when the input was upper-case and 'MAPCASE' was not
-- applied.
isLdhByte :: Word8 -> Bool
isLdhByte b
    | (b .|. 0x20) - 0x61 < 26 = True      -- A-Z, a-z
    | b - 0x30 < 10            = True      -- 0-9
    | otherwise                = b == 0x2D -- hyphen
{-# INLINE isLdhByte #-}

-- | Render a single wire-form label as 'Text' in presentation
-- form and report that form's classification.
--
-- Non A-labels are classified by their byte content ('LDH',
-- 'RLDH', 'ATTRLEAF', 'WILDLABEL', or 'OCTET') and render as
-- ASCII, with escapes applied as needed.
--
-- The presentation form of a wire-form ACE-prefixed LDH-label
-- can have four possible classifications:
--
-- * 'ULABEL': The label passed validation and was decoded to Unicode.
-- * 'LAXULABEL': The label failed (lax) validation, and was decoded to Unicode.
-- * 'ALABEL': The label passed validation and left in ASCII form.
-- * 'FAKEA': The label failed validation and left in ASCII form.
--
-- The specified 'LabelFormSet' selects which of the above forms are
-- allowed, and, when a label can be rendered as either Unicode
-- or ASCII, the Unicode-output form is chosen.
--
-- 'IdnaFlags' determine whether the label content is validated
-- ('ALABELCHECK'), and what the validation entails ('NFCCHECK',
-- 'BIDICHECK').
--
-- Returns 'Left' 'ErrFormNotAllowed' when the label's content is
-- not compatible with any of the specified forms.
unparseLabelOpts
    :: LabelFormSet
    -> IdnaFlags
    -> ShortByteString
    -> Either IdnaError (Text, LabelForm)
unparseLabelOpts !forms !flagsIn sbs = runST do
    let !flags    = effectiveIdnaFlags flagsIn
        !labelLen = SBS.length sbs
        !cap0     = max 16 labelLen
    mba0 <- newByteArray cap0
    r <- unparseLabelInto forms flags 0 mba0 cap0 0 sbs 0 labelLen
    case r of
      Left err -> pure (Left err)
      Right (mba, _cap, !finalLen, !form) -> do
        shrinkMutableByteArray mba finalLen
        ByteArray b <- unsafeFreezeByteArray mba
        pure (Right (Text (TA.ByteArray b) 0 finalLen, form))

-- | Render one label into the supplied output buffer, choosing a
-- 'LabelForm' from the supplied set.  Used by both the single-label
-- 'unparseLabelOpts' and the domain-level 'unparseDomainOpts'.
--
-- The 'IdnaFlags' argument is assumed to have already had its
-- implication lifts applied (the wrappers do this once, callers
-- in a loop need not repeat).  The @lIdx@ argument is the label
-- index used when reporting 'ErrFormNotAllowed'.  The
-- @sbs@\/@off@\/@len@ triple is the label's wire-byte slice
-- within its containing 'ShortByteString' -- the parent 'Domain'
-- for 'unparseDomainOpts', the single-label argument for
-- 'unparseLabelOpts'.
unparseLabelInto
    :: forall s.
       LabelFormSet
    -> IdnaFlags
    -> Int                                  -- ^ lIdx
    -> MutableByteArray s -> Int -> Int     -- ^ output: mba, cap, cur
    -> ShortByteString -> Int -> Int        -- ^ input: sbs, off, len
    -> ST s (Either IdnaError (MutableByteArray s, Int, Int, LabelForm))
unparseLabelInto !forms !flags !lIdx
                 !mba0 !cap0 !cur0
                 sbs !off !len =
    case classifyWireBytes sbs off len of
      WSWild     -> admitAscii WILDLABEL
      WSLdh      -> admitAscii LDH
      WSRldh     -> admitAscii RLDH
      WSAttrLeaf -> admitAscii ATTRLEAF
      WSOctet    -> admitAscii OCTET
      WSXnPrefix -> dispatchXn
  where
    admitAscii !form
      | not (form `memberLabelFormSet` forms) =
          pure (Left (ErrFormNotAllowed lIdx form))
      | otherwise = do
          (mba, cap, cur) <- writeEscapedLabel mba0 cap0 cur0 sbs off len
          pure (Right (mba, cap, cur, form))

    emitUnicode !form !cps = do
        (mba, cap, cur) <- writeCodepointsUtf8 mba0 cap0 cur0 cps
        pure (Right (mba, cap, cur, form))

    -- @\"xn--\"@ branch.  Lower-case the body bytes into a fresh
    -- buffer, then dispatch on 'ALABELCHECK' and the underlying
    -- classification.
    dispatchXn = do
        let !bodyLen = len - 4
        bodyBufM <- newByteArray bodyLen
        let copyLc !i
              | i >= bodyLen = pure ()
              | otherwise = do
                  let !b = SBS.index sbs (off + 4 + i)
                      !c | b - 0x41 < 26 = b + 0x20
                         | otherwise     = b
                  writeByteArray bodyBufM i c
                  copyLc (i + 1)
        copyLc 0
        body <- unsafeFreezeByteArray bodyBufM
        if ALABELCHECK `meetsIdnaFlags` flags
          then do
            rt <- alabelRoundTrip body 0 bodyLen flags
            case rt of
              Right cps -> renderAlabel cps
              Left _    -> renderFakea body bodyLen
          else
            -- 'ALABELCHECK' off: the label is 'ALABEL' by fiat.
            -- A 'ULABEL' render uses Punycode-only (lax) decode
            -- since no strict round-trip was requested; if even
            -- Punycode fails, fall through to the literal branch.
            if ULABEL `memberLabelFormSet` forms
              then do
                d <- laxDecode body bodyLen
                case d of
                  Right cps -> emitUnicode ULABEL cps
                  Left _    -> admitXnLit ALABEL
              else admitXnLit ALABEL

    -- Render the original @\"xn--\"@ wire bytes as ASCII literal
    -- under the given classification.  Pure LDH on the wire, so
    -- 'writeEscapedLabel' here just copies bytes.
    admitXnLit !form
      | not (form `memberLabelFormSet` forms) =
          pure (Left (ErrFormNotAllowed lIdx form))
      | otherwise = do
          (mba, cap, cur) <- writeEscapedLabel mba0 cap0 cur0 sbs off len
          pure (Right (mba, cap, cur, form))

    -- Underlying 'ALABEL': choose 'ULABEL' (Unicode) when
    -- requested, else 'ALABEL' (literal), else reject.
    renderAlabel !cps
      | ULABEL `memberLabelFormSet` forms = emitUnicode ULABEL cps
      | ALABEL `memberLabelFormSet` forms = admitXnLit ALABEL
      | otherwise = pure (Left (ErrFormNotAllowed lIdx ALABEL))

    -- Underlying 'FAKEA': choose 'LAXULABEL' (Unicode via lax
    -- decode) when requested, else 'FAKEA' (literal), else
    -- reject.  When 'LAXULABEL' is requested but Punycode itself
    -- fails so there are no codepoints to render, fall through
    -- to the literal branch.
    renderFakea !body !bodyLen
      | LAXULABEL `memberLabelFormSet` forms = do
          d <- laxDecode body bodyLen
          case d of
            Right cps -> emitUnicode LAXULABEL cps
            Left _    -> admitXnLit FAKEA
      | FAKEA `memberLabelFormSet` forms = admitXnLit FAKEA
      | otherwise = pure (Left (ErrFormNotAllowed lIdx FAKEA))

-- | Render a 'Domain' as 'Text' in presentation form, paired with
-- the per-label 'LabelInfo'.  The output has no trailing dot,
-- except for the root domain which renders as @\".\"@.
--
-- See 'unparseLabelOpts' for how 'LabelFormSet' and 'IdnaFlags'
-- affect the validation and output form of each label.
--
-- Under 'BIDICHECK', the RFC 5893 cross-label rules are enforced
-- whenever any label carries an RTL or AL codepoint.  Failure
-- yields 'Left' 'ErrCrossLabelBidi'.  'ASCIIFALLBACK' degrades
-- that failure into a successful retry with 'ULABEL' and
-- 'LAXULABEL' dropped from the set, so every label renders in
-- its literal ASCII form.
--
-- The cross-label check is suppressed only when at least one
-- label is /actually/ classified as 'LAXULABEL' in the returned
-- 'LabelInfo' — not merely because 'LAXULABEL' is in the
-- supplied set.  A caller who admits LAXULABEL has already
-- accepted per-label validation failures, so a cross-label
-- rejection on top would be contradictory.  A domain whose
-- labels all classify cleanly but whose cross-label rules
-- fail still surfaces as 'ErrCrossLabelBidi'.
--
-- If any label's classification falls outside the supplied set,
-- the call returns 'Left' 'ErrFormNotAllowed'.
unparseDomainOpts
    :: LabelFormSet
    -> IdnaFlags
    -> Domain
    -> Either IdnaError (Text, LabelInfo)
unparseDomainOpts !forms !flagsIn (Domain sbs)
    | sblen <= 1 = Right (T.singleton '.', mkLabelInfo [])
    | otherwise  = case attempt forms of
        Right res -> Right res
        Left err@(ErrCrossLabelBidi _ _)
          | ASCIIFALLBACK `meetsIdnaFlags` flags ->
              attempt (forms `withoutLabelFormSet` uOutForms)
          | otherwise -> Left err
        Left err -> Left err
  where
    !flags     = effectiveIdnaFlags flagsIn
    !sblen     = SBS.length sbs
    !checkBidi = BIDICHECK `meetsIdnaFlags` flags
    !uOutForms = foldMap' labelFormToSet [ULABEL, LAXULABEL]

    -- One render pass under the given 'LabelFormSet'.  Allocates a
    -- buffer, walks labels, and on completion either returns the
    -- ('Text', 'LabelInfo') pair, or surfaces a cross-label Bidi
    -- violation (when 'BIDICHECK' is on and both an RTL/AL/AN
    -- trigger and a per-label Rules 1-6 violation were observed),
    -- or surfaces the per-label error from 'unparseLabelInto'.
    --
    -- Cross-label Bidi enforcement is suppressed when at least
    -- one label in the output was actually classified as
    -- 'LAXULABEL': admitting a LAXULABEL is the caller's opt-in
    -- to per-label permissiveness, so surfacing a cross-label
    -- rejection on top of that opt-in would be contradictory.
    -- When no label needed LAXULABEL admission --- even if
    -- 'LAXULABEL' /is/ in the supplied set --- the cross-label
    -- check still fires, so a domain whose individual labels
    -- happen to pass per-label rules but whose cross-label rules
    -- fail (e.g. @\"_tcp.אב.example\"@: ATTRLEAF + clean
    -- ULABEL + LDH, with the cross-label rule catching @\"_tcp\"@
    -- in RTL context) still surfaces as 'ErrCrossLabelBidi'.
    attempt :: LabelFormSet -> Either IdnaError (Text, LabelInfo)
    attempt !activeForms = runST do
        let !cap0 = max 32 sblen
        mba0 <- newByteArray cap0
        r <- walk activeForms mba0 cap0 0 0 0 [] False Nothing
        case r of
          Left err -> pure (Left err)
          Right (mba, _cap, !finalLen, !revForms, !triggered, !mViol) -> do
            let !laxAdmitted = any (== LAXULABEL) revForms
            case mViol of
              Just (idx, rule)
                | triggered && checkBidi && not laxAdmitted ->
                    pure (Left (ErrCrossLabelBidi idx rule))
              _ -> do
                shrinkMutableByteArray mba finalLen
                ByteArray b <- unsafeFreezeByteArray mba
                let !text = Text (TA.ByteArray b) 0 finalLen
                    !info = mkLabelInfo (reverse revForms)
                pure (Right (text, info))

    -- Walk labels left-to-right, rendering each into the shared
    -- output buffer via 'unparseLabelInto' and scanning the
    -- just-written bytes (under 'BIDICHECK') to update the
    -- cross-label Bidi state.  Carries the buffer state, the
    -- in-reverse 'LabelForm' accumulator, and the Bidi state
    -- (whether an RTL/AL/AN codepoint has been seen anywhere, and
    -- the index of the first label that failed Rules 1-6 under
    -- forced check).
    walk :: forall s. LabelFormSet
         -> MutableByteArray s -> Int -> Int -> Int -> Int
         -> [LabelForm] -> Bool -> Maybe (Int, BidiRuleViolation)
         -> ST s ( Either IdnaError
                          ( MutableByteArray s, Int, Int, [LabelForm]
                          , Bool, Maybe (Int, BidiRuleViolation) ) )
    walk !activeForms !mba !cap !cur !off !lIdx
         !revForms !triggered !mViol = do
        let !lLen = fromIntegral (SBS.index sbs off) :: Int
        if lLen == 0
          then pure (Right (mba, cap, cur, revForms, triggered, mViol))
          else do
            -- @\'.\'@ separator before all labels after the first.
            (mba1, cap1, cur1) <-
              if lIdx > 0
                then writeRawByte mba cap cur 0x2E
                else pure (mba, cap, cur)
            r <- unparseLabelInto activeForms flags lIdx
                                  mba1 cap1 cur1
                                  sbs (off + 1) lLen
            case r of
              Left err -> pure (Left err)
              Right (mba2, cap2, cur2, !form) -> do
                (!triggered', !mViol') <-
                  if checkBidi
                    then do
                      !sm <- bidiSummaryFromMutableUtf8Range mba2 cur1 cur2
                      let !trig' = addBidiTrigger triggered sm
                          !mViol'
                            | Just _ <- mViol = mViol
                            | otherwise = case checkBidiSummary True sm of
                                Just rule -> Just (lIdx, rule)
                                Nothing   -> Nothing
                      pure (trig', mViol')
                    else pure (triggered, mViol)
                walk activeForms mba2 cap2 cur2
                     (off + 1 + lLen) (lIdx + 1)
                     (form : revForms) triggered' mViol'

-- | Build a 'BidiSummary' by walking a UTF-8-encoded byte range
-- within a 'MutableByteArray'.  Used by 'unparseDomainOpts' to
-- score each label's Bidi contribution by scanning the bytes
-- it just emitted, without freezing the buffer (which is still
-- being built into).  The bytes are well-formed UTF-8 by
-- construction (our writers produced them); the decoder branches
-- on the leading-byte prefix and assumes the indicated number of
-- continuation bytes follow.
bidiSummaryFromMutableUtf8Range
    :: MutableByteArray s -> Int -> Int -> ST s BidiSummary
bidiSummaryFromMutableUtf8Range !mba !start !end = go start emptyBidiSummary
  where
    go !i !sm
      | i >= end  = pure sm
      | otherwise = do
          !b0 <- readByteArray @Word8 mba i
          if | b0 < 0x80 ->
                 let !cp = fromIntegral b0
                 in go (i + 1) (extendBidiSummary sm (bidiClassCp cp))
             | b0 < 0xE0 -> do
                 !b1 <- readByteArray @Word8 mba (i + 1)
                 let !cp = ((fromIntegral b0 .&. 0x1F) `unsafeShiftL` 6)
                       .|.  (fromIntegral b1 .&. 0x3F)
                 go (i + 2) (extendBidiSummary sm (bidiClassCp cp))
             | b0 < 0xF0 -> do
                 !b1 <- readByteArray @Word8 mba (i + 1)
                 !b2 <- readByteArray @Word8 mba (i + 2)
                 let !cp = ((fromIntegral b0 .&. 0x0F) `unsafeShiftL` 12)
                       .|. ((fromIntegral b1 .&. 0x3F) `unsafeShiftL` 6)
                       .|.  (fromIntegral b2 .&. 0x3F)
                 go (i + 3) (extendBidiSummary sm (bidiClassCp cp))
             | otherwise -> do
                 !b1 <- readByteArray @Word8 mba (i + 1)
                 !b2 <- readByteArray @Word8 mba (i + 2)
                 !b3 <- readByteArray @Word8 mba (i + 3)
                 let !cp = ((fromIntegral b0 .&. 0x07) `unsafeShiftL` 18)
                       .|. ((fromIntegral b1 .&. 0x3F) `unsafeShiftL` 12)
                       .|. ((fromIntegral b2 .&. 0x3F) `unsafeShiftL` 6)
                       .|.  (fromIntegral b3 .&. 0x3F)
                 go (i + 4) (extendBidiSummary sm (bidiClassCp cp))

-- | The single-label counterpart to 'domainToUnicode'.  Valid
-- A-labels decode to U-labels.  Any other label renders as ASCII
-- (with escapes for bytes that need them).  See 'unparseLabelOpts'
-- for finer control.
labelToUnicode :: ShortByteString -> Text
labelToUnicode sbs = case unparseLabelOpts forms defaultIdnaFlags sbs of
    Right (t, _) -> t
    Left  _      -> error "labelToUnicode: unreachable"
  where
    !forms = foldMap' labelFormToSet
        [LDH, RLDH, ULABEL, FAKEA, ATTRLEAF, WILDLABEL, OCTET]

-- | The single-label counterpart to 'domainToAscii'.  Output is
-- pure ASCII, A-labels stay literal.  Other labels' wire bytes
-- are output with escapes as needed.  See 'unparseLabelOpts' for
-- finer control.
labelToAscii :: ShortByteString -> Text
labelToAscii sbs = case unparseLabelOpts forms mempty sbs of
    Right (t, _) -> t
    Left  _      -> error "labelToAscii: unreachable"
  where
    !forms = foldMap' labelFormToSet
        [LDH, RLDH, ALABEL, FAKEA, ATTRLEAF, WILDLABEL, OCTET]

-- | Lax A-label decoder: Punycode-decode the body and surface
-- the resulting codepoints unconditionally, /without/ U-label
-- validation or re-encode-and-compare.  Returns @'Left'
-- 'BadPunycode'@ only when the Punycode body itself fails to
-- decode -- no other failure mode is possible without
-- validation.
laxDecode :: forall s. ByteArray -> Int -> ST s (Either AceReason (PrimArray Int))
laxDecode !body !bodyLen = do
    cpBuf <- newPrimArray maxCpsPerLabel
    rDec  <- punycodeDecode body 0 bodyLen cpBuf 0 maxCpsPerLabel
    case rDec of
      Left  _       -> pure (Left BadPunycode)
      Right cpCount -> Right <$> freezeCps cpBuf cpCount

----------------------------------------------------------------------
-- Domain helpers (default strict IDNA2008 options)
----------------------------------------------------------------------

-- | Parse a 'Text' as a domain name and return either the parsed
-- 'Domain' or an 'IdnaError'.  Uses 'idnLabelForms' as the
-- permitted set of label forms @('LDH' | 'ALABEL' | 'ULABEL')@ and
-- 'defaultIdnaFlags' for the parser options.
mkDomain :: Text -> Either IdnaError Domain
mkDomain = fmap fst . parseDomain idnLabelForms

-- | 'mkDomain' for 'String' input.  Equivalent to @'mkDomain' . 'T.pack'@.
mkDomainStr :: String -> Either IdnaError Domain
mkDomainStr = mkDomain . T.pack

-- | 'mkDomain' for a UTF-8 'ByteString'.
mkDomainUtf8 :: ByteString -> Either IdnaError Domain
mkDomainUtf8 = fmap fst . parseDomainUtf8 idnLabelForms defaultIdnaFlags

-- | 'mkDomain' for a UTF-8 'ShortByteString'.
mkDomainShort :: ShortByteString -> Either IdnaError Domain
mkDomainShort = fmap fst . parseDomainShort idnLabelForms defaultIdnaFlags

----------------------------------------------------------------------
-- Tiny utilities
----------------------------------------------------------------------

-- | If @w@ is an ASCII decimal digit, return its numeric value (0..9);
-- otherwise return 'Nothing'.  Returning the value avoids a redundant
-- subtraction at every call site.
asciiDigit :: Word8 -> Maybe Word8
asciiDigit !w
    | d <= 9    = Just d
    | otherwise = Nothing
  where
    !d = w - 0x30
{-# INLINE asciiDigit #-}

isLdhCp :: Int -> Bool
isLdhCp !cp
    | cp >= 0x30, cp <= 0x39 = True
    | cp >= 0x41, cp <= 0x5A = True
    | cp >= 0x61, cp <= 0x7A = True
    | cp == 0x2D             = True
    | otherwise              = False
{-# INLINE isLdhCp #-}

-- | Walk @cpBuf[0..cnt)@ and return 'True' iff any codepoint is
-- ASCII (@< 0x80@) and not in the LDH alphabet.  Used by
-- 'uLabelEncode' to detect labels whose presentation form
-- contains non-LDH ASCII that 'LAXULABEL' must /not/ rescue,
-- regardless of which codepoint the validator surfaced as the
-- first failure.
hasNonLdhAsciiCps :: MutablePrimArray s Int -> Int -> ST s Bool
hasNonLdhAsciiCps !cpBuf !cnt = go 0
  where
    go !i
      | i >= cnt  = pure False
      | otherwise = do
          !cp <- readPrimArray cpBuf i
          if cp < 0x80 && not (isLdhCp cp)
            then pure True
            else go (i + 1)

----------------------------------------------------------------------
-- Case-fold helpers (used by 'MAPCASE')
----------------------------------------------------------------------

-- | Lowercase ASCII A-Z in @cpBuf[0..cnt)@ in place.  Idempotent;
-- non-ASCII codepoints and non-letter ASCII bytes are left alone.
asciiDownCaseBuf :: forall s. MutablePrimArray s Int -> Int -> ST s ()
asciiDownCaseBuf !buf !cnt = go 0
  where
    go :: Int -> ST s ()
    go !i
      | i >= cnt = pure ()
      | otherwise = do
          cp <- readPrimArray buf i
          when (cp >= 0x41 && cp <= 0x5A)
               (writePrimArray buf i (cp + 0x20))
          go (i + 1)

-- | Apply Unicode simple lowercase mapping to every non-ASCII
-- codepoint in @cpBuf[0..cnt)@ in place, using
-- "Text.IDNA2008.Internal.Case"'s 'Case.toLower' -- the
-- @Simple_Lowercase_Mapping@ table pinned to the UCD version this
-- library tracks (see @x-unicode-version@ in @idna2008.cabal@),
-- independent of the host GHC's @base@ vintage.  ASCII codepoints
-- are skipped: the pre-classification 'asciiDownCaseBuf' pass
-- already handled them, and 'Case.toLower' on already-lowercase
-- ASCII is identity.
unicodeDownCaseBuf :: forall s. MutablePrimArray s Int -> Int -> ST s ()
unicodeDownCaseBuf !buf !cnt = go 0
  where
    go :: Int -> ST s ()
    go !i
      | i >= cnt = pure ()
      | otherwise = do
          cp <- readPrimArray buf i
          when (cp >= 0x80) $ do
              let !cp' = ord (Case.toLower (chr cp))
              when (cp' /= cp) (writePrimArray buf i cp')
          go (i + 1)

-- | Apply 'widthMapCp' to every codepoint in @cpBuf[0..cnt)@ in
-- place: codepoints with @\<wide\>@ or @\<narrow\>@ decomposition
-- are replaced by their single-codepoint target.  Used by the
-- 'MAPWIDTH' option (RFC 5895 section 2.2).
unicodeWidthBuf :: forall s. MutablePrimArray s Int -> Int -> ST s ()
unicodeWidthBuf !buf !cnt = go 0
  where
    go :: Int -> ST s ()
    go !i
      | i >= cnt = pure ()
      | otherwise = do
          cp <- readPrimArray buf i
          let !cp' = widthMapCp cp
          when (cp' /= cp) (writePrimArray buf i cp')
          go (i + 1)

-- | Apply the 'MAPUTS46' allow-list ('uts46Lookup') to every
-- codepoint in @cpBuf[0..cnt)@ in place.  Source codepoints in the
-- allow-list are replaced by their target sequence (1:n, where
-- @n >= 1@); other codepoints are left alone.  Returns the new
-- codepoint count.
--
-- If the post-expansion count exceeds 'maxCpsPerLabel', the buffer
-- is left untouched and the returned count is the would-be size,
-- which the caller should treat as 'ErrLabelTooLong'.  Otherwise
-- expansion happens in place, walking right-to-left so the write
-- head never overruns the read head — positions @[0..i-1]@ of the
-- input each contribute @>= 1@ codepoint to the output, so the
-- total output prefix is @>= i@ at all times.
unicodeUTS46Buf
    :: forall s. MutablePrimArray s Int -> Int -> ST s Int
unicodeUTS46Buf !cpBuf !cnt = do
    !hit <- scan 0
    if not hit
      then pure cnt
      else do
        !out <- sizeOut 0 0
        if out > maxCpsPerLabel
          then pure out
          else do
            expandInPlace (cnt - 1) (out - 1)
            pure out
  where
    -- Fast scan: any source codepoint in the allow-list?
    scan :: Int -> ST s Bool
    scan !i
      | i >= cnt = pure False
      | otherwise = do
          cp <- readPrimArray cpBuf i
          case uts46Lookup cp of
            Just _  -> pure True
            Nothing -> scan (i + 1)

    -- Total post-expansion codepoint count.
    sizeOut :: Int -> Int -> ST s Int
    sizeOut !i !acc
      | i >= cnt = pure acc
      | otherwise = do
          cp <- readPrimArray cpBuf i
          let !n = case uts46Lookup cp of
                Just tgts -> length tgts
                Nothing   -> 1
          sizeOut (i + 1) (acc + n)

    -- Walk right-to-left, expanding in place.
    expandInPlace :: Int -> Int -> ST s ()
    expandInPlace !inIdx !outIdx
      | inIdx < 0 = pure ()
      | otherwise = do
          cp <- readPrimArray cpBuf inIdx
          case uts46Lookup cp of
            Nothing -> do
              writePrimArray cpBuf outIdx cp
              expandInPlace (inIdx - 1) (outIdx - 1)
            Just tgts -> do
              let !n = length tgts
              writeTargets cpBuf (outIdx - n + 1) tgts
              expandInPlace (inIdx - 1) (outIdx - n)

    writeTargets :: MutablePrimArray s Int -> Int -> [Int] -> ST s ()
    writeTargets !_   !_ []     = pure ()
    writeTargets !buf !i (x:xs) = do
        writePrimArray buf i x
        writeTargets buf (i + 1) xs

----------------------------------------------------------------------
-- Domain data type
----------------------------------------------------------------------

-- | A wire-form fully-qualified domain name.
--
-- Stored as a 'ShortByteString' to avoid pinning, since the
-- typical name is short and the buffer outlives the parser
-- run.  The encoding is the standard DNS uncompressed wire
-- form: each label is one length byte followed by that many
-- content bytes, with a final zero-length label terminating
-- the name.
--
newtype Domain = Domain_ ShortByteString
    deriving (Eq, Ord, TH.Lift)

instance Show Domain where
    showsPrec p = showsPrec p . T.unpack . domainToAscii

-- | Bidirectional pattern synonym for 'Domain'.
--
-- As a pattern (@case dom of {Domain sbs -> ... }@) it matches any
-- 'Domain' and binds the underlying wire-form 'ShortByteString'.
--
-- As an expression (@Domain sbs@) it builds a 'Domain' from the
-- supplied bytes after checking that they form a well-formed DNS
-- wire encoding.  Malformed input raises an error; the input can
-- be pre-validated via 'isValidWireForm'.  The parser functions
-- never produce malformed wire bytes.
pattern Domain :: ShortByteString -> Domain
pattern Domain sbs <- Domain_ sbs
  where
    Domain sbs
      | isValidWireForm sbs = Domain_ sbs
      | otherwise = errorWithoutStackTrace
          "Text.IDNA2008.Domain: malformed wire bytes"

{-# COMPLETE Domain #-}

-- | The whole wire-form buffer as a 'ShortByteString'.
-- Zero-copy.
wireBytesShort :: Domain -> ShortByteString
wireBytesShort (Domain sbs) = sbs

-- | The whole wire-form buffer as a regular 'ByteString'.
-- Zero-copy.
wireBytes :: Domain -> ByteString
wireBytes (Domain sbs) = SBS.fromShort sbs

-- | Walk the wire form and yield each label's content bytes,
-- skipping the leading length byte and stopping at the
-- terminal zero-length root label.
toLabels :: Domain -> [ShortByteString]
toLabels (Domain sbs) = toLabelsFromWire sbs

-- | Wire-bytes variant of 'toLabels': accepts a 'ShortByteString'
-- assumed to be valid DNS wire form (caller's responsibility, no
-- validation performed) and yields each label's content bytes.
toLabelsFromWire :: ShortByteString -> [ShortByteString]
toLabelsFromWire sbs@(SBS sba) = go 0
  where
    sblen = SBS.length sbs
    ba  = ByteArray sba
    go !off
        | off < sblen
        , llen <- fromIntegral (A.indexByteArray ba off :: Word8)
        , off' <- off + llen + 1
        = if | llen > 0 && llen < 64 && off' < sblen
             , (ByteArray lba) <- A.cloneByteArray ba (off + 1) llen
             -> SBS lba : go off'
             | llen == 0 && off' == sblen -> []
             | otherwise -> error "Invalid wire form domain"
        | otherwise = error "Invalid wire form domain"

-- | Check that the supplied 'ShortByteString' is a well-formed
-- DNS wire-form name:
--
--   * overall length is in @[1, 255]@ (DNS message-section cap
--     on a single name);
--   * the final byte is the @NUL@ root terminator;
--   * every other length byte is in @[1, 63]@ (no compression
--     pointers, no reserved length-byte values, no zero-length
--     non-root labels);
--   * the label walk lands exactly on the terminator (no
--     truncation, no overrun, no trailing junk past the root).
--
isValidWireForm :: ShortByteString -> Bool
isValidWireForm sbs@(SBS sba) =
    n >= 1 && n <= 255 && walk 0
  where
    !ba = ByteArray sba
    !n  = SBS.length sbs
    walk !off
      | off >= n   = False
      | llen == 0  = off == n - 1
      | llen <= 63 = walk (off + llen + 1)
      | otherwise  = False
      where
        !llen = fromIntegral (A.indexByteArray ba off :: Word8) :: Int

----------------------------------------------------------------------
-- Literal domains (TH splices)
----------------------------------------------------------------------

-- | Template-Haskell typed splice for a compile-time 'Domain'
-- literal.  The parser argument is any
-- @'T.Text' -> Either e Domain@ function, so a single primitive
-- subsumes every kind of literal the library can express.
-- Typical idioms:
--
-- > -- Strict default: use 'mkDomain' directly.
-- > dom :: Domain
-- > dom = $$(dnLit mkDomain "example.com")
--
-- > -- Custom flags via @(parseDomainOpts forms flags)@:
-- > dom :: Domain
-- > dom = $$(let forms = idnLabelForms
-- >              flags = defaultIdnaFlags <> allIdnaMappings
-- >              parse = fmap fst . parseDomainOpts forms flags
-- >           in dnLit parse "Example.COM")
--
-- > -- Permit an attrleaf or other less-restrictive form set:
-- > dom :: Domain
-- > dom = $$(let forms = idnLabelForms <+> ATTRLEAF
-- >              flags = defaultIdnaFlags
-- >              parse = fmap fst . parseDomainOpts forms flags
-- >           in dnLit parse "_dmarc.example.com")
--
-- The parser only performs whatever checks its own flags enable.
-- The cross-label half of RFC 5893 is a presentation-time
-- concern and is not enforced by 'parseDomain' /
-- 'parseDomainOpts'; callers wanting it at literal time should
-- wrap their parser with a post-parse call to 'unparseDomainOpts'
-- and raise any error as a parse failure.
--
-- The emitted splice is a constant 'Domain' value: no runtime
-- IDNA work.
dnLit :: forall e m. (Show e, MonadFail m, TH.Quote m)
      => (T.Text -> Either e Domain)
      -> String
      -> TH.Code m Domain
dnLit parse s = TH.joinCode case parse (T.pack s) of
    Left e  -> fail $ "Invalid domain-name literal " ++ show s
                   ++ ": " ++ show e
    Right d -> pure (TH.liftTyped d)
