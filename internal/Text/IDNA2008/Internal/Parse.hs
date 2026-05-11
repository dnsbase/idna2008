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
-- (RFC 5892 Appendix A.1\/A.2).  Opt-in @NFCCHECK@ adds a
-- conservative @NFC_Quick_Check@ pass over the decoded codepoints
-- (RFC 5891 section 5.3); @Maybe@ is treated as a definite reject in the
-- absence of a full normaliser.  Opt-in @BIDICHECK@ adds the per-label
-- subset of RFC 5893 Rules 1-6; the cross-label part of the rule set is
-- treated as a presentation-time concern and intentionally not enforced
-- in the parser.
--
-- Classification precedence: any codepoint @> 0xFF@ forces the U-label
-- path (since such a codepoint cannot fit in an octet); otherwise, any
-- non-LDH ASCII byte prefers the OCTET path (since the resulting
-- Punycode-encoded form would not be a valid LDH A-label); pure
-- Latin-1 input (codepoints @0x80..0xFF@ with no non-LDH ASCII) still
-- goes through the U-label path.
--
-- A-label classification is loose in this module: any syntactically
-- valid @\"xn--\"@-prefixed LDH label is reported as 'ALABEL'.  Strict
-- round-trip validation (Punycode decode + U-label re-validate +
-- re-encode and compare) is added in a later commit; until then,
-- 'FAKEA' is never produced.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.IDNA2008.Internal.Parse
    ( -- * Domain parsers
      parseDomain
    , parseDomainOpts
    , parseDomainUtf8
    , parseDomainShort

      -- * Domain helpers (Maybe-returning)
    , mkDomain
    , mkDomainStr
    , mkDomainUtf8
    , mkDomainShort

      -- * Domain literals (TH splices)
    , dnLit
    , dnLitAs

      -- * Domain display forms
    , asciiFromWire
    , unicodeFromWire
    , unicodeOptFromWire
    , unicodeLaxFromWire
    , domainToAscii
    , domainToUnicode
    , domainToUnicodeOpt
    , domainToUnicodeLax
    , labelToAscii
    , labelToUnicode
    , labelToUnicodeLax
    ) where

import qualified Data.ByteString.Short as SBS
import qualified Data.Char as Char
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Array.Byte (ByteArray(..))
import Data.Bits ((.|.), (.&.), testBit, unsafeShiftL, unsafeShiftR)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Char (chr, ord)
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

import Text.IDNA2008.Internal.Domain
    ( Domain(Domain, Domain_), toLabelsFromWire, wireBytes )
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
    ( IdnaFlags
    , pattern ALABELCHECK
    , pattern NFCCHECK
    , pattern EMOJIOK
    , pattern MAPDOTS
    , pattern MAPNFC
    , pattern MAPCASE
    , pattern MAPWIDTH
    , pattern BIDICHECK
    , pattern ASCIIFALLBACK
    , pattern LAXDECODE
    , defaultIdnaFlags
    , effectiveIdnaFlags
    , meetsIdnaFlags
    , withoutIdnaFlags
    )
import Text.IDNA2008.Internal.Emoji (isEmoji)
import Text.IDNA2008.Internal.Error
import Text.IDNA2008.Internal.LabelForm
    ( LabelForm
    , pattern LDH, pattern RLDH, pattern FAKEA, pattern ALABEL
    , pattern ULABEL, pattern ATTRLEAF, pattern OCTET, pattern WILDLABEL
    )
import Text.IDNA2008.Internal.LabelFormSet
    ( LabelFormSet, memberLabelFormSet, hostnameLabelForms )
import Text.IDNA2008.Internal.LabelInfo
    ( LabelInfo, mkLabelInfo )
import Text.IDNA2008.Internal.Property
    ( IdnaDisposition(..)
    , idnaDisposition
    )
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
import Text.IDNA2008.Internal.Width
    ( widthMapCp
    )
import Text.IDNA2008.Internal.Util (baToShortByteString, sbsToByteArray)

----------------------------------------------------------------------
-- Buffer sizes
----------------------------------------------------------------------

-- | Maximum wire form length, RFC 1035 section 3.1.
maxWireLen :: Int
maxWireLen = 255

-- | Maximum wire octets in a single label.
maxLabelLen :: Int
maxLabelLen = 63

-- | Maximum codepoints we admit in a single label before encoding.
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

-- | Parse a 'Text' value as a presentation-form domain name, with
-- the caller specifying the set of acceptable label forms.
-- Uses 'defaultIdnaFlags'; for explicit options use
-- 'parseDomainOpts'.
--
-- 'Text' is already guaranteed well-formed UTF-8 by its own
-- constructor invariant, so the parser views the underlying
-- 'TA.Array' directly without an intermediate copy.  For
-- callers who hold their input as raw UTF-8 bytes (from a
-- socket, a file, or any decoder that hasn't already produced
-- 'Text'), 'parseDomainUtf8' and 'parseDomainShort' skip the
-- 'Text' construction step.
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
    -- 'Data.Text.Array.Array' wraps a raw 'ByteArray#'; 'Data.Array.Byte.ByteArray'
    -- is the corresponding lifted wrapper used by 'Data.Primitive.ByteArray'.
    textArrayToByteArray :: TA.Array -> ByteArray
    textArrayToByteArray (TA.ByteArray b) = ByteArray b

-- | Parse a UTF-8 'ByteString' as a presentation-form domain
-- name.  Convenience entry point for callers who already hold
-- the input as raw UTF-8 bytes (typically from a network read,
-- a file, or a protocol decoder) and would otherwise pay a
-- @decodeUtf8@ to reach 'parseDomain'.  Uses 'defaultIdnaFlags'.
--
-- The bytes are copied once: 'ByteString' is a pinned foreign-
-- pointer buffer, and the parser operates on an unpinned
-- 'ByteArray'.  For a zero-copy variant when the caller already
-- has a 'ShortByteString', use 'parseDomainShort'.
--
-- The input is /not/ assumed to be well-formed UTF-8.  The
-- parser performs a strict RFC 3629 decode pass and returns
-- @'Left' ('ErrInvalidUtf8' loc)@ on any ill-formed sequence:
-- bad continuation bytes, overlong encodings, surrogates
-- (@U+D800@..@U+DFFF@), and codepoints @> U+10FFFF@ are all
-- rejected, with @loc@ giving the byte offset.  Bytes produced
-- by Haskell\'s standard UTF-8 encoders (@Data.Text.Encoding@
-- and the like) are canonical UTF-8 and pass unconditionally;
-- the strictness matters for bytes of unknown provenance.
parseDomainUtf8
    :: LabelFormSet                  -- ^ Permitted label forms
    -> ByteString                    -- ^ UTF-8 presentation form
    -> Either IdnaError (Domain, LabelInfo)
parseDomainUtf8 !allowed bs = parseDomainShort allowed $! SBS.toShort bs

-- | Parse a UTF-8 'ShortByteString' as a presentation-form
-- domain name.  Semantically identical to 'parseDomainUtf8' but
-- zero-copy: 'ShortByteString' shares the 'ByteArray#'
-- representation the parser operates on, so the bytes are
-- passed through directly.  Uses 'defaultIdnaFlags'.
--
-- UTF-8 handling and ill-formed-input behaviour are the same
-- as for 'parseDomainUtf8'; see the notes there.
parseDomainShort
    :: LabelFormSet                  -- ^ Permitted label forms
    -> ShortByteString               -- ^ UTF-8 presentation form
    -> Either IdnaError (Domain, LabelInfo)
parseDomainShort !allowed sb =
    parseDomainView (sbsToByteArray sb) 0 (SBS.length sb)
                    allowed defaultIdnaFlags

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
    | inLen < 0 = Left (ErrInvalidUtf8 noLoc Nothing)
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
                frozen <- unsafeFreezeByteArray resBA
                let !info = mkLabelInfo (reverse produced)
                -- The parser builds the wire form by construction
                -- (length bytes capped at 63, trailing NUL appended,
                -- total length <= maxWireLen).  Use the bare data
                -- constructor to skip the redundant validation that
                -- the 'Domain' pattern would perform.
                pure (Right (Domain_ (baToShortByteString frozen), info))

----------------------------------------------------------------------
-- Driver
----------------------------------------------------------------------

-- | Codepoints that the @MAPDOTS@ option treats as additional label
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
          pure (Left (ErrLabelTooLong (atLabel lIdx) (cpCount + 1)))
      | otherwise = do
          writePrimArray cpBuf cpCount cp
          go iPos oPos lStart (cpCount + 1) hasEscape lIdx produced

    handleEscape !iPos !oPos !lStart !cpCount !lIdx !produced
      | iPos + 1 >= inEnd =
          pure (Left (ErrBadEscape (atLabel lIdx) (Just iPos)))
      | otherwise =
          let !b1 = indexByteArray input (iPos + 1) :: Word8
          in case asciiDigit b1 of
               Just !v1
                 -- \DDD numeric escape
                 | iPos + 4 > inEnd ->
                     pure (Left (ErrBadEscape (atLabel lIdx) (Just iPos)))
                 | otherwise ->
                     let !b2 = indexByteArray input (iPos + 2) :: Word8
                         !b3 = indexByteArray input (iPos + 3) :: Word8
                     in case (asciiDigit b2, asciiDigit b3) of
                          (Just v2, Just v3) ->
                            let !v =   100 * fromIntegral v1
                                    +   10 * fromIntegral v2
                                    +        fromIntegral v3 :: Int
                            in if v > 0xFF
                                 then pure (Left (ErrBadEscape (atLabel lIdx) (Just iPos)))
                                 else
                                    appendCp v (iPos + 4) oPos lStart cpCount
                                             True lIdx produced
                          _ -> pure (Left (ErrBadEscape (atLabel lIdx) (Just iPos)))
               Nothing
                 | b1 < 0x80 ->
                     appendCp (fromIntegral b1) (iPos + 2)
                              oPos lStart cpCount True lIdx produced
                 | otherwise ->
                     case decodeUtf8At input inEnd (iPos + 1) of
                       Left e -> pure (Left e)
                       Right (cp, nextPos)
                         | cp > 0xFF ->
                             pure (Left (ErrBadEscape (atLabel lIdx) (Just iPos)))
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
            else pure (Left (ErrEmptyLabel (atLabel lIdx)))
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
    | p >= inEnd = Left (ErrInvalidUtf8 noLoc (Just p))
    | otherwise =
        let !b0 = indexByteArray input p :: Word8
        in if | b0 < 0x80 -> Right (fromIntegral b0, p + 1)
              | b0 < 0xC2 -> Left  (ErrInvalidUtf8 noLoc (Just p))
              | b0 < 0xE0 -> two p b0
              | b0 < 0xF0 -> three p b0
              | b0 < 0xF5 -> four p b0
              | otherwise -> Left  (ErrInvalidUtf8 noLoc (Just p))
  where
    contBad b = b < 0x80 || b >= 0xC0

    two !p0 !b0
        | p0 + 1 >= inEnd =
            Left (ErrInvalidUtf8 noLoc (Just p0))
        | otherwise =
            let !b1 = indexByteArray input (p0 + 1) :: Word8
            in if contBad b1
                 then Left (ErrInvalidUtf8 noLoc (Just p0))
                 else
                   let !cp = ((fromIntegral b0 - 0xC0) `unsafeShiftL` 6)
                          .|. (fromIntegral b1 - 0x80) :: Int
                   in Right (cp, p0 + 2)

    three !p0 !b0
        | p0 + 2 >= inEnd =
            Left (ErrInvalidUtf8 noLoc (Just p0))
        | otherwise =
            let !b1 = indexByteArray input (p0 + 1) :: Word8
                !b2 = indexByteArray input (p0 + 2) :: Word8
            in if contBad b1 || contBad b2
                 then Left (ErrInvalidUtf8 noLoc (Just p0))
                 else
                   let !cp = ((fromIntegral b0 - 0xE0) `unsafeShiftL` 12)
                          .|. ((fromIntegral b1 - 0x80) `unsafeShiftL` 6)
                          .|. (fromIntegral b2 - 0x80) :: Int
                   in if cp < 0x800 || (cp >= 0xD800 && cp <= 0xDFFF)
                        then Left (ErrInvalidUtf8 noLoc (Just p0))
                        else Right (cp, p0 + 3)

    four !p0 !b0
        | p0 + 3 >= inEnd =
            Left (ErrInvalidUtf8 noLoc (Just p0))
        | otherwise =
            let !b1 = indexByteArray input (p0 + 1) :: Word8
                !b2 = indexByteArray input (p0 + 2) :: Word8
                !b3 = indexByteArray input (p0 + 3) :: Word8
            in if contBad b1 || contBad b2 || contBad b3
                 then Left (ErrInvalidUtf8 noLoc (Just p0))
                 else
                   let !cp = ((fromIntegral b0 - 0xF0) `unsafeShiftL` 18)
                          .|. ((fromIntegral b1 - 0x80) `unsafeShiftL` 12)
                          .|. ((fromIntegral b2 - 0x80) `unsafeShiftL` 6)
                          .|. (fromIntegral b3 - 0x80) :: Int
                   in if cp < 0x10000 || cp > 0x10FFFF
                        then Left (ErrInvalidUtf8 noLoc (Just p0))
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
    | cpCount == 0 = pure (Left (ErrEmptyLabel (atLabel lIdx)))
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
                   then pure (Left (ErrUnpresentableLabel (atLabel lIdx)))
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
                  && (first == 0x78 || first == 0x58)
                  && (p1 == 0x6E || p1 == 0x4E)
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
        pure (Left (ErrFormNotAllowed (atLabel lIdx) form))
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
                  pure (Left (ErrAceInvalid (atLabel lIdx) reason))

----------------------------------------------------------------------
-- U-label path: validate codepoints, Punycode-encode, emit "xn--...".
----------------------------------------------------------------------

-- | Outcome of inspecting the codepoint buffer after the RFC 5895
-- per-label mappings (case-fold + width + NFC) have run.  An input
-- that originally had at least one non-ASCII codepoint may, after
-- mapping, contain only ASCII (typically when @MAPWIDTH@ collapsed
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
        -- @MAPCASE@, walk the codepoint buffer and apply Unicode
        -- toLower to any non-ASCII codepoint with a defined case
        -- mapping.  ASCII A-Z were already lower-cased pre-
        -- classification by 'flushLabel'; here we cover the rest.
        when (MAPCASE `meetsIdnaFlags` opts)
             (unicodeDownCaseBuf cpBuf cpCount)
        -- RFC 5895 section 2.2 mapping: under @MAPWIDTH@, walk the
        -- codepoint buffer and replace each fullwidth\/halfwidth
        -- codepoint with its single-codepoint decomposition target.
        -- Bits in the codepoint buffer that are not in the wide\/
        -- narrow set are left alone.  Label-separator targets
        -- (U+002E, U+3002) are handled at split time; @MAPDOTS@
        -- is implied by 'effectiveIdnaFlags' so they cannot reach
        -- this pass.
        when (MAPWIDTH `meetsIdnaFlags` opts)
             (unicodeWidthBuf cpBuf cpCount)
        -- RFC 5895 section 2.3 mapping: under @MAPNFC@, normalise the
        -- codepoint buffer to NFC in place before validation, so a
        -- decomposed input (e.g. @"a" + U+0301@) parses as the
        -- precomposed equivalent (here @U+00E1@).  Off by default;
        -- when unset 'normalizeNFC' is not invoked.
        --
        -- After normalising we strip @NFCCHECK@ from the opts
        -- handed to 'validateULabelCps': the buffer is now in NFC
        -- by construction, so the validator's 'isNFC' check would
        -- be a no-op.  A-labels (whose codepoints come from
        -- 'alabelRoundTrip', which receives the original opts)
        -- still get the @NFCCHECK@ check on their decoded form.
        (!cnt, !opts') <-
            if MAPNFC `meetsIdnaFlags` opts
              then do
                  n <- normalizeNFC cpBuf cpCount
                  pure (n, opts `withoutIdnaFlags` NFCCHECK)
              else pure (cpCount, opts)
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
              pure (Left (ErrLabelInvalid (atLabel lIdx)
                                       (DisallowedCodepoint cp)))
          AsciiBadHyphens ->
              pure (Left (ErrLabelInvalid (atLabel lIdx)
                                       HyphenViolation))
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
      Just reason -> pure (Left (ErrLabelInvalid (atLabel lIdx) reason))
      Nothing
        | not (ULABEL `memberLabelFormSet` allowed) ->
            pure (Left (ErrFormNotAllowed (atLabel lIdx) ULABEL))
        | otherwise -> do
            let !prefAt = lStart + 1
            writeByteArray outBuf  prefAt      (0x78 :: Word8) -- 'x'
            writeByteArray outBuf (prefAt + 1) (0x6E :: Word8) -- 'n'
            writeByteArray outBuf (prefAt + 2) (0x2D :: Word8) -- '-'
            writeByteArray outBuf (prefAt + 3) (0x2D :: Word8) -- '-'
            cps <- freezeCps cpBuf cnt
            let !bodyAt  = prefAt + 4
                !bodyCap = maxLabelLen - 4
            enc <- punycodeEncode cps cnt outBuf bodyAt bodyCap
            case enc of
              Left _ ->
                pure (Left (ErrPunycodeOverflow (atLabel lIdx)))
              Right encLen ->
                let !contentLen = 4 + encLen
                    !newOff     = lStart + 1 + contentLen
                in if contentLen > maxLabelLen
                     then pure (Left (ErrLabelTooLong (atLabel lIdx) contentLen))
                     else if newOff >= maxWireLen
                       then pure (Left (ErrNameTooLong (newOff + 1)))
                       else do
                         writeByteArray outBuf lStart
                                        (fromIntegral contentLen :: Word8)
                         pure (Right (newOff, ULABEL : produced))

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
--   * Under @NFCCHECK@, full canonical normalisation is performed
--     and the result compared against the input; the label is
--     rejected if it differs.  Off by default.
--   * @DISALLOWED@ \/ @UNASSIGNED@ codepoints are rejected.  Under
--     @EMOJIOK@, a non-ASCII @DISALLOWED@ codepoint with
--     @Emoji=Yes@ is admitted as if @PVALID@ (a diagnostic
--     relaxation; @UNASSIGNED@ is still rejected).
--
-- Note: the per-label RFC 5893 (Bidi) check, gated on @BIDICHECK@,
-- runs separately from this function -- see 'checkBidiLabel' in
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
              -- Diagnostic relaxation: under @EMOJIOK@ admit a
              -- non-ASCII DISALLOWED codepoint that carries the
              -- Unicode @Emoji=Yes@ property.  Restricted to
              -- non-ASCII so that ASCII punctuation that happens
              -- to be @Emoji=Yes@ (e.g. @\'#\'@ U+0023, @\'*\'@
              -- U+002A) stays disallowed.  Emoji codepoints are
              -- not @CONTEXTJ@/@CONTEXTO@, so no contextual check
              -- is needed on the relaxation path.
              | cp >= 0x80, EMOJIOK `meetsIdnaFlags` opts, isEmoji cp ->
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
-- 'checkBidiSummary' (in trigger-on mode -- per-label semantics)
-- that lifts the rule-violation payload into 'LabelReason' for
-- the parser-time error path.  See 'checkBidiSummary' for the
-- semantics of the rule application.
checkBidiLabel :: BidiSummary -> Maybe LabelReason
checkBidiLabel !s = LabelBidi <$> checkBidiSummary False s

----------------------------------------------------------------------
-- A-label strict round-trip
----------------------------------------------------------------------

-- | Strict A-label round-trip check.  Given the lower-cased Punycode
-- body of an @\"xn--\"@-prefixed label (i.e. the bytes /after/ the
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
    -> IdnaFlags                 -- ^ Threaded through to the U-label
                                --   validator (so NFCCHECK, when
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

-- | Render a 'Domain' as Unicode display 'Text', with @\'.\'@ as
-- the label separator.  An A-label is replaced by its U-label
-- codepoints; any other label (LDH, FAKEA, OCTET, ...) is
-- rendered byte-by-byte under the same escaping policy as
-- 'domainToAscii'.
--
-- No cross-label Bidi check is performed; use
-- 'domainToUnicodeOpt' for that.  Use 'domainToAscii' to keep
-- every label in ASCII form, with A-labels left literal rather
-- than decoded.
domainToUnicode :: Domain -> Text
domainToUnicode (Domain_ sbs) = unicodeFromWire sbs

-- | Wire-bytes variant of 'domainToUnicode'.  Caller must ensure
-- @sbs@ is a well-formed DNS wire-form domain name (label-length
-- prefixes, total length, root terminator); behaviour is undefined
-- otherwise.
unicodeFromWire :: ShortByteString -> Text
unicodeFromWire sbs = case toLabelsFromWire sbs of
    [] -> T.singleton '.'   -- root domain
    ls -> renderUnicodeText (strictDecode mempty) (SBS.length sbs - 2) ls

special :: Word8 -> Bool
special b = testBit tab $ fromIntegral (b - 0x22)
  where
    -- Status of 59 bytes from '"' to '\\', bit set means "special".
    tab :: Word64
    tab = 0b0100_0000_0000_0000_0000_0000_0000_0100_0010_0000_0000_0001_0000_1100_0101

-- | Render a 'Domain' as its ASCII presentation form. every
-- Printable ASCII characters that are /special/ in DNS zone files are
-- backslash-escaped, while bytes @<= 0x20@, @0x7F@, or @>= 0x80@ are
-- emitted as decimal triples @\\DDD@.
--
-- Labels are separated by @\'.\'@; the bare root renders as @\".\"@.
-- A non-root domain has /no/ trailing dot in the output; callers
-- wanting the conventional zone-file FQDN trailing dot can append
-- it themselves.
--
-- Contrast with 'domainToUnicode', which decodes ACE labels back
-- to their U-label codepoints (so the output is the user-typed
-- non-ASCII form).  'domainToAscii' deliberately does no such
-- decoding: an A-label on the wire stays in @xn--@ form, which
-- is what callers usually want for DNS-bound output.
domainToAscii :: Domain -> Text
domainToAscii (Domain_ sbs) = asciiFromWire sbs

-- | Wire-bytes variant of 'domainToAscii'.  Same trust contract
-- as 'unicodeFromWire'.
asciiFromWire :: ShortByteString -> Text
asciiFromWire sbs = case toLabelsFromWire sbs of
    [] -> T.singleton '.'
    ls ->
        -- Wire form has one length byte per label plus a trailing
        -- root zero; presentation form has @n - 1@ dots between
        -- labels and no terminator.  So @wireLen - 2@ is the
        -- exact unescaped-output length for a non-root domain.
        renderAsciiText (SBS.length sbs - 2) ls

-- | Build the dotted ASCII presentation form into a single
-- 'Text'-shaped buffer.  @sizeEst@ is the initial capacity (a
-- tight estimate avoids a copy; under-estimates pay only log-many
-- 'resizeMutableByteArray' calls).  Pre-condition: @labels@ is
-- non-empty (the bare-root case is handled by the caller).
renderAsciiText :: Int -> [ShortByteString] -> Text
renderAsciiText !sizeEst labels = runST do
    let !cap0 = max sizeEst 16
    mba0 <- newByteArray cap0
    (mba, !finalLen) <- driveLabels mba0 cap0 0 True labels
    -- In-place shrink; no allocation when slack is small.
    shrinkMutableByteArray mba finalLen
    ByteArray b <- unsafeFreezeByteArray mba
    pure (Text (TA.ByteArray b) 0 finalLen)
  where
    driveLabels !mba !_   !cur _     []         = pure (mba, cur)
    driveLabels !mba !cap !cur first (lbl:rest) = do
        (mba', cap', cur') <-
            if first then pure (mba, cap, cur)
                     else writeRawByte mba cap cur 0x2E
        (mba'', cap'', cur'') <- writeEscapedLabel mba' cap' cur' lbl
        driveLabels mba'' cap'' cur'' False rest

-- | Reserve at least @need@ bytes of headroom in the buffer,
-- growing it via 'resizeMutableByteArray' if necessary.  The
-- growth schedule doubles the capacity (or jumps directly to
-- @cur + need@, whichever is larger), bounding amortised resize
-- cost to @O(1)@ per byte written.
ensureCapacity
    :: MutableByteArray s -> Int -> Int -> Int
    -> ST s (MutableByteArray s, Int)
ensureCapacity !mba !cap !cur !need
    | cur + need <= cap = pure (mba, cap)
    | otherwise = do
        let !cap' = max (cur + need) (cap + cap `div` 2)
        mba' <- resizeMutableByteArray mba cap'
        pure (mba', cap')

-- | Grow if needed, then write a single raw byte at the cursor.
writeRawByte
    :: MutableByteArray s -> Int -> Int -> Word8
    -> ST s (MutableByteArray s, Int, Int)
writeRawByte !mba !cap !cur !b = do
    (mba', cap') <- ensureCapacity mba cap cur 1
    writeByteArray mba' cur b
    pure (mba', cap', cur + 1)

-- | Walk a label's wire bytes, writing each (with escape as
-- needed) into the buffer.
writeEscapedLabel
    :: MutableByteArray s -> Int -> Int -> ShortByteString
    -> ST s (MutableByteArray s, Int, Int)
writeEscapedLabel !mba0 !cap0 !cur0 !sbs = step mba0 cap0 cur0 0
  where
    !arr = sbsToByteArray sbs
    !n   = SBS.length sbs
    step !mba !cap !cur !i
      | i >= n = pure (mba, cap, cur)
      | otherwise = do
          let !b = indexByteArray arr i :: Word8
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

-- | Build the dotted Unicode presentation form into a single
-- 'Text'-shaped buffer.  @sizeEst@ is an initial capacity (the
-- @wireLen - 2@ estimate of the ASCII path also works here: it
-- is exact for the no-escape ASCII case and the doubling growth
-- absorbs the expansion when an ACE label decodes to a U-label).
-- @decode@ is the caller's A-label decoder: 'strictDecode' uses
-- it for the standard round-trip-validating renderer,
-- 'laxDecode' for the diagnostic variant.  Pre-condition:
-- @labels@ is non-empty.
renderUnicodeText
    :: (forall s. ByteArray -> Int -> ST s (Maybe (PrimArray Int)))
    -> Int                          -- ^ initial capacity estimate
    -> [ShortByteString]            -- ^ labels (non-empty)
    -> Text
renderUnicodeText _      !_       []     = T.singleton '.'
renderUnicodeText decode !sizeEst labels = runST go
  where
    go :: forall s. ST s Text
    go = do
        let !cap0 = max sizeEst 16
        mba0 <- newByteArray cap0
        (mba, !finalLen) <- driveLabels mba0 cap0 0 True labels
        shrinkMutableByteArray mba finalLen
        ByteArray b <- unsafeFreezeByteArray mba
        pure (Text (TA.ByteArray b) 0 finalLen)
      where
        driveLabels :: MutableByteArray s
                    -> Int
                    -> Int
                    -> Bool
                    -> [ShortByteString]
                    ->  ST s (MutableByteArray s, Int)
        driveLabels !mba !_   !cur _     []         = pure (mba, cur)
        driveLabels !mba !cap !cur first (lbl:rest) = do
            (mba', cap', cur') <-
                if first then pure (mba, cap, cur)
                         else writeRawByte mba cap cur 0x2E
            (mba'', cap'', cur'') <-
                writeRenderedUnicodeLabel decode mba' cap' cur' lbl
            driveLabels mba'' cap'' cur'' False rest

-- | Render a label into the buffer in the Unicode-flavoured way.
-- An ACE label (@\"xn--\"@-prefixed) whose body the caller's
-- @decode@ accepts is emitted as its decoded codepoints in UTF-8;
-- any other label falls through to 'writeEscapedLabel' (sharing
-- the ASCII path's byte-escape machinery), so non-ACE labels and
-- ACE labels whose decode fails render identically across the
-- Unicode and ASCII paths.
writeRenderedUnicodeLabel
    :: (forall t. ByteArray -> Int -> ST t (Maybe (PrimArray Int)))
    -> MutableByteArray s -> Int -> Int -> ShortByteString
    -> ST s (MutableByteArray s, Int, Int)
writeRenderedUnicodeLabel decode !mba !cap !cur !sbs
    | not (hasAcePrefix labelArr labelLen) =
        writeEscapedLabel mba cap cur sbs
    | otherwise = do
        -- Lower-case the body, decode.
        mBuf <- newByteArray bodyLen
        let copyLc !i
              | i >= bodyLen = pure ()
              | otherwise = do
                  let !b = indexByteArray labelArr (4 + i) :: Word8
                      !b'
                        | b >= 0x41 && b <= 0x5A = b + 0x20
                        | otherwise              = b
                  writeByteArray mBuf i b'
                  copyLc (i + 1)
        copyLc 0
        body <- unsafeFreezeByteArray mBuf
        mCps <- decode body bodyLen
        case mCps of
            Just cps -> writeCodepointsUtf8 mba cap cur cps
            Nothing  -> writeEscapedLabel mba cap cur sbs
  where
    !labelArr = sbsToByteArray sbs
    !labelLen = SBS.length sbs
    !bodyLen  = labelLen - 4

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
-- @\\C@ or @\\DDD@ treatment.  This matters under 'LAXDECODE'
-- and via 'labelToUnicodeLax', where a FAKEA Punycode body may
-- decode to codepoints like @\'.\'@, @\';\'@, or @\'\@\'@ that
-- 'strictDecode' would have rejected.  Multi-byte UTF-8
-- encodings of codepoints @>= 0x80@ are emitted verbatim;
-- continuation bytes are all @>= 0x80@ and carry no
-- per-byte zone-file meaning.
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

-- | Render a 'Domain' as Unicode display 'Text' under caller-
-- supplied 'IdnaFlags'.  Without any flag set, the result is
-- equivalent to 'domainToUnicode' wrapped in 'Right'.  The
-- flags (combined monoidally) are described below.
--
-- /Per-label content/ ('EMOJIOK', 'NFCCHECK'):
--
-- These tweak what the strict per-label round-trip will accept.
-- Under 'EMOJIOK', a label whose Punycode body decodes to an
-- emoji codepoint passes the round-trip and renders as its
-- decoded form; the default rejects emoji and the label
-- renders in its literal A-label spelling.  'NFCCHECK'
-- additionally requires the decoded form to be in NFC: a
-- 'Domain' built by parsing a presentation form has already
-- had NFC enforced at parse time, so the render-time check is
-- redundant; for a 'Domain' built from raw wire bytes of
-- unknown provenance (e.g. a network read), 'NFCCHECK' at
-- render time is the only enforcement that runs.
--
-- /Per-label decoding/ ('LAXDECODE'):
--
-- Switches the per-label decoder from the strict one used by
-- 'domainToUnicode' to the one used by 'domainToUnicodeLax'.
-- See the warnings there: output may not round-trip, and may
-- fail to re-encode.  The cross-label Bidi machinery still
-- runs on the laxly-decoded text when 'BIDICHECK' is set, so
-- the check matches what the reader will actually see.
--
-- /Cross-label Bidi/ ('BIDICHECK', 'ASCIIFALLBACK'):
--
-- 'BIDICHECK' applies the cross-label half of RFC 5893: once
-- any label has @R@\/@AL@\/@AN@ content, Rules 1-6 of section 2
-- are required of /every/ label, including pure-LTR siblings
-- like @\"_tcp\"@ or all-digit names.  A failure returns
-- @'Left' ('ErrCrossLabelBidi' i rule)@ naming the first
-- offending label (zero-based @i@) and the rule that failed.
--
-- 'ASCIIFALLBACK' (which implies 'BIDICHECK') downgrades that
-- failure to a successful 'Right' with the entire domain
-- rendered as 'domainToAscii' would render it --- pure ASCII,
-- no Bidi-relevant codepoints, safe to embed in any-direction
-- text.  Useful for display contexts where "show something
-- readable" beats "report a validation failure".
domainToUnicodeOpt
    :: IdnaFlags
    -> Domain
    -> Either IdnaError Text
domainToUnicodeOpt opts (Domain_ sbs) = unicodeOptFromWire opts sbs

-- | Wire-bytes variant of 'domainToUnicodeOpt'.  Same trust
-- contract as 'unicodeFromWire'.
unicodeOptFromWire
    :: IdnaFlags
    -> ShortByteString
    -> Either IdnaError Text
unicodeOptFromWire !optsIn sbs =
    let !opts    = effectiveIdnaFlags optsIn
        !labels  = toLabelsFromWire sbs
        !decode  | LAXDECODE `meetsIdnaFlags` opts = laxDecode
                 | otherwise                       = strictDecode opts
        !sizeEst = SBS.length sbs - 2
    in if not (BIDICHECK `meetsIdnaFlags` opts)
         then Right (renderUnicodeText decode sizeEst labels)
         else case renderUnicodeTextChecked decode sizeEst labels of
           Right t          -> Right t
           Left  (idx, rule)
             -- A-label fallback: every label rendered as its wire
             -- bytes (xn-- labels stay literal, ASCII labels stay
             -- ASCII).  Output is therefore pure ASCII and
             -- contains no codepoint of class R/AL/AN, so it is
             -- unambiguous in any-direction text.
             | ASCIIFALLBACK `meetsIdnaFlags` opts -> Right (asciiFromWire sbs)
             | otherwise                           ->
                 Left (ErrCrossLabelBidi idx rule)

-- | Optimistic combined-buffer renderer with streaming cross-label
-- Bidi check.  Builds the same buffer that 'renderUnicodeText'
-- would, and after each label is written scans its just-emitted
-- bytes (UTF-8-decoded out of the partly-built buffer) to update
-- two pieces of cross-label state:
--
--   * @triggered@ -- have we seen a label with R\/AL\/AN content?
--   * @firstViolation@ -- the first label, if any, whose
--     'BidiSummary' fails RFC 5893 Rules 1-6 under forced check.
--
-- After the last label, the decision is trivial: if and only if
-- both pieces are set, the cross-label check fails and we return
-- 'Left'.  Otherwise the rendered 'Text' is returned directly.
-- The buffer is never frozen-then-rebuilt; in the no-trigger
-- common case (hostname-style names) the only extra work over a
-- plain render is the per-label byte scan.
--
-- Violations are recorded but not acted upon until we know
-- whether the trigger fires.  A label like @\"_tcp\"@ on its own
-- would fail Rule 1 under forced check, but if no other label is
-- Bidi-flavoured the rules don't apply and the violation is
-- moot.  This matches the semantics of the previous two-pass
-- design.
renderUnicodeTextChecked
    :: (forall s. ByteArray -> Int -> ST s (Maybe (PrimArray Int)))
    -> Int                          -- ^ initial capacity estimate
    -> [ShortByteString]            -- ^ labels (may be empty)
    -> Either (Int, BidiRuleViolation) Text
renderUnicodeTextChecked _      !_       []     = Right (T.singleton '.')
renderUnicodeTextChecked decode !sizeEst labels = runST do
    let !cap0 = max sizeEst 16
    mba0 <- newByteArray cap0
    (mba, !finalLen, !triggered, !mViol) <-
        drive mba0 cap0 0 True 0 False Nothing labels
    case mViol of
      Just v | triggered -> pure (Left v)
      _ -> do
          shrinkMutableByteArray mba finalLen
          ByteArray b <- unsafeFreezeByteArray mba
          pure (Right (Text (TA.ByteArray b) 0 finalLen))
  where
    drive
        :: MutableByteArray s
        -> Int                              -- cap
        -> Int                              -- cur
        -> Bool                             -- first?
        -> Int                              -- label index
        -> Bool                             -- triggered so far?
        -> Maybe (Int, BidiRuleViolation)   -- first violation seen
        -> [ShortByteString]
        -> ST s ( MutableByteArray s, Int
                , Bool, Maybe (Int, BidiRuleViolation) )
    drive !mba !_   !cur _     _    !trig !mViol [] =
        pure (mba, cur, trig, mViol)
    drive !mba !cap !cur first !idx !trig !mViol (lbl:rest) = do
        (mba1, cap1, cur1) <-
            if first then pure (mba, cap, cur)
                     else writeRawByte mba cap cur 0x2E
        (mba2, cap2, cur2) <-
            writeRenderedUnicodeLabel decode mba1 cap1 cur1 lbl
        -- Scan the bytes we just wrote for this label, in place
        -- in the partly-built buffer.
        !sm <- bidiSummaryFromMutableUtf8Range mba2 cur1 cur2
        let !trig'  = addBidiTrigger trig sm
            !mViol' = case mViol of
              Just _  -> mViol            -- keep the first
              Nothing -> case checkBidiSummary True sm of
                Just rule -> Just (idx, rule)
                Nothing   -> Nothing
        drive mba2 cap2 cur2 False (idx + 1) trig' mViol' rest

-- | Build a 'BidiSummary' by walking a UTF-8-encoded byte range
-- within a 'MutableByteArray'.  Used by 'renderUnicodeTextChecked'
-- to score each label's Bidi contribution by scanning the bytes
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

-- | The single-label counterpart to 'domainToUnicode': an
-- A-label is replaced by the corresponding U-label; any other
-- label is rendered byte-by-byte under the same escaping
-- policy as 'domainToAscii'.
labelToUnicode :: ShortByteString -> Text
labelToUnicode = labelToUnicodeWith mempty

-- | The single-label counterpart to 'domainToAscii'.  Output
-- is always pure ASCII; see 'domainToAscii' for the escape
-- rules.
labelToAscii :: ShortByteString -> Text
labelToAscii sbs = renderAsciiText (SBS.length sbs) [sbs]

-- | Strict counterpart to 'labelToUnicode' that lets the caller
-- choose the 'IdnaFlags' that the underlying round-trip check
-- honours.  Used by 'domainToUnicodeOpt' so render-time
-- options like @EMOJIOK@ have the expected effect: a label
-- whose Punycode body decodes to an emoji codepoint round-trips
-- successfully under @EMOJIOK@ and renders as its decoded form,
-- while still requiring full Punycode round-trip equality and
-- the (mostly off) IDN validation gates.  Off by default ---
-- 'labelToUnicode' uses 'mempty' --- so the strict-but-permissive
-- pre-existing semantics are preserved.
labelToUnicodeWith :: IdnaFlags -> ShortByteString -> Text
labelToUnicodeWith opts = aceLabelToUnicode (strictDecode opts)

-- | Strict A-label decoder: Punycode-decode the body and require
-- it to re-encode to the same bytes under the given 'IdnaFlags'.
-- Returns the decoded codepoints on success; 'Nothing' on any
-- failure (Punycode decode error, U-label validation failure, or
-- round-trip mismatch).  Written with the @forall s.@ at the
-- second argument so the partial application
-- @'strictDecode' opts@ is polymorphic in the 'ST' state thread,
-- which is what 'aceLabelToUnicode' and 'renderUnicodeText' both
-- want.
strictDecode :: IdnaFlags
             -> (forall s. ByteArray -> Int -> ST s (Maybe (PrimArray Int)))
strictDecode !opts !body !bodyLen = do
    rt <- alabelRoundTrip body 0 bodyLen opts
    case rt of
      Right cps -> pure (Just cps)
      Left  _   -> pure Nothing

-- | Lax counterpart to 'labelToUnicode': Punycode-decode the body
-- of an A-label and render the resulting codepoints, /without/
-- U-label validation or re-encode-and-compare.  Intended for
-- diagnostic display only: lets a caller see what an ill-formed
-- A-label decodes to (e.g. emoji codepoints, contextually-
-- misplaced punctuation, C1 control bytes) even when the strict
-- round-trip would reject the result and fall back to the ACE
-- form.
--
-- A /fake/ A-label whose Punycode body fails to decode falls back
-- remains in ASCII form.  No 'IdnaFlags' parameter control flags
-- are available because no validation is performed.
--
-- /Warning/: this is a debugging tool, not a presentation function.
-- The output is not guaranteed to round-trip through 'parseDomain',
-- and may not parse at all.  Unless debugging, use 'labelToUnicode'
-- or the strict 'domainToUnicode'.
labelToUnicodeLax :: ShortByteString -> Text
labelToUnicodeLax = aceLabelToUnicode laxDecode

-- | Lax A-label decoder: Punycode-decode the body and surface
-- the resulting codepoints unconditionally, /without/ U-label
-- validation or re-encode-and-compare.  Returns 'Nothing' only
-- when the Punycode body itself fails to decode.
laxDecode :: forall s. ByteArray -> Int -> ST s (Maybe (PrimArray Int))
laxDecode !body !bodyLen = do
    cpBuf <- newPrimArray maxCpsPerLabel
    rDec  <- punycodeDecode body 0 bodyLen cpBuf 0 maxCpsPerLabel
    case rDec of
      Left  _       -> pure Nothing
      Right cpCount -> Just <$> freezeCps cpBuf cpCount

-- | Shared implementation of 'labelToUnicode' / 'labelToUnicodeLax'.
-- Thin wrapper around 'writeRenderedUnicodeLabel': allocate a
-- per-label buffer, drive the in-buffer per-label writer (which
-- handles the @\"xn--\"@ prefix check, Punycode decode, codepoint
-- emission, and the non-ACE / decode-failure fallbacks), then
-- freeze and wrap as 'Text'.  All escape policy lives in the
-- in-buffer helpers ('writeRenderedUnicodeLabel',
-- 'writeCodepointsUtf8', 'writeCpUtf8', 'writeEscapedLabel',
-- 'writeEscapedByte'); this function adds nothing beyond the
-- buffer envelope.
aceLabelToUnicode
    :: (forall s. ByteArray -> Int -> ST s (Maybe (PrimArray Int)))
    -> ShortByteString
    -> Text
aceLabelToUnicode decode sbs = runST do
    -- Same sizing heuristic as 'renderUnicodeText': exact for the
    -- no-escape ASCII case; doubling growth absorbs any expansion
    -- (\\C, \\DDD, or A-label decode-and-expand).
    let !cap0 = max 16 (SBS.length sbs)
    mba0 <- newByteArray cap0
    (mba, _cap, !finalLen) <-
        writeRenderedUnicodeLabel decode mba0 cap0 0 sbs
    shrinkMutableByteArray mba finalLen
    ByteArray b <- unsafeFreezeByteArray mba
    pure (Text (TA.ByteArray b) 0 finalLen)

-- | Lax counterpart to 'domainToUnicode': renders each label via
-- 'labelToUnicodeLax'.  Use for diagnostic display only.  See the
-- warnings on 'labelToUnicodeLax': the output is not guaranteed to
-- round-trip through 'parseDomain', is not safe to feed back into
-- a zone file or DNS resolver, and may surface ugly content (C1
-- controls, escaped zone-file specials) from malformed input.
-- Use 'domainToUnicode' for anything callers downstream might
-- trust.
domainToUnicodeLax :: Domain -> Text
domainToUnicodeLax (Domain_ sbs) = unicodeLaxFromWire sbs

-- | Wire-bytes variant of 'domainToUnicodeLax'.  Same trust
-- contract as 'unicodeFromWire'.
unicodeLaxFromWire :: ShortByteString -> Text
unicodeLaxFromWire sbs = case toLabelsFromWire sbs of
    [] -> T.singleton '.'
    ls -> renderUnicodeText laxDecode (SBS.length sbs - 2) ls

-- | Test whether the first four bytes of a label are
-- @\"xn--\"@ (case-insensitive on the @\"xn\"@ part).
hasAcePrefix :: ByteArray -> Int -> Bool
hasAcePrefix arr n
    | n < 4 = False
    | otherwise =
        let !b0 = indexByteArray arr 0 :: Word8
            !b1 = indexByteArray arr 1 :: Word8
            !b2 = indexByteArray arr 2 :: Word8
            !b3 = indexByteArray arr 3 :: Word8
        in (b0 == 0x78 || b0 == 0x58)
        && (b1 == 0x6E || b1 == 0x4E)
        && b2 == 0x2D
        && b3 == 0x2D

----------------------------------------------------------------------
-- Domain helpers (Maybe-returning)
----------------------------------------------------------------------

-- | Drop the produced 'LabelInfo' (and any error detail) from a
-- parser's result, leaving just 'Maybe' 'Domain'.
toMaybeDom :: Either e (Domain, b) -> Maybe Domain
toMaybeDom = either (const Nothing) (Just . fst)
{-# INLINE toMaybeDom #-}

-- | Convenient default: parse a 'Text' as a domain name and return
-- 'Nothing' on any error.  Uses 'hostnameLabelForms' as the permitted set
-- of label forms (LDH | RLDH | FAKEA | ALABEL | ULABEL).
mkDomain :: Text -> Maybe Domain
mkDomain = toMaybeDom . parseDomain hostnameLabelForms

-- | 'mkDomain' for 'String' input.  Equivalent to @'mkDomain' . 'T.pack'@.
mkDomainStr :: String -> Maybe Domain
mkDomainStr = mkDomain . T.pack

-- | 'mkDomain' for a UTF-8 'ByteString'.
mkDomainUtf8 :: ByteString -> Maybe Domain
mkDomainUtf8 = toMaybeDom . parseDomainUtf8 hostnameLabelForms

-- | 'mkDomain' for a UTF-8 'ShortByteString'.
mkDomainShort :: ShortByteString -> Maybe Domain
mkDomainShort = toMaybeDom . parseDomainShort hostnameLabelForms

----------------------------------------------------------------------
-- Domain literals (TH splices)
----------------------------------------------------------------------

-- | Template-Haskell typed splice for an IDNA-aware 'Domain' literal.
-- The presentation-form 'String' is parsed at compile time using
-- 'hostnameLabelForms', and the resulting wire form (with U-labels encoded
-- to A-labels) is embedded into the program as a constant.  Example:
--
-- > domain :: Domain
-- > domain = $$(dnLit "muenchen.example.org")
--
-- An invalid literal becomes a compile-time error.  Callers needing
-- a different form set (e.g. one that admits attrleafs) or different
-- 'IdnaFlags' should use 'dnLitAs'.
--
-- The literal is checked against 'defaultIdnaFlags', which includes
-- 'BIDICHECK'.  In addition to the per-label half of RFC 5893
-- enforced at parse time, the cross-label half is verified at
-- compile time by running 'domainToUnicodeOpt': any literal whose
-- labels collectively violate Rules 1-6 fails the splice.  Since
-- the check runs at build time, the resulting binary pays no
-- runtime cost.
dnLit :: forall m. (MonadFail m, TH.Quote m) => String -> TH.Code m Domain
dnLit = dnLitAs hostnameLabelForms defaultIdnaFlags

-- | Like 'dnLit' but with an explicit allowed-form set and explicit
-- 'IdnaFlags'.  Useful for embedding literals that 'dnLit' would
-- reject -- e.g. an 'ATTRLEAF' label, or a name that the
-- cross-label Bidi check rejects.  Callers wanting a literal that
-- bypasses the cross-label check pass an 'IdnaFlags' value with
-- 'BIDICHECK' cleared.
dnLitAs :: forall m. (MonadFail m, TH.Quote m)
        => LabelFormSet
        -> IdnaFlags
        -> String
        -> TH.Code m Domain
dnLitAs allowed opts0 s = TH.liftCode $ fmap TH.TExp $
    let !opts = effectiveIdnaFlags opts0
    in case parseDomainOpts allowed opts (T.pack s) of
      Left e ->
          fail $ "Invalid domain-name literal " ++ show s
              ++ ": " ++ show e
      Right (dn, _) -> case checkLitBidi opts dn of
        Just e ->
            fail $ "Domain-name literal " ++ show s
                ++ " violates RFC 5893: " ++ show e
        Nothing ->
            TH.appE (TH.conE 'Domain)
                    (TH.appE (TH.varE 'SBS.toShort)
                             (TH.lift (wireBytes dn)))

-- | Compile-time helper for 'dnLitAs': when the caller's
-- 'IdnaFlags' enables 'BIDICHECK', run the cross-label
-- 'domainToUnicodeOpt' check and return the offending
-- 'IdnaError' (or 'Nothing' if the domain is clean
-- or the check is disabled).  When 'ASCIIFALLBACK' is also set the
-- caller has explicitly opted into the lenient render policy, so
-- 'domainToUnicodeOpt' returns 'Right' even on a Bidi failure
-- and this helper reports 'Nothing' too.
checkLitBidi :: IdnaFlags -> Domain -> Maybe IdnaError
checkLitBidi !opts !dn
    | not (BIDICHECK `meetsIdnaFlags` opts) = Nothing
    | otherwise = case domainToUnicodeOpt opts dn of
        Right _ -> Nothing
        Left  e -> Just e


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

-- | Apply Unicode toLower to every non-ASCII codepoint in
-- @cpBuf[0..cnt)@ in place, using the host GHC's 'Char.toLower'
-- table.  ASCII codepoints are skipped: the pre-classification
-- 'asciiDownCaseBuf' pass already handled them, and
-- 'Char.toLower' on already-lowercase ASCII is identity.
unicodeDownCaseBuf :: forall s. MutablePrimArray s Int -> Int -> ST s ()
unicodeDownCaseBuf !buf !cnt = go 0
  where
    go :: Int -> ST s ()
    go !i
      | i >= cnt = pure ()
      | otherwise = do
          cp <- readPrimArray buf i
          when (cp >= 0x80) $ do
              let !cp' = ord (Char.toLower (chr cp))
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
