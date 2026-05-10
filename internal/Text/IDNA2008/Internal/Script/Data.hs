-- |
-- Module      : Text.IDNA2008.Internal.Script.Data
-- Description : Compact codepoint range tables for the Unicode
--               Scripts consulted by the CONTEXTO contextual rules.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- AUTOMATICALLY GENERATED.  Do not edit by hand.  Regenerate with:
--
--   python3 internal/tools/genIdnaScript.py <unicode-version> \\
--           <Scripts.txt>
--
-- Source: https://www.unicode.org/Public/17.0.0/ucd/Scripts.txt
-- Aligned with: Unicode 17.0.0
-- Greek ranges: 36
-- Hebrew ranges: 9
-- HKH (Hiragana | Katakana | Han) ranges: 39
--
-- Only the four scripts whose membership is tested by RFC 5892
-- Appendix A.4-A.7 (Greek, Hebrew, Hiragana \/ Katakana \/ Han) are
-- represented.  Each script's @sc@ (Script) ranges are stored as a
-- sorted sequence of @(start, end)@ pairs (inclusive on both ends),
-- packed into a single 'ByteArray' as alternating 'Word32's:
--
-- > [start0, end0, start1, end1, ...]
--
-- Lookup is binary search for the largest @start_i \<= cp@ and a
-- bounds check @cp \<= end_i@; see "Text.IDNA2008.Internal.Script".
--
-- The Hiragana, Katakana, and Han ranges are merged into a single
-- @hkhRanges@ table because RFC 5892 A.7 only ever asks for
-- \"any of these three\", and the merged table costs no extra
-- comparisons at lookup time.  Greek and Hebrew remain separate
-- because A.4 only asks for Greek and A.5\/A.6 only for Hebrew.
module Text.IDNA2008.Internal.Script.Data
    ( greekRangeCount
    , greekRanges
    , hebrewRangeCount
    , hebrewRanges
    , hkhRangeCount
    , hkhRanges
    ) where

import Data.Array.Byte (ByteArray)
import Data.Word (Word32)
import qualified Data.Primitive.ByteArray as PBA


----------------------------------------------------------------------
-- Greek (Script: Grek)
----------------------------------------------------------------------

-- | Number of (start, end) range pairs in 'greekRanges'.
greekRangeCount :: Int
greekRangeCount = 36

-- | Greek script codepoint ranges, alternating start\/end.
greekRanges :: ByteArray
greekRanges = PBA.byteArrayFromList
    [ (0x000370 :: Word32), 0x000373
    , 0x000375, 0x000377
    , 0x00037A, 0x00037D
    , 0x00037F, 0x00037F
    , 0x000384, 0x000384
    , 0x000386, 0x000386
    , 0x000388, 0x00038A
    , 0x00038C, 0x00038C
    , 0x00038E, 0x0003A1
    , 0x0003A3, 0x0003E1
    , 0x0003F0, 0x0003FF
    , 0x001D26, 0x001D2A
    , 0x001D5D, 0x001D61
    , 0x001D66, 0x001D6A
    , 0x001DBF, 0x001DBF
    , 0x001F00, 0x001F15
    , 0x001F18, 0x001F1D
    , 0x001F20, 0x001F45
    , 0x001F48, 0x001F4D
    , 0x001F50, 0x001F57
    , 0x001F59, 0x001F59
    , 0x001F5B, 0x001F5B
    , 0x001F5D, 0x001F5D
    , 0x001F5F, 0x001F7D
    , 0x001F80, 0x001FB4
    , 0x001FB6, 0x001FC4
    , 0x001FC6, 0x001FD3
    , 0x001FD6, 0x001FDB
    , 0x001FDD, 0x001FEF
    , 0x001FF2, 0x001FF4
    , 0x001FF6, 0x001FFE
    , 0x002126, 0x002126
    , 0x00AB65, 0x00AB65
    , 0x010140, 0x01018E
    , 0x0101A0, 0x0101A0
    , 0x01D200, 0x01D245
    ]

----------------------------------------------------------------------
-- Hebrew (Script: Hebr)
----------------------------------------------------------------------

-- | Number of (start, end) range pairs in 'hebrewRanges'.
hebrewRangeCount :: Int
hebrewRangeCount = 9

-- | Hebrew script codepoint ranges, alternating start\/end.
hebrewRanges :: ByteArray
hebrewRanges = PBA.byteArrayFromList
    [ (0x000591 :: Word32), 0x0005C7
    , 0x0005D0, 0x0005EA
    , 0x0005EF, 0x0005F4
    , 0x00FB1D, 0x00FB36
    , 0x00FB38, 0x00FB3C
    , 0x00FB3E, 0x00FB3E
    , 0x00FB40, 0x00FB41
    , 0x00FB43, 0x00FB44
    , 0x00FB46, 0x00FB4F
    ]

----------------------------------------------------------------------
-- Hiragana | Katakana | Han (merged for RFC 5892 A.7)
----------------------------------------------------------------------

-- | Number of (start, end) range pairs in 'hkhRanges'.
hkhRangeCount :: Int
hkhRangeCount = 39

-- | Codepoints whose @sc@ (Script) is Hiragana, Katakana,
-- or Han, merged into one sorted range list.
hkhRanges :: ByteArray
hkhRanges = PBA.byteArrayFromList
    [ (0x002E80 :: Word32), 0x002E99
    , 0x002E9B, 0x002EF3
    , 0x002F00, 0x002FD5
    , 0x003005, 0x003005
    , 0x003007, 0x003007
    , 0x003021, 0x003029
    , 0x003038, 0x00303B
    , 0x003041, 0x003096
    , 0x00309D, 0x00309F
    , 0x0030A1, 0x0030FA
    , 0x0030FD, 0x0030FF
    , 0x0031F0, 0x0031FF
    , 0x0032D0, 0x0032FE
    , 0x003300, 0x003357
    , 0x003400, 0x004DBF
    , 0x004E00, 0x009FFF
    , 0x00F900, 0x00FA6D
    , 0x00FA70, 0x00FAD9
    , 0x00FF66, 0x00FF6F
    , 0x00FF71, 0x00FF9D
    , 0x016FE2, 0x016FE3
    , 0x016FF0, 0x016FF6
    , 0x01AFF0, 0x01AFF3
    , 0x01AFF5, 0x01AFFB
    , 0x01AFFD, 0x01AFFE
    , 0x01B000, 0x01B122
    , 0x01B132, 0x01B132
    , 0x01B150, 0x01B152
    , 0x01B155, 0x01B155
    , 0x01B164, 0x01B167
    , 0x01F200, 0x01F200
    , 0x020000, 0x02A6DF
    , 0x02A700, 0x02B81D
    , 0x02B820, 0x02CEAD
    , 0x02CEB0, 0x02EBE0
    , 0x02EBF0, 0x02EE5D
    , 0x02F800, 0x02FA1D
    , 0x030000, 0x03134A
    , 0x031350, 0x033479
    ]
