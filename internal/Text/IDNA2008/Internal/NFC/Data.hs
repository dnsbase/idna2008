-- |
-- Module      : Text.IDNA2008.Internal.NFC.Data
-- Description : Codepoint range tables for the Unicode
--               @NFC_Quick_Check@ property values @No@ and
--               @Maybe@.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- AUTOMATICALLY GENERATED.  Do not edit by hand.  Regenerate with:
--
--   python3 internal/tools/genIdnaNFC.py <unicode-version> \\
--           <DerivedNormalizationProps.txt>
--
-- Source: https://www.unicode.org/Public/17.0.0/ucd/DerivedNormalizationProps.txt
-- Aligned with: Unicode 17.0.0
-- NFC_QC=No  ranges: 73
-- NFC_QC=Maybe ranges: 49
--
-- Codepoints absent from both tables are implicitly @Yes@
-- (already in NFC).
module Text.IDNA2008.Internal.NFC.Data
    ( nfcNoRangeCount
    , nfcNoRanges
    , nfcMaybeRangeCount
    , nfcMaybeRanges
    ) where

import Data.Array.Byte (ByteArray)
import Data.Word (Word32)
import qualified Data.Primitive.ByteArray as PBA

----------------------------------------------------------------------
-- NFC_Quick_Check = No
----------------------------------------------------------------------

-- | Number of (start, end) range pairs in 'nfcNoRanges'.
nfcNoRangeCount :: Int
nfcNoRangeCount = 73

-- | Codepoint ranges with @NFC_Quick_Check = No@,
-- alternating start\/end @Word32@ pairs.
nfcNoRanges :: ByteArray
nfcNoRanges = PBA.byteArrayFromList
    [ (0x000340 :: Word32)
    , 0x000341
    , 0x000343
    , 0x000344
    , 0x000374
    , 0x000374
    , 0x00037E
    , 0x00037E
    , 0x000387
    , 0x000387
    , 0x000958
    , 0x00095F
    , 0x0009DC
    , 0x0009DD
    , 0x0009DF
    , 0x0009DF
    , 0x000A33
    , 0x000A33
    , 0x000A36
    , 0x000A36
    , 0x000A59
    , 0x000A5B
    , 0x000A5E
    , 0x000A5E
    , 0x000B5C
    , 0x000B5D
    , 0x000F43
    , 0x000F43
    , 0x000F4D
    , 0x000F4D
    , 0x000F52
    , 0x000F52
    , 0x000F57
    , 0x000F57
    , 0x000F5C
    , 0x000F5C
    , 0x000F69
    , 0x000F69
    , 0x000F73
    , 0x000F73
    , 0x000F75
    , 0x000F76
    , 0x000F78
    , 0x000F78
    , 0x000F81
    , 0x000F81
    , 0x000F93
    , 0x000F93
    , 0x000F9D
    , 0x000F9D
    , 0x000FA2
    , 0x000FA2
    , 0x000FA7
    , 0x000FA7
    , 0x000FAC
    , 0x000FAC
    , 0x000FB9
    , 0x000FB9
    , 0x001F71
    , 0x001F71
    , 0x001F73
    , 0x001F73
    , 0x001F75
    , 0x001F75
    , 0x001F77
    , 0x001F77
    , 0x001F79
    , 0x001F79
    , 0x001F7B
    , 0x001F7B
    , 0x001F7D
    , 0x001F7D
    , 0x001FBB
    , 0x001FBB
    , 0x001FBE
    , 0x001FBE
    , 0x001FC9
    , 0x001FC9
    , 0x001FCB
    , 0x001FCB
    , 0x001FD3
    , 0x001FD3
    , 0x001FDB
    , 0x001FDB
    , 0x001FE3
    , 0x001FE3
    , 0x001FEB
    , 0x001FEB
    , 0x001FEE
    , 0x001FEF
    , 0x001FF9
    , 0x001FF9
    , 0x001FFB
    , 0x001FFB
    , 0x001FFD
    , 0x001FFD
    , 0x002000
    , 0x002001
    , 0x002126
    , 0x002126
    , 0x00212A
    , 0x00212B
    , 0x002329
    , 0x00232A
    , 0x002ADC
    , 0x002ADC
    , 0x00F900
    , 0x00FA0D
    , 0x00FA10
    , 0x00FA10
    , 0x00FA12
    , 0x00FA12
    , 0x00FA15
    , 0x00FA1E
    , 0x00FA20
    , 0x00FA20
    , 0x00FA22
    , 0x00FA22
    , 0x00FA25
    , 0x00FA26
    , 0x00FA2A
    , 0x00FA6D
    , 0x00FA70
    , 0x00FAD9
    , 0x00FB1D
    , 0x00FB1D
    , 0x00FB1F
    , 0x00FB1F
    , 0x00FB2A
    , 0x00FB36
    , 0x00FB38
    , 0x00FB3C
    , 0x00FB3E
    , 0x00FB3E
    , 0x00FB40
    , 0x00FB41
    , 0x00FB43
    , 0x00FB44
    , 0x00FB46
    , 0x00FB4E
    , 0x01D15E
    , 0x01D164
    , 0x01D1BB
    , 0x01D1C0
    , 0x02F800
    , 0x02FA1D
    ]


----------------------------------------------------------------------
-- NFC_Quick_Check = Maybe
----------------------------------------------------------------------

-- | Number of (start, end) range pairs in 'nfcMaybeRanges'.
nfcMaybeRangeCount :: Int
nfcMaybeRangeCount = 49

-- | Codepoint ranges with @NFC_Quick_Check = Maybe@,
-- alternating start\/end @Word32@ pairs.
nfcMaybeRanges :: ByteArray
nfcMaybeRanges = PBA.byteArrayFromList
    [ (0x000300 :: Word32)
    , 0x000304
    , 0x000306
    , 0x00030C
    , 0x00030F
    , 0x00030F
    , 0x000311
    , 0x000311
    , 0x000313
    , 0x000314
    , 0x00031B
    , 0x00031B
    , 0x000323
    , 0x000328
    , 0x00032D
    , 0x00032E
    , 0x000330
    , 0x000331
    , 0x000338
    , 0x000338
    , 0x000342
    , 0x000342
    , 0x000345
    , 0x000345
    , 0x000653
    , 0x000655
    , 0x00093C
    , 0x00093C
    , 0x0009BE
    , 0x0009BE
    , 0x0009D7
    , 0x0009D7
    , 0x000B3E
    , 0x000B3E
    , 0x000B56
    , 0x000B57
    , 0x000BBE
    , 0x000BBE
    , 0x000BD7
    , 0x000BD7
    , 0x000C56
    , 0x000C56
    , 0x000CC2
    , 0x000CC2
    , 0x000CD5
    , 0x000CD6
    , 0x000D3E
    , 0x000D3E
    , 0x000D57
    , 0x000D57
    , 0x000DCA
    , 0x000DCA
    , 0x000DCF
    , 0x000DCF
    , 0x000DDF
    , 0x000DDF
    , 0x00102E
    , 0x00102E
    , 0x001161
    , 0x001175
    , 0x0011A8
    , 0x0011C2
    , 0x001B35
    , 0x001B35
    , 0x003099
    , 0x00309A
    , 0x0110BA
    , 0x0110BA
    , 0x011127
    , 0x011127
    , 0x01133E
    , 0x01133E
    , 0x011357
    , 0x011357
    , 0x0113B8
    , 0x0113B8
    , 0x0113BB
    , 0x0113BB
    , 0x0113C2
    , 0x0113C2
    , 0x0113C5
    , 0x0113C5
    , 0x0113C7
    , 0x0113C9
    , 0x0114B0
    , 0x0114B0
    , 0x0114BA
    , 0x0114BA
    , 0x0114BD
    , 0x0114BD
    , 0x0115AF
    , 0x0115AF
    , 0x011930
    , 0x011930
    , 0x01611E
    , 0x016129
    , 0x016D67
    , 0x016D68
    ]
