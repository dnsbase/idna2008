-- |
-- Module      : Text.IDNA2008.Internal.Script
-- Description : Predicates for the Unicode Scripts consulted by the
--               CONTEXTO contextual rules (RFC 5892 A.4-A.7).
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Membership predicates over the Unicode @sc@ (Script) property
-- for the four scripts named by RFC 5892 Appendix A.4-A.7:
-- Greek, Hebrew, and the Hiragana \/ Katakana \/ Han trio (the
-- last collapsed into one predicate because A.7 only ever needs
-- the union).  Backed by the compact range tables in
-- "Text.IDNA2008.Internal.Script.Data".

module Text.IDNA2008.Internal.Script
    ( isGreekCp
    , isHebrewCp
    , isHkhCp
    ) where

import Text.IDNA2008.Internal.Ranges (inRanges)
import Text.IDNA2008.Internal.Script.Data
    ( greekRangeCount
    , greekRanges
    , hebrewRangeCount
    , hebrewRanges
    , hkhRangeCount
    , hkhRanges
    )

-- | Is @cp@ in the Greek script?  RFC 5892 A.4 consults this on
-- the codepoint immediately following @U+0375@ (Greek Lower
-- Numeral Sign).
isGreekCp :: Int -> Bool
isGreekCp = inRanges greekRanges greekRangeCount
{-# INLINE isGreekCp #-}

-- | Is @cp@ in the Hebrew script?  RFC 5892 A.5 \/ A.6 consult
-- this on the codepoint immediately preceding @U+05F3@ (Hebrew
-- Punctuation Geresh) or @U+05F4@ (Hebrew Punctuation
-- Gershayim).
isHebrewCp :: Int -> Bool
isHebrewCp = inRanges hebrewRanges hebrewRangeCount
{-# INLINE isHebrewCp #-}

-- | Is @cp@ in Hiragana, Katakana, or Han?  RFC 5892 A.7
-- requires that a label containing @U+30FB@ (Katakana Middle
-- Dot) also contain at least one codepoint in this union.
isHkhCp :: Int -> Bool
isHkhCp = inRanges hkhRanges hkhRangeCount
{-# INLINE isHkhCp #-}

