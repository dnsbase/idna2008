-- |
-- Module      : Text.IDNA2008.Internal.Emoji
-- Description : Typed wrappers over the Unicode @Emoji=Yes@ range
--               table and its UTS #46-mapped subset, consumed by
--               the @EMOJIOK@ option.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Two lookups, 'isEmoji' and 'isMappedEmoji', both implemented as
-- partial applications of "Text.IDNA2008.Internal.Ranges"'s
-- 'inRanges' helper over the coalesced range tables in
-- "Text.IDNA2008.Internal.Emoji.Data".
--
-- 'isEmoji' is the Unicode @Emoji=Yes@ property predicate;
-- 'isMappedEmoji' is the subset whose UTS #46 status is also
-- @mapped@ -- i.e. the ambiguous-across-tools subset (browsers
-- following UTS #46 mapping reach the fold target; admit-as-is
-- tooling would reach the unmapped form).  This library
-- declines to pick either interpretation: codepoints in the
-- mapped-emoji subset remain @DISALLOWED@ even under @EMOJIOK@.
--
-- The IDNA U-label validator combines the two predicates: under
-- @EMOJIOK@ a non-ASCII @DISALLOWED@ codepoint is admitted iff
-- @'isEmoji' cp && not ('isMappedEmoji' cp)@.  Both predicates
-- are dead code when @EMOJIOK@ is not set.

module Text.IDNA2008.Internal.Emoji
    ( isEmoji
    , isMappedEmoji
    ) where

import Text.IDNA2008.Internal.Ranges (inRanges)
import Text.IDNA2008.Internal.Emoji.Data
    ( emojiRangeCount
    , emojiRanges
    , mappedEmojiRangeCount
    , mappedEmojiRanges
    )

-- | Test whether @cp@ has the Unicode @Emoji=Yes@ property.
-- Codepoints absent from the range table are not emoji.
isEmoji :: Int -> Bool
isEmoji = inRanges emojiRanges emojiRangeCount
{-# INLINE isEmoji #-}

-- | Test whether @cp@ is both @Emoji=Yes@ and folded by UTS #46.
-- These codepoints are excluded from the @EMOJIOK@ relaxation
-- because they resolve ambiguously across the ecosystem.  See
-- the module haddock in "Text.IDNA2008.Internal.Emoji.Data" for
-- the empirical rationale.
isMappedEmoji :: Int -> Bool
isMappedEmoji = inRanges mappedEmojiRanges mappedEmojiRangeCount
{-# INLINE isMappedEmoji #-}
