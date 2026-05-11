-- |
-- Module      : Text.IDNA2008.Internal.LabelFormSet
-- Description : Sets of label classifications.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- 'LabelFormSet' is a /set/ of 'LabelForm' values, stored as a
-- 'Word8' bitmask over the eight real-label classifications.
-- It is the type passed to the parser as the @allowed@
-- argument and the type produced by the command-line vocabulary
-- parser.
--
-- The singleton 'NoLabel' is /not/ representable as a
-- 'LabelFormSet' member: 'singletonLabelFormSet' maps it to
-- 'mempty' (via Word8 overflow on the bit shift), and
-- 'memberLabelFormSet' returns 'False' for it against any set.
-- This guarantees that the sentinel cannot leak into a
-- permitted-set or a parser-output set.
--
-- Composition: the canonical way to build a set is from a
-- pre-built named set ('allLabelForms', 'idnLabelForms',
-- 'hostnameLabelForms') or from 'mempty', combined with
-- individual 'LabelForm' values via '<+>' and '<->':
--
-- > mempty <+> LDH <+> ULABEL <+> ALABEL
-- > hostnameLabelForms <-> FAKEA
-- > idnLabelForms <+> WILDLABEL
--
-- Set-to-set composition uses '<>' (Monoid union) and
-- 'withoutLabelFormSet' (set difference).
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.IDNA2008.Internal.LabelFormSet
    ( -- * Set type
      LabelFormSet
    , unLabelFormSet

      -- * Set construction from singletons
    , (<+>)
    , (<->)
    , singletonLabelFormSet
    , labelFormSetFromList

      -- * Set-level operations
    , meetsLabelFormSet
    , memberLabelFormSet
    , withoutLabelFormSet

      -- * Pre-built sets
    , allLabelForms
    , idnLabelForms
    , hostnameLabelForms

      -- * CLI vocabulary
    , labelFormSetTokens
    , labelFormSetPresets
    , parseLabelFormSet
    , parseLabelFormSetStr
    ) where

import Control.Monad ((>=>))
import Data.Bits ((.|.), (.&.), complement, shiftL, testBit, unsafeShiftR)
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Word (Word8)

import Text.IDNA2008.Internal.LabelForm
    ( LabelForm
    , pattern LDH
    , pattern RLDH
    , pattern FAKEA
    , pattern ALABEL
    , pattern ULABEL
    , pattern ATTRLEAF
    , pattern OCTET
    , pattern WILDLABEL
    , unLabelForm
    )
import Text.IDNA2008.Internal.Tokens
    ( TokenEntry(..), Preset, asciiByteString, parseTokens )

infixl 6 <+>, <->

-- | A set of 'LabelForm' values, stored as a 'Word8' bitmask
-- over the eight real-label classifications.  The data
-- constructor is not exported; sets are built via 'mempty',
-- '<>', '<+>', '<->', 'singletonLabelFormSet',
-- 'labelFormSetFromList', or the pre-built named values
-- 'allLabelForms', 'idnLabelForms', 'hostnameLabelForms'.
newtype LabelFormSet = LabelFormSet Word8
    deriving (Eq, Ord)

-- | Extract the underlying bitmask.  Internal use only.
unLabelFormSet :: LabelFormSet -> Word8
unLabelFormSet (LabelFormSet w) = w
{-# INLINE unLabelFormSet #-}

instance Semigroup LabelFormSet where
    LabelFormSet a <> LabelFormSet b = LabelFormSet (a .|. b)
    {-# INLINE (<>) #-}

instance Monoid LabelFormSet where
    mempty = LabelFormSet 0
    {-# INLINE mempty #-}

instance Show LabelFormSet where
    show s = case formNames s of
        []  -> "mempty"
        ns  -> "(" ++ intercalate "<>" ns ++ ")"

-- | Decompose a set into its singleton form names, in canonical
-- bit order.  Internal-only; used by 'Show'.
formNames :: LabelFormSet -> [String]
formNames (LabelFormSet w) = go names w
  where
    names = [ "LDH", "RLDH", "FAKEA", "ALABEL"
            , "ULABEL", "ATTRLEAF", "OCTET", "WILDLABEL" ]
    go _      0 = []
    go []     _ = []
    go (s:ss) n
        | odd n     = s : go ss (n `unsafeShiftR` 1)
        | otherwise = go ss (n `unsafeShiftR` 1)

----------------------------------------------------------------------
-- Set construction from singletons
----------------------------------------------------------------------

-- | Add a singleton 'LabelForm' to a set.  Reads as
-- @set \`addForm\` LDH@ but more concisely as @set \<+\> LDH@.
--
-- Note that @set \<+\> 'Text.IDNA2008.Internal.LabelForm.NoLabel'
-- == set@: the sentinel maps to 'mempty', and union with
-- 'mempty' is the identity.
(<+>) :: LabelFormSet -> LabelForm -> LabelFormSet
set <+> form = set <> singletonLabelFormSet form
{-# INLINE (<+>) #-}

-- | Remove a singleton 'LabelForm' from a set.  Reads as
-- @set \`withoutForm\` FAKEA@ but more concisely as
-- @set \<-\> FAKEA@.
--
-- @set \<-\> 'Text.IDNA2008.Internal.LabelForm.NoLabel' == set@.
(<->) :: LabelFormSet -> LabelForm -> LabelFormSet
set <-> form = set `withoutLabelFormSet` singletonLabelFormSet form
{-# INLINE (<->) #-}

-- | Turn a single 'LabelForm' into a one-element 'LabelFormSet'.
-- Maps 'Text.IDNA2008.Internal.LabelForm.NoLabel' to 'mempty'
-- via 'Word8' overflow on the bit shift: @1 \`shiftL\` 8@ is
-- zero in 'Word8', so the sentinel becomes the empty set.
singletonLabelFormSet :: LabelForm -> LabelFormSet
singletonLabelFormSet form =
    LabelFormSet (1 `shiftL` fromIntegral (unLabelForm form))
{-# INLINE singletonLabelFormSet #-}

-- | Build a 'LabelFormSet' from a list of 'LabelForm' values.
-- Duplicates and 'Text.IDNA2008.Internal.LabelForm.NoLabel'
-- entries are silently absorbed.
labelFormSetFromList :: [LabelForm] -> LabelFormSet
labelFormSetFromList = foldr (\f s -> singletonLabelFormSet f <> s) mempty

----------------------------------------------------------------------
-- Set-level operations
----------------------------------------------------------------------

-- | Subset test: @a \`meetsLabelFormSet\` b@ is 'True' iff
-- every form in @a@ is also in @b@.  Reads as @a@ satisfies
-- the requirements expressed by @b@.
--
-- Note that @'mempty' \`meetsLabelFormSet\` b@ is 'True' for
-- any @b@: the empty set is vacuously a subset of every set.
meetsLabelFormSet :: LabelFormSet -> LabelFormSet -> Bool
LabelFormSet a `meetsLabelFormSet` LabelFormSet b = a == a .&. b
{-# INLINE meetsLabelFormSet #-}

-- | Membership test: does the given 'LabelForm' belong to the
-- set?  Returns 'False' for
-- 'Text.IDNA2008.Internal.LabelForm.NoLabel' against any set.
memberLabelFormSet :: LabelForm -> LabelFormSet -> Bool
memberLabelFormSet form (LabelFormSet b)
    | i < 8     = testBit b (fromIntegral i)
    | otherwise = False
  where
    i = unLabelForm form
{-# INLINE memberLabelFormSet #-}

-- | Set difference: @a \`withoutLabelFormSet\` b@ removes every
-- bit of @b@ from @a@.  Companion to '<>' (union).
withoutLabelFormSet :: LabelFormSet -> LabelFormSet -> LabelFormSet
LabelFormSet a `withoutLabelFormSet` LabelFormSet b =
    LabelFormSet (a .&. complement b)
{-# INLINE withoutLabelFormSet #-}

----------------------------------------------------------------------
-- Pre-built sets
----------------------------------------------------------------------

-- | Every real-label classification.  Use as the @allowed@
-- argument to admit any classification.
allLabelForms :: LabelFormSet
allLabelForms = LabelFormSet 0xFF

-- | The IDN-flavoured subset: ordinary hostname-style labels
-- ('LDH'), valid A-labels, and Unicode labels.  Excludes
-- 'RLDH' and 'FAKEA' so unusual @\"xn--\"@ shapes are
-- rejected at parse time.
idnLabelForms :: LabelFormSet
idnLabelForms = mempty <+> LDH <+> ALABEL <+> ULABEL

-- | Hostname-shaped labels: 'idnLabelForms' plus 'RLDH' and
-- 'FAKEA'.  Useful when consuming names from the wild (zone
-- files, public-suffix child registrations) where unusual but
-- syntactically valid LDH labels do appear.
hostnameLabelForms :: LabelFormSet
hostnameLabelForms = idnLabelForms <+> RLDH <+> FAKEA

----------------------------------------------------------------------
-- CLI vocabulary
----------------------------------------------------------------------

-- | Single-bit token table for command-line vocabulary.
labelFormSetTokens :: [TokenEntry LabelFormSet]
labelFormSetTokens =
    [ TokenEntry (singletonLabelFormSet LDH)       "ldh"        []
    , TokenEntry (singletonLabelFormSet RLDH)      "rldh"       []
    , TokenEntry (singletonLabelFormSet FAKEA)     "fakea"      []
    , TokenEntry (singletonLabelFormSet ALABEL)    "alabel"     []
    , TokenEntry (singletonLabelFormSet ULABEL)    "ulabel"     []
    , TokenEntry (singletonLabelFormSet ATTRLEAF)  "attrleaf"   []
    , TokenEntry (singletonLabelFormSet OCTET)     "octet"      []
    , TokenEntry (singletonLabelFormSet WILDLABEL) "wildlabel"  []
    ]

-- | Preset names that expand to multi-bit form sets.
labelFormSetPresets :: [Preset LabelFormSet]
labelFormSetPresets =
    [ ("idn",    idnLabelForms)
    , ("strict", idnLabelForms)             -- alias of @idn@
    , ("host",   hostnameLabelForms)
    , ("all",    allLabelForms)
    ]

-- | Parse a comma-separated CLI value into a 'LabelFormSet'.
parseLabelFormSet :: LabelFormSet -> ByteString -> Either String LabelFormSet
parseLabelFormSet !defS =
    parseTokens labelFormSetTokens
                (("default", defS) : labelFormSetPresets)
                defS
                withoutLabelFormSet

-- | 'String'-flavoured wrapper for 'parseLabelFormSet' suitable
-- for @optparse-applicative@'s 'eitherReader'.
parseLabelFormSetStr :: LabelFormSet -> String -> Either String LabelFormSet
parseLabelFormSetStr !defS = asciiByteString >=> parseLabelFormSet defS
