-- |
-- Module      : Text.IDNA2008.Internal.LabelFormSet
-- Description : Sets of label classifications.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Sets of 'LabelForm' values used as the parser's admission set.
-- See 'LabelFormSet' for construction idioms and operations.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.IDNA2008.Internal.LabelFormSet
    ( -- * Set type
      LabelFormSet
    , unLabelFormSet

      -- * Set construction from singletons
    , (<+>)
    , (<->)
    , labelFormToSet

      -- * Set-level operations
    , isLabelFormSubset
    , memberLabelFormSet
    , withoutLabelFormSet

      -- * Pre-built sets
    , allLabelForms
    , idnLabelForms
    , hostnameLabelForms

      -- * Command-line option syntax
    , labelFormSetTokens
    , labelFormSetPresets
    , parseLabelFormSet
    , parseLabelFormSetStr
    ) where

import Control.Monad ((>=>))
import Data.Bits ((.|.), (.&.), complement, shiftL, testBit, unsafeShiftR)
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Word (Word16)

import Text.IDNA2008.Internal.LabelForm ( LabelForm(..) )
import Text.IDNA2008.Internal.Tokens
    ( TokenEntry(..), Preset, asciiByteString, parseTokens )

infixl 6 <+>, <->

-- | A set of 'LabelForm' values, used as the @allowed@ argument
-- to the parser.  Build sets from 'mempty' or one of the
-- pre-built named values ('allLabelForms', 'idnLabelForms',
-- 'hostnameLabelForms'), then add or remove individual forms
-- with @('<+>')@ and @('<->')@, or combine sets with @('<>')@.
-- 'labelFormToSet' promotes a single 'LabelForm' to a one-element
-- set; 'withoutLabelFormSet' implements set difference.
--
-- Examples:
--
-- > mempty <+> LDH <+> ULABEL <+> ALABEL
-- > foldMap' labelFormToSet [LDH, ULABEL, ALABEL]
-- > hostnameLabelForms <-> FAKEA
-- > idnLabelForms <+> WILDLABEL
newtype LabelFormSet = LabelFormSet Word16
    deriving Eq

-- | Extract the underlying bitmask.  Internal use only.
unLabelFormSet :: LabelFormSet -> Word16
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
        ns  -> intercalate "," ns

-- | Decompose a set into its singleton form names, in canonical
-- bit order.  Internal-only; used by 'Show'.
formNames :: LabelFormSet -> [String]
formNames (LabelFormSet w) = go names w
  where
    names = [ "LDH", "RLDH", "FAKEA", "ALABEL"
            , "ULABEL", "ATTRLEAF", "OCTET", "WILDLABEL", "LAXULABEL" ]
    go _      0 = []
    go []     _ = []
    go (s:ss) n
        | odd n     = s : go ss (n `unsafeShiftR` 1)
        | otherwise = go ss (n `unsafeShiftR` 1)

----------------------------------------------------------------------
-- Set construction from singletons
----------------------------------------------------------------------

-- | Add a 'LabelForm' to a set.
(<+>) :: LabelFormSet -> LabelForm -> LabelFormSet
set <+> form = set <> labelFormToSet form
{-# INLINE (<+>) #-}

-- | Remove a 'LabelForm' from a set.
(<->) :: LabelFormSet -> LabelForm -> LabelFormSet
set <-> form = set `withoutLabelFormSet` labelFormToSet form
{-# INLINE (<->) #-}

-- | One-element set containing the given 'LabelForm'.
-- 'NoLabel' maps to 'mempty'.
labelFormToSet :: LabelForm -> LabelFormSet
labelFormToSet (LabelForm_ form)
    | form < 9  = LabelFormSet (1 `shiftL` fromIntegral form)
    | otherwise = mempty
{-# INLINE labelFormToSet #-}

----------------------------------------------------------------------
-- Set-level operations
----------------------------------------------------------------------

-- | Subset test: @a \`isLabelFormSubset\` b@ is 'True' iff
-- every form in @a@ is also in @b@.  'mempty' is a subset of
-- every set.
isLabelFormSubset :: LabelFormSet -> LabelFormSet -> Bool
LabelFormSet a `isLabelFormSubset` LabelFormSet b = a == a .&. b
{-# INLINE isLabelFormSubset #-}

-- | Membership test: does the given 'LabelForm' belong to the
-- set?  Returns 'False' for 'NoLabel' against any set.
memberLabelFormSet :: LabelForm -> LabelFormSet -> Bool
memberLabelFormSet (LabelForm_ form) (LabelFormSet b)
    | form < 9  = testBit b (fromIntegral form)
    | otherwise = False
{-# INLINE memberLabelFormSet #-}

-- | Set difference: @a \`withoutLabelFormSet\` b@ removes every
-- element of @b@ from @a@.  Companion to '<>' (union).
withoutLabelFormSet :: LabelFormSet -> LabelFormSet -> LabelFormSet
LabelFormSet a `withoutLabelFormSet` LabelFormSet b =
    LabelFormSet (a .&. complement b)
{-# INLINE withoutLabelFormSet #-}

----------------------------------------------------------------------
-- Pre-built sets
----------------------------------------------------------------------

-- | Every label classification that arises from a clean-path
-- validation outcome: 'LDH', 'RLDH', 'FAKEA', 'ALABEL',
-- 'ULABEL', 'ATTRLEAF', 'OCTET', 'WILDLABEL'.
--
-- /Excludes/ 'LAXULABEL': that classification is the explicit
-- opt-in for admitting U-labels that fail strict IDN
-- validation, and on the unparse side admitting it suppresses
-- the cross-label Bidi check.  To include it, write
-- @allLabelForms '<+>' 'LAXULABEL'@ (or @\"all,+laxulabel\"@
-- in command-line option syntax).
allLabelForms :: LabelFormSet
allLabelForms = LabelFormSet 0x00FF

-- | The strict subset for internationalised domain names:
-- ordinary hostname-style labels ('LDH'), valid A-labels, and
-- Unicode labels.
idnLabelForms :: LabelFormSet
idnLabelForms = mempty <+> LDH <+> ALABEL <+> ULABEL

-- | Permissive hostname-like labels: 'idnLabelForms' plus 'RLDH' and
-- 'FAKEA'.  Useful when dealing with names found in the wild where
-- unusual but syntactically valid prior to IDNA2008 LDH labels may appear.
hostnameLabelForms :: LabelFormSet
hostnameLabelForms = idnLabelForms <+> RLDH <+> FAKEA

----------------------------------------------------------------------
-- Command-line option syntax
----------------------------------------------------------------------

-- | Single-bit token table for the command-line option syntax.
labelFormSetTokens :: [TokenEntry LabelFormSet]
labelFormSetTokens =
    [ TokenEntry (labelFormToSet LDH)       "ldh"        []
    , TokenEntry (labelFormToSet RLDH)      "rldh"       []
    , TokenEntry (labelFormToSet FAKEA)     "fakea"      []
    , TokenEntry (labelFormToSet ALABEL)    "alabel"     []
    , TokenEntry (labelFormToSet ULABEL)    "ulabel"     []
    , TokenEntry (labelFormToSet ATTRLEAF)  "attrleaf"   []
    , TokenEntry (labelFormToSet OCTET)     "octet"      []
    , TokenEntry (labelFormToSet WILDLABEL) "wildlabel"  []
    , TokenEntry (labelFormToSet LAXULABEL) "laxulabel"  []
    ]

-- | Preset names that expand to multi-bit form sets.
labelFormSetPresets :: [Preset LabelFormSet]
labelFormSetPresets =
    [ ("idn",    idnLabelForms)
    , ("strict", idnLabelForms)             -- alias of @idn@
    , ("host",   hostnameLabelForms)
    , ("all",    allLabelForms)
    ]

-- | Parse a comma-separated CLI value as a 'LabelFormSet'.
-- The first argument is the application-specific default.
--
-- Each comma-separated token may optionally be prefixed with
-- @\'+\'@ (additive, the default) or @\'-\'@ (subtractive).
-- If the first token has a sign prefix, the running result is
-- seeded with the supplied default; otherwise the running
-- result starts empty and the tokens replace the default
-- cleanly.
--
-- Tokens are matched case-insensitively, with unambiguous prefix
-- matching (3-character minimum) as a fallback when there's no
-- exact match.  The token @default@ or an unambiguous prefix
-- matches the supplied default value.
parseLabelFormSet :: LabelFormSet -- ^ Application-specific default
                  -> ByteString   -- ^ Tokens to parse
                  -> Either String LabelFormSet
parseLabelFormSet !defS =
    parseTokens labelFormSetTokens
                (("default", defS) : labelFormSetPresets)
                defS
                withoutLabelFormSet

-- | 'String'-based wrapper for 'parseLabelFormSet': validates that
-- the input is pure ASCII.
parseLabelFormSetStr :: LabelFormSet -> String -> Either String LabelFormSet
parseLabelFormSetStr !defS = asciiByteString >=> parseLabelFormSet defS
