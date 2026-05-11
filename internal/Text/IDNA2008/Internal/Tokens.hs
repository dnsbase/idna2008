-- |
-- Module      : Text.IDNA2008.Internal.Tokens
-- Description : Command-line option syntax for bit-flag types.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Reusable parser and renderer for monoidal bit-flag types
-- ('Text.IDNA2008.Internal.LabelFormSet.LabelFormSet',
-- 'Text.IDNA2008.Internal.Flags.IdnaFlags') exposed on the
-- command line as comma-separated lists of canonical names with
-- @+@\/@-@ prefix syntax for set arithmetic.
--
-- Each type provides a table of 'TokenEntry' values (one per
-- single-bit flag, with a canonical CLI name and an optional list
-- of memorable aliases) plus an optional list of preset names.
-- The functions in this module turn that data into a parser and a
-- renderer.
--
-- Lookup is case-insensitive.  An exact match against the
-- canonical name, any alias, or any preset wins.  If no exact
-- match is found, a unique prefix match against the same union
-- is accepted; ambiguous prefixes are rejected with an error
-- listing the candidates.
{-# LANGUAGE CPP #-}

module Text.IDNA2008.Internal.Tokens
    ( -- * Token table entries
      TokenEntry(..)
    , Preset
      -- * Parser
    , parseTokens
      -- * String-input helper (ASCII validation)
    , asciiByteString
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.Char (isSpace, toLower)
#if !MIN_VERSION_base(4,20,0)
import Data.Foldable(foldl')
#endif
import Data.List (intercalate, nubBy, sortOn)

----------------------------------------------------------------------
-- Token-table types
----------------------------------------------------------------------

-- | One row of a CLI token table: a single-bit flag value plus
-- the canonical CLI name and any number of additional aliases
-- accepted on input.  Aliases are case-insensitive; the
-- canonical name (also case-insensitive on input) is the one the
-- renderer emits.
data TokenEntry a = TokenEntry
    { teValue     :: !a            -- ^ The flag this row describes.
    , teCanonical :: !ByteString   -- ^ Canonical CLI name (lower-case).
    , teAliases   :: ![ByteString] -- ^ Additional accepted forms.
    }

-- | A preset is a named multi-bit composite (e.g. @host@ for
-- 'Text.IDNA2008.Internal.LabelFormSet.LabelFormSet').  Presets are
-- an input-side convenience and are never emitted by the
-- renderer.
type Preset a = (ByteString, a)

----------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------

-- | Parse a comma-separated CLI value into a monoidal flag set.
--
-- Each comma-separated token is optionally prefixed with @\'+\'@
-- (additive, the default) or @\'-\'@ (subtractive).  The token
-- name is resolved against the canonical names, aliases, and
-- preset names; case-insensitively, exact match preferred,
-- unambiguous prefix match accepted as fallback.  Prefix
-- matches require at least three characters of input so that
-- short shortcuts that happen to be unambiguous today don't
-- quietly break when a future release adds a collision.
--
-- The starting state of the running result depends on the first
-- token's leading character:
--
--   * If the first token has a @\'+\'@ or @\'-\'@ prefix, the
--     running result is seeded with the caller-supplied
--     @implicitBase@.  This makes @\"+X\"@ read naturally as
--     \"add X to my default\" and @\"-X\"@ as \"remove X from
--     my default\".
--   * Otherwise the running result starts at 'mempty', so a
--     leading absolute token replaces the default cleanly.
--
-- Whitespace around commas and around tokens is ignored.  An
-- empty input string yields 'mempty'.
parseTokens
    :: forall a. (Monoid a)
    => [TokenEntry a]      -- ^ Single-bit token table.
    -> [Preset a]          -- ^ Multi-bit presets.
    -> a                   -- ^ Implicit base used when input
                           --   begins with @+@ or @-@.
    -> (a -> a -> a)       -- ^ Subtractive op (set \`op\` value).
    -> ByteString          -- ^ Input.
    -> Either String a
parseTokens !table !presets !implicitBase !withoutOp !input =
    foldl' step (Right initial) tokens
  where
    tokens  = splitComma input
    initial = case tokens of
                (t:_) | hasLeadingSign (trim t) -> implicitBase
                _                                -> mempty

    hasLeadingSign b = case BS8.uncons b of
        Just ('+', _) -> True
        Just ('-', _) -> True
        _             -> False

    -- Flat name -> value map, with names lower-cased.
    nameMap :: [(ByteString, a)]
    nameMap = nubFstOn $
        concatMap entryNames table
        ++ [(BS8.map toLower n, v) | (n, v) <- presets]

    entryNames :: TokenEntry a -> [(ByteString, a)]
    entryNames (TokenEntry v c as) =
        [(BS8.map toLower n, v) | n <- c : as]

    -- Sorted by length so prefix matching reports candidates in
    -- a stable, readable order.
    sortedNames :: [(ByteString, a)]
    sortedNames = sortOn (BS.length . fst) nameMap

    step :: Either String a -> ByteString -> Either String a
    step (Left e) _    = Left e
    step (Right s) tok =
        let (op, name) = splitOp (trim tok)
        in if BS.null name
             then Left "empty token in CLI flag list"
             else case lookupName name of
                 Right v -> Right (op s v)
                 Left  e -> Left e

    splitOp :: ByteString -> (a -> a -> a, ByteString)
    splitOp b = case BS8.uncons b of
        Just ('+', rest) -> ((<>),       rest)
        Just ('-', rest) -> (withoutOp,  rest)
        _                -> ((<>),       b)

    lookupName :: ByteString -> Either String a
    lookupName !name = case lookup nameLc nameMap of
        Just v  -> Right v
        Nothing
          | BS.length nameLc < minPrefixLen ->
              Left $ "token " ++ show (BS8.unpack name)
                  ++ " is too short for prefix matching"
                  ++ " (need at least "
                  ++ show minPrefixLen
                  ++ " characters)"
          | otherwise -> case prefixMatches of
              [(_, v)]   -> Right v
              []         -> Left $ "unknown token "
                                ++ show (BS8.unpack name)
              cs         -> Left $ "ambiguous token "
                                ++ show (BS8.unpack name)
                                ++ "; matches: "
                                ++ intercalate ", "
                                     [BS8.unpack n | (n, _) <- cs]
      where
        nameLc = BS8.map toLower name
        prefixMatches =
            [ entry | entry@(n, _) <- sortedNames
                    , nameLc `BS.isPrefixOf` n ]

    -- | Minimum input length for an inexact (prefix) match.  An
    -- exact match is always accepted regardless of length; this
    -- floor only constrains the prefix-match fallback so that
    -- short shortcuts can't be added implicitly today and
    -- silently invalidated by a later release that shares the
    -- prefix.  Three characters is enough for the short prefixes
    -- we want today (@asc@, @bid@, @att@, ...) while leaving
    -- room for future tokens to share a 1- or 2-letter prefix
    -- without breaking existing scripts.
    minPrefixLen :: Int
    minPrefixLen = 3

----------------------------------------------------------------------
-- String-input helper
----------------------------------------------------------------------

-- | Validate that a 'String' contains only ASCII codepoints
-- (@\< '\\x80'@) and convert it to a 'ByteString' suitable for
-- 'parseTokens'.
asciiByteString :: String -> Either String ByteString
asciiByteString s = case break (>= '\x80') s of
    (ok, [])     -> Right (BS8.pack ok)
    (ok, bad:_)  -> Left $ "non-ASCII character "
                        ++ show bad
                        ++ " at column "
                        ++ show (length ok + 1)
                        ++ " of "
                        ++ show s

----------------------------------------------------------------------
-- Internals
----------------------------------------------------------------------

-- | Split @input@ on commas, dropping empty fragments at the
-- ends and around adjacent commas.
splitComma :: ByteString -> [ByteString]
splitComma !s =
    filter (not . BS.null . trim)
           (BS.split (fromIntegral (fromEnum ',')) s)

-- | Strip ASCII whitespace from both ends.
trim :: ByteString -> ByteString
trim = BS8.dropWhileEnd isSpace . BS8.dropWhile isSpace

-- | Deduplicate by first component, keeping the first
-- occurrence.  Used to discard accidentally-duplicated entries
-- (e.g. an alias that matches the canonical name of a different
-- token in a misconfigured table).
nubFstOn :: Eq k => [(k, v)] -> [(k, v)]
nubFstOn = nubBy (\(a, _) (b, _) -> a == b)
