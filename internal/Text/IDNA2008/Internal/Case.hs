-- |
-- Module      : Text.IDNA2008.Internal.Case
-- Description : Unicode simple case folding for MAPCASE.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Simple (1:1) Unicode lowercase mapping pinned to the UCD version
-- this library tracks (see @x-unicode-version@ in @idna2008.cabal@,
-- and re-run @internal/tools/update@ to advance).
--
-- This is the case folder consulted by the @MAPCASE@ input mapping.
-- Compared to "Data.Char"'s @toLower@, the behaviour is the same in
-- shape — @Char -> Char@, single-codepoint output, no language or
-- locale-specific exceptions — but the table is fixed by this
-- library's UCD pin rather than by whatever @base@ happens to ship,
-- so cased letters introduced in Unicode releases newer than @base@
-- still fold correctly.
--
-- The @Char -> Char@ signature is the spec: ligatures (e.g.
-- @U+0132@) and digraphs (e.g. @U+01C4@) stay as single
-- codepoints, mapping to the corresponding lower case form; no
-- decomposition into multi-letter sequences happens here.

module Text.IDNA2008.Internal.Case
    ( toLower
    ) where

import Text.IDNA2008.Internal.Case.Data (toLower)
