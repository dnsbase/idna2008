#!/usr/bin/env python3
"""
Generate Text.IDNA2008.Internal.Script.Data from the Unicode UCD
Scripts.txt extracted property file.

Usage:
    python3 internal/tools/genIdnaScript.py <unicode-version> \\
            <Scripts.txt>

Reads Scripts.txt and emits a complete Haskell module to stdout with
three tables:

  * 'greekRanges'  -- codepoints with Script = Greek
  * 'hebrewRanges' -- codepoints with Script = Hebrew
  * 'hkhRanges'    -- the union Hiragana \\| Katakana \\| Han

Each table is encoded as alternating @start, end@ Word32 entries
(both endpoints inclusive), sorted by start with abutting ranges
coalesced.  RFC 5892 Appendix A.4-A.7 are the consumers; the
Hiragana/Katakana/Han trio is merged because A.7 only ever asks
for "any of these three".

Source: https://www.unicode.org/Public/<version>/ucd/Scripts.txt
"""
from __future__ import annotations

import re
import sys
from typing import Iterable

# Property-file line: hex-cp[..hex-cp] ; property-name [# comment]
LINE = re.compile(
    r"^\s*([0-9A-Fa-f]+)(?:\.\.([0-9A-Fa-f]+))?\s*;\s*([A-Za-z0-9_]+)"
)


def parse_lines(path: str, wanted: set[str]) -> dict[str, list[tuple[int, int]]]:
    """Return {script -> [(start, end)]} for every entry whose
    script name is in 'wanted'."""
    out: dict[str, list[tuple[int, int]]] = {s: [] for s in wanted}
    with open(path, encoding="utf-8") as f:
        for raw in f:
            line = raw.split("#", 1)[0]
            m = LINE.match(line)
            if not m:
                continue
            script = m.group(3)
            if script not in wanted:
                continue
            start = int(m.group(1), 16)
            end = int(m.group(2), 16) if m.group(2) else start
            out[script].append((start, end))
    return out


def coalesce(pairs: Iterable[tuple[int, int]]) -> list[tuple[int, int]]:
    """Sort and merge adjacent (or overlapping) (start, end) pairs."""
    pairs = sorted(pairs, key=lambda p: p[0])
    out: list[tuple[int, int]] = []
    for s, e in pairs:
        if out and out[-1][1] + 1 >= s:
            ps, pe = out[-1]
            out[-1] = (ps, max(pe, e))
        else:
            out.append((s, e))
    return out


def cp_hex(cp: int) -> str:
    """Format a codepoint as 0xNNNNNN with at least 6 hex digits."""
    return f"0x{cp:06X}"


def emit_word32_pairs(name: str, pairs: list[tuple[int, int]]) -> str:
    """Render @name :: ByteArray ; name = PBA.byteArrayFromList [...]@
    with one (start, end) row per source line."""
    lines = [f"{name} :: ByteArray"]
    if not pairs:
        lines.append(f"{name} = PBA.byteArrayFromList ([] :: [Word32])")
        return "\n".join(lines)
    lines.append(f"{name} = PBA.byteArrayFromList")
    rendered: list[str] = []
    for i, (s, e) in enumerate(pairs):
        if i == 0:
            cell = f"({cp_hex(s)} :: Word32), {cp_hex(e)}"
        else:
            cell = f"{cp_hex(s)}, {cp_hex(e)}"
        rendered.append(cell)
    body = "\n    , ".join(rendered)
    lines.append(f"    [ {body}")
    lines.append("    ]")
    return "\n".join(lines)


HEADER = """\
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
--   python3 internal/tools/genIdnaScript.py <unicode-version> \\\\
--           <Scripts.txt>
--
-- Source: https://www.unicode.org/Public/{version}/ucd/Scripts.txt
-- Aligned with: Unicode {version}
-- Greek ranges: {n_grek}
-- Hebrew ranges: {n_hebr}
-- HKH (Hiragana | Katakana | Han) ranges: {n_hkh}
--
-- Only the four scripts whose membership is tested by RFC 5892
-- Appendix A.4-A.7 (Greek, Hebrew, Hiragana \\/ Katakana \\/ Han) are
-- represented.  Each script's @sc@ (Script) ranges are stored as a
-- sorted sequence of @(start, end)@ pairs (inclusive on both ends),
-- packed into a single 'ByteArray' as alternating 'Word32's:
--
-- > [start0, end0, start1, end1, ...]
--
-- Lookup is binary search for the largest @start_i \\<= cp@ and a
-- bounds check @cp \\<= end_i@; see "Text.IDNA2008.Internal.Script".
--
-- The Hiragana, Katakana, and Han ranges are merged into a single
-- @hkhRanges@ table because RFC 5892 A.7 only ever asks for
-- \\"any of these three\\", and the merged table costs no extra
-- comparisons at lookup time.  Greek and Hebrew remain separate
-- because A.4 only asks for Greek and A.5\\/A.6 only for Hebrew.
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

"""


SECTION_HEADING = """\

----------------------------------------------------------------------
-- {title}
----------------------------------------------------------------------

"""


def main() -> None:
    if len(sys.argv) != 3:
        sys.stderr.write(__doc__)
        sys.exit(1)

    version = sys.argv[1]
    scripts_path = sys.argv[2]

    wanted = {"Greek", "Hebrew", "Hiragana", "Katakana", "Han"}
    raw = parse_lines(scripts_path, wanted)

    greek  = coalesce(raw["Greek"])
    hebrew = coalesce(raw["Hebrew"])
    hkh    = coalesce(raw["Hiragana"] + raw["Katakana"] + raw["Han"])

    out = sys.stdout
    out.write(HEADER.format(
        version=version,
        n_grek=len(greek),
        n_hebr=len(hebrew),
        n_hkh=len(hkh),
    ))

    out.write(SECTION_HEADING.format(title="Greek (Script: Grek)"))
    out.write("-- | Number of (start, end) range pairs in 'greekRanges'.\n")
    out.write("greekRangeCount :: Int\n")
    out.write(f"greekRangeCount = {len(greek)}\n\n")
    out.write("-- | Greek script codepoint ranges, alternating start\\/end.\n")
    out.write(emit_word32_pairs("greekRanges", greek))
    out.write("\n")

    out.write(SECTION_HEADING.format(title="Hebrew (Script: Hebr)"))
    out.write("-- | Number of (start, end) range pairs in 'hebrewRanges'.\n")
    out.write("hebrewRangeCount :: Int\n")
    out.write(f"hebrewRangeCount = {len(hebrew)}\n\n")
    out.write("-- | Hebrew script codepoint ranges, alternating start\\/end.\n")
    out.write(emit_word32_pairs("hebrewRanges", hebrew))
    out.write("\n")

    out.write(SECTION_HEADING.format(
        title="Hiragana | Katakana | Han (merged for RFC 5892 A.7)"))
    out.write("-- | Number of (start, end) range pairs in 'hkhRanges'.\n")
    out.write("hkhRangeCount :: Int\n")
    out.write(f"hkhRangeCount = {len(hkh)}\n\n")
    out.write("-- | Codepoints whose @sc@ (Script) is Hiragana, Katakana,\n")
    out.write("-- or Han, merged into one sorted range list.\n")
    out.write(emit_word32_pairs("hkhRanges", hkh))
    out.write("\n")


if __name__ == "__main__":
    main()
