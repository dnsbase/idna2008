#!/usr/bin/env python3
"""
Generate Text.IDNA2008.Internal.NFC.Tables.Data from the Unicode
UCD files UnicodeData.txt and CompositionExclusions.txt.

Usage:
    python3 internal/tools/genIdnaNFCTables.py <unicode-version> \\
            <UnicodeData.txt> <CompositionExclusions.txt>

Reads canonical decomposition mappings (field 5 of UnicodeData.txt
without a @<...>@ tag), Canonical_Combining_Class (field 3), and the
list of codepoints excluded from composition.  Emits a complete
Haskell module to stdout with three tables:

  * Canonical decomposition: source codepoint -> (first, second)
    where second == 0 denotes a singleton decomposition.

  * Primary composition: (first, second) -> result, restricted to
    decompositions that are not singletons, where the first
    constituent is a starter (CCC = 0), and where the source is
    not in the Composition_Exclusion set.

  * Canonical_Combining_Class: codepoint -> CCC value, encoded as
    sorted ranges of equal-CCC runs.

Hangul syllables are intentionally excluded from all three tables;
they are handled by closed-form math in the consumer module.
"""
from __future__ import annotations

import re
import sys
from typing import Iterable

# UnicodeData.txt is one record per line, semicolon-separated, 15
# fields.  We only care about a few of them; just split() and index.
DECOMP_TAG = re.compile(r"^<[^>]+>\s*")

# Hangul ranges (decomposed via algorithmic math, not table).
HANGUL_S_BASE = 0xAC00
HANGUL_S_COUNT = 11172  # 0xAC00 .. 0xD7A3
HANGUL_L_BASE = 0x1100
HANGUL_L_COUNT = 19
HANGUL_V_BASE = 0x1161
HANGUL_V_COUNT = 21
HANGUL_T_BASE = 0x11A7
HANGUL_T_COUNT = 28
HANGUL_END = HANGUL_S_BASE + HANGUL_S_COUNT - 1


def is_hangul_syllable(cp: int) -> bool:
    return HANGUL_S_BASE <= cp <= HANGUL_END


def parse_unicode_data(
    path: str,
) -> tuple[
    dict[int, list[int]],  # source -> canonical decomposition cps
    dict[int, int],        # codepoint -> CCC (only entries with CCC > 0)
]:
    decomps: dict[int, list[int]] = {}
    cccs: dict[int, int] = {}
    with open(path, encoding="utf-8") as f:
        for raw in f:
            line = raw.rstrip("\n")
            if not line:
                continue
            fields = line.split(";")
            if len(fields) < 15:
                continue
            cp = int(fields[0], 16)
            if is_hangul_syllable(cp):
                # Hangul handled algorithmically; skip the table.
                continue
            ccc_str = fields[3].strip()
            if ccc_str:
                ccc = int(ccc_str)
                if ccc != 0:
                    cccs[cp] = ccc
            decomp_field = fields[5].strip()
            if not decomp_field:
                continue
            # Strip any "<compatibility-tag>" prefix; canonical entries
            # have no tag.
            if DECOMP_TAG.match(decomp_field):
                continue
            parts = decomp_field.split()
            decomp = [int(p, 16) for p in parts]
            if decomp:
                decomps[cp] = decomp
    return decomps, cccs


def parse_composition_exclusions(path: str) -> set[int]:
    excluded: set[int] = set()
    with open(path, encoding="utf-8") as f:
        for raw in f:
            line = raw.split("#", 1)[0].strip()
            if not line:
                continue
            # Some lines are ranges "start..end"; most are single cps.
            if ".." in line:
                lo, hi = line.split("..", 1)
                lo_cp = int(lo.strip(), 16)
                hi_cp = int(hi.strip(), 16)
                for cp in range(lo_cp, hi_cp + 1):
                    excluded.add(cp)
            else:
                excluded.add(int(line, 16))
    return excluded


def coalesce_ccc(cccs: dict[int, int]) -> list[tuple[int, int, int]]:
    """Group consecutive codepoints with equal CCC into ranges,
    sorted by start.  Returns [(start, end, ccc)]."""
    items = sorted(cccs.items())
    out: list[tuple[int, int, int]] = []
    for cp, v in items:
        if out and out[-1][2] == v and out[-1][1] + 1 == cp:
            ps, _, pv = out[-1]
            out[-1] = (ps, cp, pv)
        else:
            out.append((cp, cp, v))
    return out


def build_composition(
    decomps: dict[int, list[int]],
    excluded: set[int],
    cccs: dict[int, int],
) -> list[tuple[int, int, int]]:
    """Reverse the decomposition map into primary composition entries.
    A composition (first, second) -> result is included iff:

      * The decomposition has exactly two codepoints.
      * The result is not in the Composition_Exclusion set.
      * The first constituent is a starter (CCC == 0), so cccs
        does not list it.
    """
    out: list[tuple[int, int, int]] = []
    for src, decomp in decomps.items():
        if len(decomp) != 2:
            continue
        if src in excluded:
            continue
        first, second = decomp
        if cccs.get(first, 0) != 0:
            # Non-starter primary; UAX #15 excludes these from
            # primary composition.
            continue
        out.append((first, second, src))
    out.sort(key=lambda t: (t[0], t[1]))
    return out


def cp_hex(cp: int) -> str:
    return f"0x{cp:06X}"


def emit_word32_array(name: str, items: Iterable[int]) -> str:
    items = list(items)
    n = len(items)
    lines: list[str] = []
    lines.append(f"{name} :: ByteArray")
    if n == 0:
        lines.append(f"{name} = PBA.byteArrayFromList ([] :: [Word32])")
        return "\n".join(lines)
    lines.append(f"{name} = PBA.byteArrayFromList")
    pieces: list[str] = []
    for i, v in enumerate(items):
        pieces.append(f"({cp_hex(v)} :: Word32)" if i == 0 else cp_hex(v))
    body = "\n    , ".join(pieces)
    lines.append(f"    [ {body}")
    lines.append("    ]")
    return "\n".join(lines)


def emit_word8_array(name: str, items: Iterable[int]) -> str:
    items = list(items)
    n = len(items)
    lines: list[str] = []
    lines.append(f"{name} :: ByteArray")
    if n == 0:
        lines.append(f"{name} = PBA.byteArrayFromList ([] :: [Word8])")
        return "\n".join(lines)
    lines.append(f"{name} = PBA.byteArrayFromList")
    pieces: list[str] = []
    for i, v in enumerate(items):
        pieces.append(f"({v} :: Word8)" if i == 0 else str(v))
    body = "\n    , ".join(pieces)
    lines.append(f"    [ {body}")
    lines.append("    ]")
    return "\n".join(lines)


HEADER = """\
-- |
-- Module      : Text.IDNA2008.Internal.NFC.Tables.Data
-- Description : Canonical decomposition, primary composition, and
--               Canonical_Combining_Class tables used by the full
--               NFC normalize-and-compare validator.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- AUTOMATICALLY GENERATED.  Do not edit by hand.  Regenerate with:
--
--   python3 internal/tools/genIdnaNFCTables.py <unicode-version> \\\\
--           <UnicodeData.txt> <CompositionExclusions.txt>
--
-- Source: https://www.unicode.org/Public/{version}/ucd/
-- Aligned with: Unicode {version}
-- Decomposition entries: {n_decomp}
-- Composition entries: {n_comp}
-- CCC ranges: {n_ccc}
--
-- Hangul syllables are excluded from all three tables; the
-- consumer module handles them via closed-form arithmetic.
module Text.IDNA2008.Internal.NFC.Tables.Data
    ( -- * Canonical decomposition
      decompCount
    , decompKeys
    , decompFirst
    , decompSecond

      -- * Primary composition
    , compCount
    , compFirst
    , compSecond
    , compResult

      -- * Canonical_Combining_Class
    , cccCount
    , cccStarts
    , cccEnds
    , cccValues
    ) where

import Data.Array.Byte (ByteArray)
import Data.Word (Word8, Word32)
import qualified Data.Primitive.ByteArray as PBA

----------------------------------------------------------------------
-- Canonical decomposition
----------------------------------------------------------------------

decompCount :: Int
decompCount = {n_decomp}

"""

COMP_HEADING = """

----------------------------------------------------------------------
-- Primary composition
----------------------------------------------------------------------

compCount :: Int
compCount = {n_comp}

"""

CCC_HEADING = """

----------------------------------------------------------------------
-- Canonical_Combining_Class (CCC)
----------------------------------------------------------------------

cccCount :: Int
cccCount = {n_ccc}

"""


def main() -> None:
    if len(sys.argv) != 4:
        sys.stderr.write(__doc__)
        sys.exit(1)

    version = sys.argv[1]
    udata_path = sys.argv[2]
    excl_path = sys.argv[3]

    decomps, cccs = parse_unicode_data(udata_path)
    excluded = parse_composition_exclusions(excl_path)
    comps = build_composition(decomps, excluded, cccs)
    ccc_ranges = coalesce_ccc(cccs)

    # Sort decomp by source codepoint.
    decomp_items = sorted(decomps.items())
    decomp_keys = [k for (k, _) in decomp_items]
    decomp_first = [v[0] for (_, v) in decomp_items]
    decomp_second = [
        v[1] if len(v) >= 2 else 0
        for (_, v) in decomp_items
    ]

    out = sys.stdout
    out.write(
        HEADER.format(
            version=version,
            n_decomp=len(decomp_keys),
            n_comp=len(comps),
            n_ccc=len(ccc_ranges),
        )
    )
    out.write(emit_word32_array("decompKeys", decomp_keys))
    out.write("\n\n")
    out.write(emit_word32_array("decompFirst", decomp_first))
    out.write("\n\n")
    out.write(emit_word32_array("decompSecond", decomp_second))
    out.write("\n")

    out.write(COMP_HEADING.format(n_comp=len(comps)))
    out.write(emit_word32_array("compFirst",  [c[0] for c in comps]))
    out.write("\n\n")
    out.write(emit_word32_array("compSecond", [c[1] for c in comps]))
    out.write("\n\n")
    out.write(emit_word32_array("compResult", [c[2] for c in comps]))
    out.write("\n")

    out.write(CCC_HEADING.format(n_ccc=len(ccc_ranges)))
    out.write(emit_word32_array("cccStarts", [r[0] for r in ccc_ranges]))
    out.write("\n\n")
    out.write(emit_word32_array("cccEnds",   [r[1] for r in ccc_ranges]))
    out.write("\n\n")
    out.write(emit_word8_array ("cccValues", [r[2] for r in ccc_ranges]))
    out.write("\n")


if __name__ == "__main__":
    main()
