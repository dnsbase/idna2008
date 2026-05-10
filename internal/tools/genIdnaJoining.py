#!/usr/bin/env python3
"""
Generate Text.IDNA2008.Internal.Joining.Data from the Unicode UCD
extracted property files.

Usage:
    python3 internal/tools/genIdnaJoining.py <unicode-version> \\
            <DerivedJoiningType.txt> \\
            <DerivedCombiningClass.txt>

Reads the two UCD-derived property files and writes a complete Haskell
module to stdout.  Only the four Joining_Type values consulted by RFC
5892 Appendix A.1 (L, R, D, T) are kept; U (Non_Joining) and C
(Join_Causing) entries are dropped: the table-miss path in
"Text.IDNA2008.Internal.Joining" matches their "stop" semantics in
the rule, so storing them buys nothing.

Only codepoints with Canonical_Combining_Class = 9 (Virama) are kept
from the combining-class file.

Source files are the Unicode 12 (or later) UCD extracts:

    https://www.unicode.org/Public/12.0.0/ucd/extracted/DerivedJoiningType.txt
    https://www.unicode.org/Public/12.0.0/ucd/extracted/DerivedCombiningClass.txt
"""
from __future__ import annotations

import re
import sys
from typing import Iterable

# Joining_Type tags must agree with jtTagL/jtTagR/jtTagD/jtTagT in
# Text.IDNA2008.Internal.Joining.Data.
JT_TAG = {"L": 1, "R": 2, "D": 3, "T": 4}

# Property-file line: hex-cp[..hex-cp] ; property-name [# comment]
LINE = re.compile(
    r"^\s*([0-9A-Fa-f]+)(?:\.\.([0-9A-Fa-f]+))?\s*;\s*([A-Za-z0-9_]+)"
)


def parse_lines(path: str, wanted: set[str]) -> list[tuple[int, int, str]]:
    """Return [(start, end, prop)] from `path`, keeping only entries
    whose property name is in `wanted`.  Lines are parsed individually,
    not coalesced."""
    out: list[tuple[int, int, str]] = []
    with open(path, encoding="utf-8") as f:
        for raw in f:
            line = raw.split("#", 1)[0]
            m = LINE.match(line)
            if not m:
                continue
            start = int(m.group(1), 16)
            end = int(m.group(2), 16) if m.group(2) else start
            prop = m.group(3)
            if prop in wanted:
                out.append((start, end, prop))
    return out


def coalesce(triples: Iterable[tuple[int, int, str]]) -> list[tuple[int, int, str]]:
    """Sort by start and merge ranges that abut and share a property."""
    triples = sorted(triples, key=lambda t: t[0])
    out: list[tuple[int, int, str]] = []
    for s, e, p in triples:
        if out and out[-1][2] == p and out[-1][1] + 1 == s:
            ps, _, _ = out[-1]
            out[-1] = (ps, e, p)
        else:
            out.append((s, e, p))
    return out


def coalesce_pairs(pairs: Iterable[tuple[int, int]]) -> list[tuple[int, int]]:
    """Sort and merge adjacent (start, end) pairs."""
    pairs = sorted(pairs, key=lambda p: p[0])
    out: list[tuple[int, int]] = []
    for s, e in pairs:
        if out and out[-1][1] + 1 == s:
            ps, _ = out[-1]
            out[-1] = (ps, e)
        else:
            out.append((s, e))
    return out


def cp_hex(cp: int) -> str:
    """Format a codepoint as 0xNNNNNN with at least 6 hex digits."""
    return f"0x{cp:06X}"


def emit_word32_list(
    name: str,
    typ: str,
    items: list[int],
    *,
    line_comment: list[str] | None = None,
) -> str:
    """Render @name :: typ ; name = PBA.byteArrayFromList [...]@ as a
    formatted Haskell expression with one element per line.  The first
    element carries the @:: Word32@ annotation."""
    lines = [f"{name} :: {typ}", f"{name} = PBA.byteArrayFromList"]
    if not items:
        lines.append("    [ ]")
        return "\n".join(lines)
    rendered = []
    for i, v in enumerate(items):
        if i == 0:
            cell = f"({cp_hex(v)} :: Word32)"
        else:
            cell = cp_hex(v)
        comment = ""
        if line_comment is not None:
            comment = f"  -- {line_comment[i]}"
        rendered.append(f"{cell}{comment}")
    body = "\n    , ".join(rendered)
    lines.append(f"    [ {body}")
    lines.append("    ]")
    return "\n".join(lines)


def emit_word8_list(name: str, items: list[int]) -> str:
    """Same shape as 'emit_word32_list' but for Word8 tag arrays."""
    lines = [f"{name} :: ByteArray", f"{name} = PBA.byteArrayFromList"]
    if not items:
        lines.append("    [ ]")
        return "\n".join(lines)
    rendered = []
    for i, v in enumerate(items):
        if i == 0:
            cell = f"({v} :: Word8)"
        else:
            cell = str(v)
        rendered.append(cell)
    body = "\n    , ".join(rendered)
    lines.append(f"    [ {body}")
    lines.append("    ]")
    return "\n".join(lines)


HEADER = """\
-- |
-- Module      : Text.IDNA2008.Internal.Joining.Data
-- Description : Compact codepoint range tables for the Unicode
--               Joining_Type and Virama (Canonical_Combining_Class
--               = 9) properties consulted by the CONTEXTJ
--               contextual rules.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- AUTOMATICALLY GENERATED.  Do not edit by hand.  Regenerate with:
--
--   python3 internal/tools/genIdnaJoining.py <unicode-version> \\\\
--           <DerivedJoiningType.txt> <DerivedCombiningClass.txt>
--
-- Source: https://www.unicode.org/Public/{version}/ucd/extracted/
-- Aligned with: Unicode {version}
-- Joining_Type ranges: {n_jt}
-- Virama ranges: {n_v}
--
-- The four Joining_Type values relevant to RFC 5892 A.1 are encoded
-- as a 'Word8' tag per range:
--
--   * 1 = L (Left_Joining)
--   * 2 = R (Right_Joining)
--   * 3 = D (Dual_Joining)
--   * 4 = T (Transparent)
--
-- Joining_Type=C (Join_Causing, including ZWJ\\/ZWNJ themselves and
-- Arabic Tatweel @U+0640@) and Joining_Type=U (Non_Joining) are not
-- encoded: at any position a codepoint with one of those types is a
-- \\"stop\\" for the regex.
module Text.IDNA2008.Internal.Joining.Data
    ( -- * Joining_Type
      jtRangeCount
    , jtRangeStarts
    , jtRangeEnds
    , jtRangeTags

      -- * Joining_Type tag values
    , jtTagL
    , jtTagR
    , jtTagD
    , jtTagT

      -- * Virama (CCC = 9)
    , viramaRangeCount
    , viramaRanges
    ) where

import Data.Array.Byte (ByteArray)
import Data.Word (Word8, Word32)
import qualified Data.Primitive.ByteArray as PBA

----------------------------------------------------------------------
-- Joining_Type
----------------------------------------------------------------------

-- | Tag value for Joining_Type=L (Left_Joining).
jtTagL :: Word8
jtTagL = 1

-- | Tag value for Joining_Type=R (Right_Joining).
jtTagR :: Word8
jtTagR = 2

-- | Tag value for Joining_Type=D (Dual_Joining).
jtTagD :: Word8
jtTagD = 3

-- | Tag value for Joining_Type=T (Transparent).
jtTagT :: Word8
jtTagT = 4

"""


VIRAMA_HEADING = """\

----------------------------------------------------------------------
-- Virama (Canonical_Combining_Class = 9)
----------------------------------------------------------------------

"""


def main() -> None:
    if len(sys.argv) != 4:
        sys.stderr.write(__doc__)
        sys.exit(1)

    version = sys.argv[1]
    jt_path = sys.argv[2]
    ccc_path = sys.argv[3]

    # ----- Joining_Type --------------------------------------------------
    jt_long = {
        "Left_Joining": "L",
        "Right_Joining": "R",
        "Dual_Joining": "D",
        "Transparent": "T",
    }
    jt_short = set(jt_long.values())

    # Some UCD distributions use long names ("Right_Joining"), others use
    # the short codes ("R").  Accept either.
    raw = parse_lines(jt_path, set(jt_long.keys()) | jt_short)
    normalised = []
    for s, e, p in raw:
        if p in jt_long:
            p = jt_long[p]
        normalised.append((s, e, p))

    jt = coalesce(normalised)
    starts = [s for (s, _e, _p) in jt]
    ends = [e for (_s, e, _p) in jt]
    tags = [JT_TAG[p] for (_s, _e, p) in jt]

    # ----- Virama (CCC = 9) ---------------------------------------------
    raw_ccc = parse_lines(ccc_path, {"9"})
    v_pairs = coalesce_pairs([(s, e) for (s, e, _p) in raw_ccc])

    virama_flat = []
    for s, e in v_pairs:
        virama_flat.append(s)
        virama_flat.append(e)

    # ----- Emit ----------------------------------------------------------
    out = sys.stdout
    out.write(HEADER.format(version=version, n_jt=len(jt), n_v=len(v_pairs)))

    out.write(f"-- | Number of joining-type ranges.\n")
    out.write(f"jtRangeCount :: Int\n")
    out.write(f"jtRangeCount = {len(jt)}\n\n")

    out.write("-- | Inclusive range starts, sorted ascending.\n")
    out.write(emit_word32_list("jtRangeStarts", "ByteArray", starts))
    out.write("\n\n")

    out.write("-- | Inclusive range ends, parallel to 'jtRangeStarts'.\n")
    out.write(emit_word32_list("jtRangeEnds", "ByteArray", ends))
    out.write("\n\n")

    out.write("-- | Joining_Type tag for each range, parallel to\n")
    out.write("-- 'jtRangeStarts' and 'jtRangeEnds'.\n")
    out.write(emit_word8_list("jtRangeTags", tags))
    out.write("\n")

    out.write(VIRAMA_HEADING)
    out.write(f"-- | Number of (start, end) range pairs in 'viramaRanges'.\n")
    out.write(f"viramaRangeCount :: Int\n")
    out.write(f"viramaRangeCount = {len(v_pairs)}\n\n")

    out.write("-- | Codepoints with Canonical_Combining_Class = 9\n")
    out.write("-- (Virama), alternating start\\/end.\n")
    out.write(emit_word32_list("viramaRanges", "ByteArray", virama_flat))
    out.write("\n")


if __name__ == "__main__":
    main()
