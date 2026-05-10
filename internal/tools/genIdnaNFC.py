#!/usr/bin/env python3
"""
Generate Text.IDNA2008.Internal.NFC.Data from the Unicode UCD
file DerivedNormalizationProps.txt.

Usage:
    python3 internal/tools/genIdnaNFC.py <unicode-version> \\
            <DerivedNormalizationProps.txt>

Reads NFC_Quick_Check entries (the lines tagged @NFC_QC@ with
values @N@ or @M@) and writes a complete Haskell module to
stdout with two parallel range tables: codepoints whose
@NFC_QC = No@ and those whose @NFC_QC = Maybe@.  Codepoints
not covered by either are implicitly @Yes@.

The source file lives at:

    https://www.unicode.org/Public/<version>/ucd/DerivedNormalizationProps.txt
"""
from __future__ import annotations

import re
import sys
from typing import Iterable

# Property-file line shape:
#   start[..end]  ; NFC_QC; <Y|N|M> [# comment]
# We only care about NFC_QC; everything else is skipped.
LINE = re.compile(
    r"^\s*([0-9A-Fa-f]+)(?:\.\.([0-9A-Fa-f]+))?\s*;\s*NFC_QC\s*;\s*([NMY])\b"
)


def parse_lines(path: str, wanted_values: set[str]) -> list[tuple[int, int, str]]:
    """Return [(start, end, value)] for every NFC_QC line whose
    third column is in `wanted_values`.  Lines are kept as given
    (no coalescing yet)."""
    out: list[tuple[int, int, str]] = []
    with open(path, encoding="utf-8") as f:
        for raw in f:
            line = raw.split("#", 1)[0]
            m = LINE.match(line)
            if not m:
                continue
            start = int(m.group(1), 16)
            end = int(m.group(2), 16) if m.group(2) else start
            value = m.group(3)
            if value in wanted_values:
                out.append((start, end, value))
    return out


def coalesce_pairs(pairs: Iterable[tuple[int, int]]) -> list[tuple[int, int]]:
    """Sort and merge adjacent (start, end) ranges."""
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
    return f"0x{cp:06X}"


def emit_ranges(
    base: str,
    qc_label: str,
    ranges: list[tuple[int, int]],
) -> str:
    """Emit @<base>RangeCount@ and @<base>Ranges@ bindings for
    the given coalesced (start, end) list, with comments
    referring to the QC label (\"No\" or \"Maybe\")."""
    name = f"{base}Ranges"
    count_name = f"{base}RangeCount"
    n = len(ranges)
    lines = []
    lines.append(f"-- | Number of (start, end) range pairs in '{name}'.")
    lines.append(f"{count_name} :: Int")
    lines.append(f"{count_name} = {n}")
    lines.append("")
    lines.append(
        f"-- | Codepoint ranges with @NFC_Quick_Check = {qc_label}@,"
    )
    lines.append("-- alternating start\\/end @Word32@ pairs.")
    lines.append(f"{name} :: ByteArray")
    if n == 0:
        lines.append(f"{name} = PBA.byteArrayFromList ([] :: [Word32])")
        return "\n".join(lines)
    lines.append(f"{name} = PBA.byteArrayFromList")
    flat: list[str] = []
    for i, (s, e) in enumerate(ranges):
        if i == 0:
            flat.append(f"({cp_hex(s)} :: Word32)")
        else:
            flat.append(cp_hex(s))
        flat.append(cp_hex(e))
    body = "\n    , ".join(flat)
    lines.append(f"    [ {body}")
    lines.append("    ]")
    return "\n".join(lines)


HEADER = """\
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
--   python3 internal/tools/genIdnaNFC.py <unicode-version> \\\\
--           <DerivedNormalizationProps.txt>
--
-- Source: https://www.unicode.org/Public/{version}/ucd/DerivedNormalizationProps.txt
-- Aligned with: Unicode {version}
-- NFC_QC=No  ranges: {n_no}
-- NFC_QC=Maybe ranges: {n_maybe}
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

"""


SECTION_BREAK = """

----------------------------------------------------------------------
-- NFC_Quick_Check = Maybe
----------------------------------------------------------------------

"""


def main() -> None:
    if len(sys.argv) != 3:
        sys.stderr.write(__doc__)
        sys.exit(1)

    version = sys.argv[1]
    props_path = sys.argv[2]

    raw = parse_lines(props_path, {"N", "M"})
    no_pairs = coalesce_pairs([(s, e) for (s, e, v) in raw if v == "N"])
    maybe_pairs = coalesce_pairs([(s, e) for (s, e, v) in raw if v == "M"])

    out = sys.stdout
    out.write(
        HEADER.format(
            version=version,
            n_no=len(no_pairs),
            n_maybe=len(maybe_pairs),
        )
    )
    out.write(emit_ranges("nfcNo", "No", no_pairs))
    out.write("\n")
    out.write(SECTION_BREAK)
    out.write(emit_ranges("nfcMaybe", "Maybe", maybe_pairs))
    out.write("\n")


if __name__ == "__main__":
    main()
