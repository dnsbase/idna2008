#!/usr/bin/env python3
"""
Generate Text.IDNA2008.Internal.Width.Data from UnicodeData.txt.

Usage:
    python3 internal/tools/genIdnaWidth.py <unicode-version> \\
            <UnicodeData.txt>

Reads field 5 (Decomposition_Mapping) for each codepoint and keeps
those tagged <wide> or <narrow>, emitting two parallel sorted Word32
arrays of (source, target) codepoints for the RFC 5895 section 2.2
fullwidth/halfwidth mapping.

Source: https://www.unicode.org/Public/<version>/ucd/UnicodeData.txt
"""
from __future__ import annotations

import re
import sys

WIDE_NARROW_RE = re.compile(r"^<(wide|narrow)>\s+([0-9A-Fa-f]+)\s*$")


def parse(path: str) -> list[tuple[int, int]]:
    """Return [(source, target)] for every codepoint with a single-
    codepoint <wide> or <narrow> decomposition."""
    out: list[tuple[int, int]] = []
    with open(path, encoding="utf-8") as f:
        for line in f:
            fields = line.rstrip("\n").split(";")
            if len(fields) < 6:
                continue
            cp = int(fields[0], 16)
            decomp = fields[5]
            m = WIDE_NARROW_RE.match(decomp)
            if not m:
                continue
            target = int(m.group(2), 16)
            out.append((cp, target))
    return sorted(out)


def cp_hex(cp: int) -> str:
    return f"0x{cp:06X}"


def emit_word32_list(name: str, items: list[int]) -> str:
    lines = [f"{name} :: ByteArray"]
    if not items:
        lines.append(f"{name} = PBA.byteArrayFromList ([] :: [Word32])")
        return "\n".join(lines)
    lines.append(f"{name} = PBA.byteArrayFromList")
    rendered = []
    for i, v in enumerate(items):
        cell = f"({cp_hex(v)} :: Word32)" if i == 0 else cp_hex(v)
        rendered.append(cell)
    body = "\n    , ".join(rendered)
    lines.append(f"    [ {body}")
    lines.append("    ]")
    return "\n".join(lines)


HEADER = """\
-- |
-- Module      : Text.IDNA2008.Internal.Width.Data
-- Description : Compact codepoint mapping table for the Unicode
--               wide \\/ narrow decompositions consumed by the
--               RFC 5895 section 2.2 (fullwidth\\/halfwidth) mapping.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- AUTOMATICALLY GENERATED.  Do not edit by hand.  Regenerate with:
--
--   python3 internal/tools/genIdnaWidth.py <unicode-version> \\\\
--           <UnicodeData.txt>
--
-- Source: https://www.unicode.org/Public/{version}/ucd/UnicodeData.txt
-- Aligned with: Unicode {version}
-- Width-mapping entries: {n}
--
-- Each entry is a single-codepoint decomposition: a fullwidth or
-- halfwidth codepoint that maps to a single canonical-width target.
-- Stored as two parallel sorted arrays -- 'widthSources' (source
-- codepoints, sorted ascending) and 'widthTargets' (the targets, in
-- the same order).  Lookup is binary search over 'widthSources'.
module Text.IDNA2008.Internal.Width.Data
    ( widthRangeCount
    , widthSources
    , widthTargets
    ) where

import Data.Array.Byte (ByteArray)
import Data.Word (Word32)
import qualified Data.Primitive.ByteArray as PBA

"""


def main() -> None:
    if len(sys.argv) != 3:
        sys.stderr.write(__doc__)
        sys.exit(1)
    version = sys.argv[1]
    pairs = parse(sys.argv[2])
    sources = [s for s, _ in pairs]
    targets = [t for _, t in pairs]

    out = sys.stdout
    out.write(HEADER.format(version=version, n=len(pairs)))

    out.write("-- | Number of (source, target) entries.\n")
    out.write("widthRangeCount :: Int\n")
    out.write(f"widthRangeCount = {len(pairs)}\n\n")

    out.write("-- | Source codepoints, sorted ascending.\n")
    out.write(emit_word32_list("widthSources", sources))
    out.write("\n\n")

    out.write("-- | Target codepoints; parallel to 'widthSources'.\n")
    out.write(emit_word32_list("widthTargets", targets))
    out.write("\n")


if __name__ == "__main__":
    main()
