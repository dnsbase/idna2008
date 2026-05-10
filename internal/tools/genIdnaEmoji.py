#!/usr/bin/env python3
"""
Generate Text.IDNA2008.Internal.Emoji.Data from the Unicode UCD
emoji-data.txt extracted property file.

Usage:
    python3 internal/tools/genIdnaEmoji.py <unicode-version> \\
            <emoji-data.txt>

Reads the UCD emoji property file and writes a complete Haskell module
to stdout.  Only entries with property name "Emoji" are kept; the
related properties (Emoji_Presentation, Emoji_Modifier,
Emoji_Modifier_Base, Emoji_Component, Extended_Pictographic) are
ignored: a codepoint is considered "an emoji" for IDNA-relaxation
purposes if and only if Emoji=Yes.

Source file (per Unicode emoji version):

    Unicode 12.0:  https://www.unicode.org/Public/emoji/12.0/emoji-data.txt
    Unicode 13.0+: https://www.unicode.org/Public/<ver>/ucd/emoji/emoji-data.txt

(In 13.0 the emoji files moved into the main UCD bundle.)
"""
from __future__ import annotations

import re
import sys
from typing import Iterable

# Property-file line: hex-cp[..hex-cp] ; property-name [# comment]
LINE = re.compile(
    r"^\s*([0-9A-Fa-f]+)(?:\.\.([0-9A-Fa-f]+))?\s*;\s*([A-Za-z0-9_]+)"
)


def parse_emoji(path: str) -> list[tuple[int, int]]:
    """Return [(start, end)] from `path` for entries with property
    name == "Emoji"."""
    out: list[tuple[int, int]] = []
    with open(path, encoding="utf-8") as f:
        for raw in f:
            line = raw.split("#", 1)[0]
            m = LINE.match(line)
            if not m:
                continue
            if m.group(3) != "Emoji":
                continue
            start = int(m.group(1), 16)
            end = int(m.group(2), 16) if m.group(2) else start
            out.append((start, end))
    return out


def coalesce_pairs(pairs: Iterable[tuple[int, int]]) -> list[tuple[int, int]]:
    """Sort and merge adjacent (start, end) pairs."""
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


def emit_word32_list(name: str, items: list[int]) -> str:
    """Render @name :: ByteArray ; name = PBA.byteArrayFromList [...]@
    as a formatted Haskell expression with one element per line.  The
    first element carries the @:: Word32@ annotation."""
    lines = [f"{name} :: ByteArray"]
    if not items:
        # Empty-list literals need an explicit element type to keep
        # 'PBA.byteArrayFromList' unambiguous.
        lines.append(f"{name} = PBA.byteArrayFromList ([] :: [Word32])")
        return "\n".join(lines)
    lines.append(f"{name} = PBA.byteArrayFromList")
    rendered = []
    for i, v in enumerate(items):
        if i == 0:
            rendered.append(f"({cp_hex(v)} :: Word32)")
        else:
            rendered.append(cp_hex(v))
    body = "\n    , ".join(rendered)
    lines.append(f"    [ {body}")
    lines.append("    ]")
    return "\n".join(lines)


HEADER = """\
-- |
-- Module      : Text.IDNA2008.Internal.Emoji.Data
-- Description : Compact codepoint range table for the Unicode
--               @Emoji@ property.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- AUTOMATICALLY GENERATED.  Do not edit by hand.  Regenerate with:
--
--   python3 internal/tools/genIdnaEmoji.py <unicode-version> \\\\
--           <emoji-data.txt>
--
-- Source (Unicode 12.0): https://www.unicode.org/Public/emoji/12.0/emoji-data.txt
-- Source (Unicode 13.0+): https://www.unicode.org/Public/{version}/ucd/emoji/emoji-data.txt
-- Aligned with: Unicode {version}
-- Emoji ranges: {n}
--
-- Codepoints with Unicode property @Emoji=Yes@.  Used by the
-- @ALLOWEMOJI@ option in "Text.IDNA2008.Internal.Parse" to relax
-- IDNA2008's blanket @DISALLOWED@ disposition for symbol\\/pictograph
-- codepoints.  Encoded as alternating start\\/end @Word32@ entries
-- (sorted by start, with abutting ranges coalesced).
module Text.IDNA2008.Internal.Emoji.Data
    ( emojiRangeCount
    , emojiRanges
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
    path = sys.argv[2]

    pairs = coalesce_pairs(parse_emoji(path))

    flat: list[int] = []
    for s, e in pairs:
        flat.append(s)
        flat.append(e)

    out = sys.stdout
    out.write(HEADER.format(version=version, n=len(pairs)))

    out.write("-- | Number of (start, end) range pairs in 'emojiRanges'.\n")
    out.write("emojiRangeCount :: Int\n")
    out.write(f"emojiRangeCount = {len(pairs)}\n\n")

    out.write("-- | Codepoints with Unicode @Emoji=Yes@, alternating\n")
    out.write("-- start\\/end.\n")
    out.write(emit_word32_list("emojiRanges", flat))
    out.write("\n")


if __name__ == "__main__":
    main()
