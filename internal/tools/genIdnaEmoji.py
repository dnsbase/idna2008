#!/usr/bin/env python3
"""
Generate Text.IDNA2008.Internal.Emoji.Data from the Unicode UCD
emoji-data.txt extracted property file and the UTS #46
IdnaMappingTable.txt.

Usage:
    python3 internal/tools/genIdnaEmoji.py <unicode-version> \\
            <emoji-data.txt> <IdnaMappingTable.txt>

Reads two inputs:

  1. The UCD emoji-data.txt property file.  Only entries with property
     name "Emoji" are kept; the related properties
     (Emoji_Presentation, Emoji_Modifier, Emoji_Modifier_Base,
     Emoji_Component, Extended_Pictographic) are ignored.  A codepoint
     is "an emoji" for IDNA-relaxation purposes iff Emoji=Yes.

  2. The UTS #46 IdnaMappingTable.txt.  We extract codepoints with
     status @mapped@ and intersect with the Emoji=Yes set above to
     produce the "mapped-emoji" subset.  These codepoints are
     Emoji=Yes but UTS #46 also folds them to a different codepoint
     sequence -- typically a CJK ideograph or an ASCII letter -- and
     so resolve ambiguously across the ecosystem: browsers (Safari)
     apply the fold and reach the target; tools like libidn2 in
     non-transitional mode reject them; this library admits them
     as-is under EMOJIOK.  Because the mapped form and the admit-as-is
     form may resolve to separately registered domains under different
     operators (see U+1F238 (1F238).ws  vs.  U+7533 (7533).ws  for
     a real-world case), EMOJIOK in this library specifically
     EXCLUDES this subset: it admits Emoji=Yes codepoints that are
     not UTS #46-mapped, and rejects (as DisallowedCodepoint) those
     that are.

Source files:

    Unicode 12.0  emoji: https://www.unicode.org/Public/emoji/12.0/emoji-data.txt
    Unicode 13.0+ emoji: https://www.unicode.org/Public/<ver>/ucd/emoji/emoji-data.txt
    Unicode 17.0+ uts46: https://www.unicode.org/Public/<ver>/idna/IdnaMappingTable.txt
    Unicode <=16  uts46: https://www.unicode.org/Public/idna/<ver>/IdnaMappingTable.txt
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


def parse_uts46_mapped(path: str) -> set[int]:
    """Return the set of codepoints whose UTS #46 status is exactly
    `mapped` in `IdnaMappingTable.txt`.  Other statuses (valid,
    disallowed, ignored, deviation) are not returned."""
    out: set[int] = set()
    with open(path, encoding="utf-8") as f:
        for raw in f:
            line = raw.split("#", 1)[0].strip()
            if not line:
                continue
            parts = [p.strip() for p in line.split(";")]
            if len(parts) < 2 or parts[1] != "mapped":
                continue
            rng = parts[0]
            if ".." in rng:
                a, b = rng.split("..")
                cps = range(int(a, 16), int(b, 16) + 1)
            else:
                cps = [int(rng, 16)]
            out.update(cps)
    return out


def mapped_emoji_pairs(
    emoji_pairs: list[tuple[int, int]],
    mapped_cps: set[int],
) -> list[tuple[int, int]]:
    """Intersect the Emoji=Yes range list with the mapped-codepoint
    set, returning a coalesced (start, end) list of codepoints in
    both."""
    matches: list[int] = sorted(
        cp for s, e in emoji_pairs for cp in range(s, e + 1) if cp in mapped_cps
    )
    return coalesce_pairs([(cp, cp) for cp in matches])


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
-- Description : Compact codepoint range tables for the Unicode
--               @Emoji@ property and its UTS #46-mapped subset.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- AUTOMATICALLY GENERATED.  Do not edit by hand.  Regenerate with:
--
--   python3 internal/tools/genIdnaEmoji.py <unicode-version> \\\\
--           <emoji-data.txt> <IdnaMappingTable.txt>
--
-- Source (Unicode 12.0): https://www.unicode.org/Public/emoji/12.0/emoji-data.txt
-- Source (Unicode 13.0+): https://www.unicode.org/Public/{version}/ucd/emoji/emoji-data.txt
-- Source (Unicode 17.0+, idna): https://www.unicode.org/Public/{version}/idna/IdnaMappingTable.txt
-- Aligned with: Unicode {version}
-- Emoji ranges: {n_emoji}
-- Mapped-emoji ranges: {n_mapped}
--
-- Two range tables, both with the same @Word32@ start\\/end layout
-- (sorted by start, abutting ranges coalesced):
--
--   * 'emojiRanges' -- codepoints with Unicode property @Emoji=Yes@.
--
--   * 'mappedEmojiRanges' -- the subset of 'emojiRanges' whose
--     UTS #46 status is @mapped@; cross-tool ambiguous.
--
-- Both tables feed the @EMOJIOK@ relaxation in
-- "Text.IDNA2008.Internal.Parse"; see the @EMOJIOK@ pattern in
-- "Text.IDNA2008.Internal.Flags" for the policy rationale.
module Text.IDNA2008.Internal.Emoji.Data
    ( emojiRangeCount
    , emojiRanges
    , mappedEmojiRangeCount
    , mappedEmojiRanges
    ) where

import Data.Array.Byte (ByteArray)
import Data.Word (Word32)
import qualified Data.Primitive.ByteArray as PBA

"""


def main() -> None:
    if len(sys.argv) != 4:
        sys.stderr.write(__doc__)
        sys.exit(1)

    version = sys.argv[1]
    emoji_path = sys.argv[2]
    uts46_path = sys.argv[3]

    emoji_pairs = coalesce_pairs(parse_emoji(emoji_path))
    mapped_cps  = parse_uts46_mapped(uts46_path)
    mapped_pairs = mapped_emoji_pairs(emoji_pairs, mapped_cps)

    flat: list[int] = []
    for s, e in emoji_pairs:
        flat.append(s); flat.append(e)
    mapped_flat: list[int] = []
    for s, e in mapped_pairs:
        mapped_flat.append(s); mapped_flat.append(e)

    out = sys.stdout
    out.write(HEADER.format(version=version, n_emoji=len(emoji_pairs),
                            n_mapped=len(mapped_pairs)))

    out.write("-- | Number of (start, end) range pairs in 'emojiRanges'.\n")
    out.write("emojiRangeCount :: Int\n")
    out.write(f"emojiRangeCount = {len(emoji_pairs)}\n\n")

    out.write("-- | Codepoints with Unicode @Emoji=Yes@, alternating\n")
    out.write("-- start\\/end.\n")
    out.write(emit_word32_list("emojiRanges", flat))
    out.write("\n\n")

    out.write("-- | Number of (start, end) range pairs in\n")
    out.write("-- 'mappedEmojiRanges'.\n")
    out.write("mappedEmojiRangeCount :: Int\n")
    out.write(f"mappedEmojiRangeCount = {len(mapped_pairs)}\n\n")

    out.write("-- | Subset of 'emojiRanges' whose UTS #46 status is\n")
    out.write("-- @mapped@, alternating start\\/end.  Excluded from\n")
    out.write("-- the @EMOJIOK@ relaxation; see module haddock for\n")
    out.write("-- the rationale.\n")
    out.write(emit_word32_list("mappedEmojiRanges", mapped_flat))
    out.write("\n")


if __name__ == "__main__":
    main()
