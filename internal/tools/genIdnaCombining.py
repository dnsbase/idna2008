#!/usr/bin/env python3
"""
genIdnaCombining.py -- regenerate Text.IDNA2008.Internal.Combining.Data
from the Unicode UCD's UnicodeData.txt.

A combining mark is a codepoint whose General_Category is one of
Mn (Nonspacing_Mark), Mc (Spacing_Mark), or Me (Enclosing_Mark).
RFC 5891 section 4.2.3.2 forbids a U-label from starting with one;
RFC 5892's disposition table classifies many of those codepoints
(Mn/Mc) as PVALID, so the per-codepoint validator alone doesn't
catch the rule.  The table this script emits is the dedicated
lookup the validator consults.

Usage:

    python3 internal/tools/genIdnaCombining.py <unicode-version> <UnicodeData.txt> \\
        > internal/Text/IDNA2008/Internal/Combining/Data.hs

The output module exposes two parallel arrays:

    combiningRangeStarts :: ByteArray of Word32  -- sorted ascending
    combiningRangeTags   :: ByteArray of Word8   -- one tag per range

Each range covers codepoints @[combiningRangeStarts[i],
combiningRangeStarts[i+1] - 1]@, with the final range running through
@0x10FFFF@.  Tag 0 means \"not a combining mark\", tag 1 means
\"combining mark\".  Codepoints not explicitly assigned in
UnicodeData.txt -- the gaps in the file -- are treated as
non-combining (tag 0); none of the reserved/unassigned blocks
carry a combining gc by default.
"""
from __future__ import annotations

import re
import sys

TAG_NON = 0
TAG_CM  = 1

COMBINING_GC = {"Mn", "Mc", "Me"}

CP_MAX = 0x10FFFF
ARRAY_LEN = CP_MAX + 1

STARTS_PER_LINE = 8
TAGS_PER_LINE   = 32

# Matches an ordinary UnicodeData.txt assignment line.  The two
# fields we read are columns 1 (codepoint, hex) and 3 (gc).
ASSIGN_RE = re.compile(
    r"^\s*([0-9A-Fa-f]+)\s*;[^;]*;([A-Za-z]+)\s*;")

# UnicodeData.txt encodes large CJK / surrogate / PUA blocks as a
# pair of synthetic <First> / <Last> rows that share a gc.  We
# detect the pair and expand the range.
FIRST_RE = re.compile(r"<.*,\s*First>")
LAST_RE  = re.compile(r"<.*,\s*Last>")


def parse(path: str) -> tuple[list[int], list[int]]:
    """Walk UnicodeData.txt, mark every codepoint whose gc is in
    @{Mn, Mc, Me}@, then coalesce equal-tag runs into ranges."""
    classes = [TAG_NON] * ARRAY_LEN
    pending_first: tuple[int, str] | None = None

    with open(path, encoding="utf-8") as f:
        for line_no, raw in enumerate(f, start=1):
            line = raw.rstrip("\n")
            if not line:
                continue
            fields = line.split(";")
            if len(fields) < 4:
                raise SystemExit(
                    f"{path}:{line_no}: too few fields: {raw!r}")
            cp = int(fields[0], 16)
            name = fields[1]
            gc   = fields[2]
            if cp < 0 or cp > CP_MAX:
                raise SystemExit(
                    f"{path}:{line_no}: codepoint U+{cp:04X} out of range")

            if FIRST_RE.search(name):
                if pending_first is not None:
                    raise SystemExit(
                        f"{path}:{line_no}: nested <First> entry")
                pending_first = (cp, gc)
                continue
            if LAST_RE.search(name):
                if pending_first is None:
                    raise SystemExit(
                        f"{path}:{line_no}: <Last> without <First>")
                lo, lo_gc = pending_first
                if lo_gc != gc:
                    raise SystemExit(
                        f"{path}:{line_no}: <First>/<Last> gc mismatch "
                        f"({lo_gc!r} vs {gc!r})")
                pending_first = None
                if gc in COMBINING_GC:
                    for c in range(lo, cp + 1):
                        classes[c] = TAG_CM
                continue

            if pending_first is not None:
                raise SystemExit(
                    f"{path}:{line_no}: <First> not followed by <Last>")

            if gc in COMBINING_GC:
                classes[cp] = TAG_CM

    if pending_first is not None:
        raise SystemExit(f"{path}: dangling <First> at end of file")

    # Coalesce equal-tag runs over the full codepoint space.
    starts: list[int] = []
    tags: list[int] = []
    last = -1
    for cp in range(ARRAY_LEN):
        c = classes[cp]
        if c != last:
            starts.append(cp)
            tags.append(c)
            last = c
    return starts, tags


def emit_starts(name: str, starts: list[int], out) -> None:
    n = len(starts)
    p = out.write
    p(f"{name} :: ByteArray\n")
    p(f"{name} = PBA.byteArrayFromList\n")
    for i in range(0, n, STARTS_PER_LINE):
        chunk = starts[i:i + STARTS_PER_LINE]
        if i == 0:
            head = f"(0x{chunk[0]:06X} :: Word32)"
            tail = ", ".join(f"0x{s:06X}" for s in chunk[1:])
            body = head + ((", " + tail) if tail else "")
            p(f"    [ {body}\n")
        else:
            body = ", ".join(f"0x{s:06X}" for s in chunk)
            p(f"    , {body}\n")
    p("    ]\n\n")


def emit_tags(name: str, tags: list[int], out) -> None:
    n = len(tags)
    p = out.write
    p(f"{name} :: ByteArray\n")
    p(f"{name} = PBA.byteArrayFromList @Word8\n")
    for i in range(0, n, TAGS_PER_LINE):
        chunk = tags[i:i + TAGS_PER_LINE]
        body = ", ".join(str(t) for t in chunk)
        if i == 0:
            p(f"    [ {body}\n")
        else:
            p(f"    , {body}\n")
    p("    ]\n")


HEADER = """\
-- |
-- Module      : Text.IDNA2008.Internal.Combining.Data
-- Description : Codegen'd combining-mark table (gc in @{{Mn, Mc, Me}}@)
--               for the RFC 5891 section 4.2.3.2 leading-mark check.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- AUTOMATICALLY GENERATED FROM UnicodeData.txt.  Do not edit by
-- hand.  Regenerate with:
--
--   python3 internal/tools/genIdnaCombining.py <unicode-version> \\\\
--           <UnicodeData.txt>
--
-- Source: https://www.unicode.org/Public/{version}/ucd/UnicodeData.txt
-- Aligned with: Unicode {version}
-- Range count: {n}
--
-- The table covers every Unicode codepoint in @[0, 0x10FFFF]@ with no
-- gaps: each entry says \"from this start until the next entry's start
-- (or 0x10FFFF for the last), the resolved combining-mark status is
-- /T/\".  Tag 0 means the codepoint is /not/ a combining mark; tag 1
-- means it is (gc in @{{Mn, Mc, Me}}@).  Codepoints absent from
-- UnicodeData.txt are treated as non-combining.
module Text.IDNA2008.Internal.Combining.Data
    ( combiningRangeCount
    , combiningRangeStarts
    , combiningRangeTags
    ) where

import Data.Array.Byte (ByteArray)
import Data.Word (Word8, Word32)
import qualified Data.Primitive.ByteArray as PBA

"""


def main() -> None:
    if len(sys.argv) != 3:
        sys.stderr.write(__doc__)
        sys.exit(1)
    version, path = sys.argv[1], sys.argv[2]
    starts, tags = parse(path)
    if not starts:
        sys.exit(f"{path}: no entries parsed")

    out = sys.stdout
    out.write(HEADER.format(version=version, n=len(starts)))

    out.write("-- | Number of (start, tag) entries.\n")
    out.write("combiningRangeCount :: Int\n")
    out.write(f"combiningRangeCount = {len(starts)}\n\n")

    out.write("-- | Range starts; sorted ascending.\n")
    emit_starts("combiningRangeStarts", starts, out)

    out.write("-- | Combining-mark tags; parallel to\n")
    out.write("-- 'combiningRangeStarts'.\n")
    out.write("--\n")
    out.write("--   * 0 -- not a combining mark\n")
    out.write("--   * 1 -- combining mark (gc in @{Mn, Mc, Me}@)\n")
    emit_tags("combiningRangeTags", tags, out)


if __name__ == "__main__":
    main()
