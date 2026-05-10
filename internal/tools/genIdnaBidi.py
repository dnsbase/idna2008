#!/usr/bin/env python3
"""
genIdnaBidi.py -- regenerate Text.IDNA2008.Internal.Bidi.Data
from the Unicode UCD's DerivedBidiClass.txt.

DerivedBidiClass.txt lists Bidi_Class assignments for every assigned
codepoint plus a small set of @missing fall-throughs that fill in the
class for unassigned codepoints in specific blocks (e.g. Hebrew, Arabic
private-use ranges) plus a global @missing line giving Left_To_Right as
the universal default.

Usage:

    python3 internal/tools/genIdnaBidi.py <unicode-version> <DerivedBidiClass.txt> \\
        > internal/Text/IDNA2008/Internal/Bidi/Data.hs

The output module exposes two parallel arrays:

    bidiRangeStarts :: ByteArray of Word32  -- sorted ascending
    bidiRangeTags   :: ByteArray of Word8   -- one tag per range

Each range covers codepoints @[bidiRangeStarts[i], bidiRangeStarts[i+1] - 1]@,
with the final range running through @0x10FFFF@.  The eleven Bidi classes
relevant to RFC 5893 (L, R, AL, AN, EN, ES, CS, ET, ON, BN, NSM) get
distinct tags 0..10; everything else (B, S, WS, LRE/RLE/PDF, LRO/RLO,
LRI/RLI/FSI/PDI) collapses into a single @Other@ tag (11), which the
parser treats as outside the rule set.
"""
from __future__ import annotations

import re
import sys
from typing import Iterable

# Eleven RFC 5893 classes, plus an "Other" bucket.
TAG_L     = 0
TAG_R     = 1
TAG_AL    = 2
TAG_AN    = 3
TAG_EN    = 4
TAG_ES    = 5
TAG_CS    = 6
TAG_ET    = 7
TAG_ON    = 8
TAG_BN    = 9
TAG_NSM   = 10
TAG_OTHER = 11

# Both abbreviations (used in regular lines) and full names (used in
# @missing lines) appear in DerivedBidiClass.txt.
CLASS_MAP = {
    # The eleven we care about.
    "L":   TAG_L,   "Left_To_Right":         TAG_L,
    "R":   TAG_R,   "Right_To_Left":         TAG_R,
    "AL":  TAG_AL,  "Arabic_Letter":         TAG_AL,
    "AN":  TAG_AN,  "Arabic_Number":         TAG_AN,
    "EN":  TAG_EN,  "European_Number":       TAG_EN,
    "ES":  TAG_ES,  "European_Separator":    TAG_ES,
    "CS":  TAG_CS,  "Common_Separator":      TAG_CS,
    "ET":  TAG_ET,  "European_Terminator":   TAG_ET,
    "ON":  TAG_ON,  "Other_Neutral":         TAG_ON,
    "BN":  TAG_BN,  "Boundary_Neutral":      TAG_BN,
    "NSM": TAG_NSM, "Nonspacing_Mark":       TAG_NSM,
    # Everything else collapses to Other.
    "B":   TAG_OTHER, "Paragraph_Separator":         TAG_OTHER,
    "S":   TAG_OTHER, "Segment_Separator":           TAG_OTHER,
    "WS":  TAG_OTHER, "White_Space":                 TAG_OTHER,
    "LRE": TAG_OTHER, "Left_To_Right_Embedding":     TAG_OTHER,
    "RLE": TAG_OTHER, "Right_To_Left_Embedding":     TAG_OTHER,
    "PDF": TAG_OTHER, "Pop_Directional_Format":      TAG_OTHER,
    "LRO": TAG_OTHER, "Left_To_Right_Override":      TAG_OTHER,
    "RLO": TAG_OTHER, "Right_To_Left_Override":      TAG_OTHER,
    "LRI": TAG_OTHER, "Left_To_Right_Isolate":       TAG_OTHER,
    "RLI": TAG_OTHER, "Right_To_Left_Isolate":       TAG_OTHER,
    "FSI": TAG_OTHER, "First_Strong_Isolate":        TAG_OTHER,
    "PDI": TAG_OTHER, "Pop_Directional_Isolate":     TAG_OTHER,
}

TAG_NAMES = {
    TAG_L:   "L",
    TAG_R:   "R",
    TAG_AL:  "AL",
    TAG_AN:  "AN",
    TAG_EN:  "EN",
    TAG_ES:  "ES",
    TAG_CS:  "CS",
    TAG_ET:  "ET",
    TAG_ON:  "ON",
    TAG_BN:  "BN",
    TAG_NSM: "NSM",
    TAG_OTHER: "Other",
}

CP_MAX = 0x10FFFF
ARRAY_LEN = CP_MAX + 1

# Patterns for lines we consume.
ASSIGN_RE  = re.compile(
    r"^\s*([0-9A-Fa-f]+)(?:\.\.([0-9A-Fa-f]+))?\s*;\s*([A-Za-z_]+)\b")
MISSING_RE = re.compile(
    r"^\s*#\s*@missing:\s*([0-9A-Fa-f]+)(?:\.\.([0-9A-Fa-f]+))?\s*;\s*([A-Za-z_]+)\b")

STARTS_PER_LINE = 8
TAGS_PER_LINE   = 32


def lookup_tag(name: str, where: str) -> int:
    try:
        return CLASS_MAP[name]
    except KeyError as exc:
        raise SystemExit(f"unknown Bidi class {name!r} in {where}") from exc


def parse(path: str) -> tuple[list[int], list[int]]:
    """Return (starts, tags) for the resolved class of every
    codepoint in @[0, CP_MAX]@.  Adjacent codepoints with the same
    class are coalesced into a single range.

    UAX #44 section 5.7.4 specifies that @missing fall-throughs
    apply only to codepoints that lack an explicit assignment, and
    where multiple @missing lines cover the same codepoint, the
    later one wins (the file lists the global default first,
    block-specific overrides after).  We therefore collect both
    kinds of line in source order and replay them in two phases:
    @missing entries first (later overwriting earlier), then
    explicit assignments (overwriting any @missing default).
    """
    UNSET = -1
    classes = [UNSET] * ARRAY_LEN
    missing: list[tuple[int, int, int]] = []
    assigns: list[tuple[int, int, int]] = []

    with open(path, encoding="utf-8") as f:
        for line_no, raw in enumerate(f, start=1):
            line = raw.rstrip("\n")

            mm = MISSING_RE.match(line)
            if mm:
                lo = int(mm.group(1), 16)
                hi = int(mm.group(2), 16) if mm.group(2) else lo
                tag = lookup_tag(mm.group(3),
                                 f"{path}:{line_no} (@missing)")
                if lo < 0 or hi > CP_MAX or lo > hi:
                    raise SystemExit(
                        f"{path}:{line_no}: @missing range "
                        f"{lo:X}..{hi:X} out of bounds")
                missing.append((lo, hi, tag))
                continue

            # Strip comments before testing for an assignment line.
            payload = line.split("#", 1)[0].strip()
            if not payload:
                continue
            am = ASSIGN_RE.match(payload)
            if not am:
                raise SystemExit(
                    f"{path}:{line_no}: cannot parse line: {raw!r}")
            lo = int(am.group(1), 16)
            hi = int(am.group(2), 16) if am.group(2) else lo
            tag = lookup_tag(am.group(3),
                             f"{path}:{line_no}")
            if lo < 0 or hi > CP_MAX or lo > hi:
                raise SystemExit(
                    f"{path}:{line_no}: range {lo:X}..{hi:X} out of bounds")
            assigns.append((lo, hi, tag))

    # Phase 1: @missing in source order.  Later overwrites earlier
    # for codepoints both ranges cover.
    for lo, hi, tag in missing:
        for cp in range(lo, hi + 1):
            classes[cp] = tag

    # Phase 2: explicit assignments, overwriting any @missing
    # default that landed on the same codepoint.
    for lo, hi, tag in assigns:
        for cp in range(lo, hi + 1):
            classes[cp] = tag

    # Sanity: full coverage required.  The global @missing line
    # (@missing: 0000..10FFFF; Left_To_Right) guarantees this, so
    # an UNSET cp here means the input file is malformed.
    for cp in range(ARRAY_LEN):
        if classes[cp] == UNSET:
            raise SystemExit(
                f"codepoint U+{cp:04X} has no Bidi_Class assignment "
                f"and no @missing fall-through")

    # Coalesce adjacent equal-class codepoints into ranges.
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
-- Module      : Text.IDNA2008.Internal.Bidi.Data
-- Description : Codegen'd Bidi_Class table for the per-label RFC 5893
--               check applied in "Text.IDNA2008.Internal.Parse".
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- AUTOMATICALLY GENERATED FROM DerivedBidiClass.txt.  Do not edit by
-- hand.  Regenerate with:
--
--   python3 internal/tools/genIdnaBidi.py <unicode-version> \\\\
--           <DerivedBidiClass.txt>
--
-- Source: https://www.unicode.org/Public/{version}/ucd/extracted/DerivedBidiClass.txt
-- Aligned with: Unicode {version}
-- Range count: {n}
--
-- The table covers every Unicode codepoint in @[0, 0x10FFFF]@ with no
-- gaps: each entry says \"from this start until the next entry's start
-- (or 0x10FFFF for the last), the resolved Bidi_Class is /T/\".  The
-- eleven classes relevant to RFC 5893 -- L, R, AL, AN, EN, ES, CS,
-- ET, ON, BN, NSM -- get distinct tags 0..10; every other class (B,
-- S, WS, the embedding\\/override\\/isolate format characters)
-- collapses to a single @Other@ tag (11), which the parser treats as
-- a Bidi-rule failure for any label in which it appears.
module Text.IDNA2008.Internal.Bidi.Data
    ( bidiRangeCount
    , bidiRangeStarts
    , bidiRangeTags
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

    out.write("-- | Number of (start, class) entries.\n")
    out.write("bidiRangeCount :: Int\n")
    out.write(f"bidiRangeCount = {len(starts)}\n\n")

    out.write("-- | Range starts; sorted ascending.\n")
    emit_starts("bidiRangeStarts", starts, out)

    out.write("-- | Class tags; parallel to 'bidiRangeStarts'.\n")
    out.write("--\n")
    out.write("--   * 0  -- L     -- Left_To_Right\n")
    out.write("--   * 1  -- R     -- Right_To_Left\n")
    out.write("--   * 2  -- AL    -- Arabic_Letter\n")
    out.write("--   * 3  -- AN    -- Arabic_Number\n")
    out.write("--   * 4  -- EN    -- European_Number\n")
    out.write("--   * 5  -- ES    -- European_Separator\n")
    out.write("--   * 6  -- CS    -- Common_Separator\n")
    out.write("--   * 7  -- ET    -- European_Terminator\n")
    out.write("--   * 8  -- ON    -- Other_Neutral\n")
    out.write("--   * 9  -- BN    -- Boundary_Neutral\n")
    out.write("--   * 10 -- NSM   -- Nonspacing_Mark\n")
    out.write("--   * 11 -- Other -- everything else (B, S, WS,\n")
    out.write("--                    embedding\\/override\\/isolate format\n")
    out.write("--                    characters); rejected by the\n")
    out.write("--                    per-label Bidi check.\n")
    emit_tags("bidiRangeTags", tags, out)


if __name__ == "__main__":
    main()
