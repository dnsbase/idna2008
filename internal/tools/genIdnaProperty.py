#!/usr/bin/env python3
"""
genIdnaProperty.py -- regenerate Text.IDNA2008.Internal.Property.Data
from an IANA IDNA properties CSV.

The IANA registry publishes per-Unicode-version derived-property tables
at:

    https://www.iana.org/assignments/idna-tables-properties/

Concrete CSV download URLs follow the pattern:

    https://www.iana.org/assignments/idna-tables-X.Y.Z/idna-tables-properties.csv

For example (Unicode 12.0.0, the most recent IANA-curated version as of
2024-04-26):

    https://www.iana.org/assignments/idna-tables-12.0.0/idna-tables-properties.csv

Usage:

    python3 internal/tools/genIdnaProperty.py <unicode-version> <csv-path> \\
        > internal/Text/IDNA2008/Internal/Property/Data.hs

The first argument is recorded in the generated module's header for
provenance.  The second is the local path to the IANA CSV.

Each row of the CSV gives a codepoint or codepoint range and one of
five disposition values:

    PVALID, CONTEXTJ, CONTEXTO, DISALLOWED, UNASSIGNED

The output module exposes two parallel arrays:

    rangeStarts :: ByteArray of Word32 -- sorted ascending
    rangeTags   :: ByteArray of Word8  -- one tag per range

with disposition tags 0 = PVALID, 1 = CONTEXTJ, 2 = CONTEXTO,
3 = DISALLOWED, 4 = UNASSIGNED.  Each range covers codepoints
[rangeStarts[i], rangeStarts[i+1] - 1] (or 0x10FFFF for the last
range).
"""
import csv
import sys

DISP_MAP = {
    "PVALID":     0,
    "CONTEXTJ":   1,
    "CONTEXTO":   2,
    "DISALLOWED": 3,
    "UNASSIGNED": 4,
}

STARTS_PER_LINE = 8
TAGS_PER_LINE   = 32


def parse_csv(path):
    starts = []
    tags = []
    with open(path, newline="") as f:
        reader = csv.DictReader(f)
        for row in reader:
            cp = row["Codepoint"]
            prop = row["Property"]
            start = int(cp.split("-")[0], 16)
            try:
                tag = DISP_MAP[prop]
            except KeyError:
                raise SystemExit(f"unknown disposition {prop!r} in {path}")
            starts.append(start)
            tags.append(tag)
    return starts, tags


def emit_module(version, starts, tags, out=sys.stdout):
    n = len(starts)
    p = out.write

    p("-- |\n")
    p("-- Module      : Text.IDNA2008.Internal.Property.Data\n")
    p("-- Description : Codegen'd IDNA2008 codepoint disposition table\n")
    p("-- Copyright   : (c) Viktor Dukhovni, 2026\n")
    p("-- License     : BSD-3-Clause\n")
    p("--\n")
    p("-- Maintainer  : ietf-dane@dukhovni.org\n")
    p("-- Stability   : unstable\n")
    p("--\n")
    p("-- AUTOMATICALLY GENERATED FROM idna-tables-properties.csv.\n")
    p("-- Do not edit by hand.  Regenerate with:\n")
    p("--\n")
    p("--   python3 internal/tools/genIdnaProperty.py <unicode-version> <csv-path>\n")
    p("--\n")
    p("-- Source: https://www.iana.org/assignments/idna-tables-properties/\n")
    p(f"-- Aligned with: Unicode {version}\n")
    p(f"-- Range count: {n}\n")
    p("--\n")
    p("-- Each disposition tag is a small Word8:\n")
    p("--\n")
    p("--   * 0 -- PVALID\n")
    p("--   * 1 -- CONTEXTJ\n")
    p("--   * 2 -- CONTEXTO\n")
    p("--   * 3 -- DISALLOWED\n")
    p("--   * 4 -- UNASSIGNED\n")
    p("module Text.IDNA2008.Internal.Property.Data\n")
    p("    ( rangeCount\n")
    p("    , rangeStarts\n")
    p("    , rangeTags\n")
    p("    ) where\n\n")

    p("import Data.Array.Byte (ByteArray)\n")
    p("import Data.Word (Word8, Word32)\n")
    p("import qualified Data.Primitive.ByteArray as PBA\n\n")

    p("rangeCount :: Int\n")
    p(f"rangeCount = {n}\n\n")

    p("rangeStarts :: ByteArray\n")
    p("rangeStarts = PBA.byteArrayFromList\n")
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

    p("rangeTags :: ByteArray\n")
    p("rangeTags = PBA.byteArrayFromList @Word8\n")
    for i in range(0, n, TAGS_PER_LINE):
        chunk = tags[i:i + TAGS_PER_LINE]
        body = ", ".join(str(t) for t in chunk)
        if i == 0:
            p(f"    [ {body}\n")
        else:
            p(f"    , {body}\n")
    p("    ]\n")


def main():
    if len(sys.argv) != 3:
        sys.exit(
            "usage: genIdnaProperty.py <unicode-version> <csv-path>\n"
            "\n"
            "Fetch the CSV from\n"
            "  https://www.iana.org/assignments/idna-tables-X.Y.Z/idna-tables-properties.csv\n"
            "and pass the local path as the second argument."
        )
    version, path = sys.argv[1], sys.argv[2]
    starts, tags = parse_csv(path)
    if not starts:
        sys.exit(f"{path}: no entries parsed")
    emit_module(version, starts, tags)


if __name__ == "__main__":
    main()
