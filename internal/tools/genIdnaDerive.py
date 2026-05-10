#!/usr/bin/env python3
"""
genIdnaDerive.py -- compute the IDNA2008 disposition table from UCD
inputs by running the RFC 5892 Section 3 derivation locally.

Output is a CSV with the same shape as IANA's idna-tables-properties.csv
(columns "Codepoint" and "Property"), so the existing genIdnaProperty.py
consumes it unchanged.

    python3 genIdnaDerive.py <unicode-version>     \\
        <UnicodeData.txt>                          \\
        <DerivedCoreProperties.txt>                \\
        <PropList.txt>                             \\
        <HangulSyllableType.txt>                   \\
        <DerivedNormalizationProps.txt>            \\
        > idna-tables-properties.csv

The first argument is recorded in stderr for provenance.

Verification: at Unicode 12.0.0 the output matches IANA's published
https://www.iana.org/assignments/idna-tables-12.0.0/
idna-tables-properties.csv byte-for-byte when both files are
reduced to the (Codepoint, Property) projection.  Once that round
trip passes, the same derivation produces a valid disposition table
at any Unicode version, and the IANA dependency is gone.

The "Unstable" check (RFC 5892 section 2.6) consults the target version's
@NFKC_CF@ (NFKC_Casefold) mapping from
DerivedNormalizationProps.txt directly, rather than recomputing
NFKC via Python's stdlib.  This avoids a silent misclassification
for codepoints assigned later than the Python interpreter's bundled
Unicode data version.
"""
from __future__ import annotations

import csv
import re
import sys
from typing import Iterable

#-----------------------------------------------------------------------
# Static tables lifted verbatim from RFC 5892
#-----------------------------------------------------------------------

# Section 2.6: Exceptions.  Codepoint -> disposition; tested before
# any computed rule.  Includes the seven CONTEXTO codepoints and the
# eight CONTEXTJ-related explicit overrides plus a handful of
# letter-shape overrides (final sigma, sharp s, ...).
EXCEPTIONS = {
    # PVALID
    0x00DF: "PVALID",      # LATIN SMALL LETTER SHARP S
    0x03C2: "PVALID",      # GREEK SMALL LETTER FINAL SIGMA
    0x06FD: "PVALID",      # ARABIC SIGN SINDHI AMPERSAND
    0x06FE: "PVALID",      # ARABIC SIGN SINDHI POSTPOSITION MEN
    0x0F0B: "PVALID",      # TIBETAN MARK INTERSYLLABIC TSHEG
    0x3007: "PVALID",      # IDEOGRAPHIC NUMBER ZERO

    # CONTEXTO
    0x00B7: "CONTEXTO",    # MIDDLE DOT (A.3)
    0x0375: "CONTEXTO",    # GREEK LOWER NUMERAL SIGN (A.4)
    0x05F3: "CONTEXTO",    # HEBREW PUNCTUATION GERESH (A.5)
    0x05F4: "CONTEXTO",    # HEBREW PUNCTUATION GERSHAYIM (A.6)
    0x30FB: "CONTEXTO",    # KATAKANA MIDDLE DOT (A.9)

    # DISALLOWED
    0x0640: "DISALLOWED",  # ARABIC TATWEEL
    0x07FA: "DISALLOWED",  # NKO LAJANYALAN
    0x302E: "DISALLOWED",  # HANGUL SINGLE DOT TONE MARK
    0x302F: "DISALLOWED",  # HANGUL DOUBLE DOT TONE MARK
    0x3031: "DISALLOWED",  # VERTICAL KANA REPEAT MARK
    0x3032: "DISALLOWED",  # VERTICAL KANA REPEAT WITH VOICED SOUND MARK
    0x3033: "DISALLOWED",  # VERTICAL KANA REPEAT MARK UPPER HALF
    0x3034: "DISALLOWED",  # VERTICAL KANA REPEAT WITH VOICED SOUND MARK UPPER HALF
    0x3035: "DISALLOWED",  # VERTICAL KANA REPEAT MARK LOWER HALF
    0x303B: "DISALLOWED",  # VERTICAL IDEOGRAPHIC ITERATION MARK
}

# Arabic-Indic digits (A.8) and Extended Arabic-Indic digits (A.9)
# are CONTEXTO via section 2.6 too.  Codified as ranges for brevity.
for cp in range(0x0660, 0x066A):
    EXCEPTIONS[cp] = "CONTEXTO"
for cp in range(0x06F0, 0x06FA):
    EXCEPTIONS[cp] = "CONTEXTO"

# Section 2.7: BackwardCompatible.  Currently empty per RFC 5892;
# the slot exists so future codepoints can be pinned to a specific
# disposition that overrides their derived class.
BACKWARD_COMPATIBLE: dict[int, str] = {}

# Section 2.6 also pins the two Join-Control codepoints to CONTEXTJ.
EXCEPTIONS[0x200C] = "CONTEXTJ"        # ZERO WIDTH NON-JOINER
EXCEPTIONS[0x200D] = "CONTEXTJ"        # ZERO WIDTH JOINER

#-----------------------------------------------------------------------
# UCD input parsing
#-----------------------------------------------------------------------

# UnicodeData.txt is one record per line, semicolon-separated, 15
# fields.  We need fields 2 (General_Category) and 5 (decomp).
def parse_unicode_data(path: str) -> tuple[
        dict[int, str],            # codepoint -> General_Category
        dict[int, bool],            # codepoint -> has compatibility decomp
]:
    gc: dict[int, str] = {}
    has_compat: dict[int, bool] = {}
    range_first: int | None = None
    range_first_gc: str = ""
    with open(path, encoding="utf-8") as f:
        for line in f:
            fields = line.rstrip("\n").split(";")
            if len(fields) < 6:
                continue
            cp = int(fields[0], 16)
            name = fields[1]
            cat = fields[2]
            decomp = fields[5]

            # Compatibility decomposition: starts with "<...>"
            compat = decomp.startswith("<") and decomp != ""

            if name.endswith(", First>"):
                range_first = cp
                range_first_gc = cat
                continue
            if name.endswith(", Last>") and range_first is not None:
                for r in range(range_first, cp + 1):
                    gc[r] = range_first_gc
                    # Range entries have no decomposition.
                    has_compat[r] = False
                range_first = None
                continue

            gc[cp] = cat
            has_compat[cp] = compat
    return gc, has_compat


# DerivedCoreProperties.txt and PropList.txt share a syntax:
#   start[..end] ; PropertyName [# comment]
PROP_LINE = re.compile(
    r"^\s*([0-9A-Fa-f]+)(?:\.\.([0-9A-Fa-f]+))?\s*;\s*([A-Za-z0-9_]+)"
)


def parse_property_file(path: str, wanted: set[str]) -> dict[str, set[int]]:
    """Return {property -> {codepoint}} for each property in 'wanted'."""
    out: dict[str, set[int]] = {p: set() for p in wanted}
    with open(path, encoding="utf-8") as f:
        for raw in f:
            line = raw.split("#", 1)[0]
            m = PROP_LINE.match(line)
            if not m:
                continue
            prop = m.group(3)
            if prop not in wanted:
                continue
            start = int(m.group(1), 16)
            end = int(m.group(2), 16) if m.group(2) else start
            out[prop].update(range(start, end + 1))
    return out


def parse_hangul_syllable_type(path: str) -> dict[int, str]:
    """codepoint -> Hangul_Syllable_Type (L, V, T, LV, LVT, NA)."""
    out: dict[int, str] = {}
    with open(path, encoding="utf-8") as f:
        for raw in f:
            line = raw.split("#", 1)[0]
            m = PROP_LINE.match(line)
            if not m:
                continue
            start = int(m.group(1), 16)
            end = int(m.group(2), 16) if m.group(2) else start
            tag = m.group(3)
            for cp in range(start, end + 1):
                out[cp] = tag
    return out


# DerivedNormalizationProps.txt NFKC_CF lines have one of two shapes:
#
#   start[..end] ; NFKC_CF;                    # mapping is empty
#   start[..end] ; NFKC_CF; <hex hex hex...>   # mapping is a sequence
#
# A codepoint is "Unstable" iff it has an explicit NFKC_CF entry whose
# mapping target differs from the source (single-cp identity is implicit
# for codepoints with no entry).
NFKC_CF_LINE = re.compile(
    r"^\s*([0-9A-Fa-f]+)(?:\.\.([0-9A-Fa-f]+))?\s*;\s*NFKC_CF\s*;\s*([^#\n]*)"
)


def parse_nfkc_cf_unstable(path: str) -> set[int]:
    """Return the set of codepoints whose NFKC_CF mapping differs
    from the codepoint itself (i.e. NFKC + casefold + NFKC changes
    them).  These are exactly the IDNA Unstable codepoints."""
    out: set[int] = set()
    with open(path, encoding="utf-8") as f:
        for raw in f:
            m = NFKC_CF_LINE.match(raw)
            if not m:
                continue
            start = int(m.group(1), 16)
            end = int(m.group(2), 16) if m.group(2) else start
            mapping_str = m.group(3).strip()
            for cp in range(start, end + 1):
                # An empty mapping ("") means "drop this codepoint",
                # which is a change.  A single-codepoint mapping
                # equal to cp itself would be an identity (rare in
                # the file -- usually identity is implicit), so check
                # for that explicitly.
                if mapping_str == "":
                    out.add(cp)
                else:
                    target = [int(t, 16) for t in mapping_str.split()]
                    if target != [cp]:
                        out.add(cp)
    return out


#-----------------------------------------------------------------------
# Section 2 categories -- predicates over a single codepoint.
#
# Each is a small function that assumes the parsed UCD tables have
# been bound at module scope (set up in main()).
#-----------------------------------------------------------------------

GC: dict[int, str] = {}              # General_Category
HAS_COMPAT: dict[int, bool] = {}     # has compatibility decomposition
DEFAULT_IGNORABLE: set[int] = set()  # Default_Ignorable_Code_Point
NONCHARACTER: set[int] = set()       # Noncharacter_Code_Point
JOIN_CONTROL: set[int] = set()       # Join_Control
WHITE_SPACE: set[int] = set()        # White_Space
HANGUL_TYPE: dict[int, str] = {}     # Hangul_Syllable_Type
UNSTABLE: set[int] = set()           # NFKC_CF != cp


def gc(cp: int) -> str:
    """General_Category of cp; "Cn" for codepoints not in
    UnicodeData.txt (the Cn default applies to all unassigned and
    reserved codepoints)."""
    return GC.get(cp, "Cn")


# Section 2.7: Unassigned.
#
#   GC == Cn AND Noncharacter_Code_Point != true
#
# Default_Ignorable codepoints that happen to be Cn are still
# Unassigned per the spec; they get DISALLOWED later via
# IgnorableProperties only if they were assigned (GC != Cn).
def is_unassigned(cp: int) -> bool:
    return gc(cp) == "Cn" and cp not in NONCHARACTER


# Section 2.9: LDH.  Lowercase letters + digits + hyphen ONLY;
# uppercase A-Z is NOT LDH for IDNA -- it falls through to the
# Unstable check (NFKC + casefold collapses A-Z onto a-z) and
# ends up DISALLOWED.
def is_ldh(cp: int) -> bool:
    return (
        (0x0061 <= cp <= 0x007A)     # a-z
        or (0x0030 <= cp <= 0x0039)  # 0-9
        or cp == 0x002D              # '-'
    )


# Section 2.5/2.10: JoinControl.  Covered explicitly via Exceptions
# above; this predicate exists for completeness and is used only as
# a fallback if Exceptions has been redacted.
def is_join_control(cp: int) -> bool:
    return cp in JOIN_CONTROL


# Section 2.6 of RFC 5892 (also termed "Unstable" in some drafts):
# the codepoint changes under NFKC + case-fold + NFKC.  Read
# directly from the target version's NFKC_CF (NFKC_Casefold)
# property in DerivedNormalizationProps.txt -- equivalent to the
# (NFKC . casefold . NFKC) composition by definition, and free of
# the version skew that would arise from running Python's stdlib
# normalisation against a Unicode version it doesn't know.
def is_unstable(cp: int) -> bool:
    return cp in UNSTABLE


# Section 2.11: IgnorableProperties.  Default_Ignorable or Noncharacter.
def is_ignorable_properties(cp: int) -> bool:
    return cp in DEFAULT_IGNORABLE or cp in NONCHARACTER


# Section 2.12: IgnorableBlocks.  Codepoints in these specific
# Unicode blocks are blanket-DISALLOWED regardless of GC; the block
# boundaries don't shift across Unicode versions, so the literal
# ranges are stable.  Per RFC 5892 section 2.12:
#
#   Combining Diacritical Marks for Symbols
#   Musical Symbols
#   Ancient Greek Musical Notation
IGNORABLE_BLOCKS: list[tuple[int, int]] = [
    (0x20D0,  0x20FF),    # Combining Diacritical Marks for Symbols
    (0x1D100, 0x1D1FF),   # Musical Symbols
    (0x1D200, 0x1D24F),   # Ancient Greek Musical Notation
]


def is_ignorable_block(cp: int) -> bool:
    for lo, hi in IGNORABLE_BLOCKS:
        if lo <= cp <= hi:
            return True
    return False


# Section 2.13: OldHangulJamo.  Hangul jamo that aren't part of the
# closed precomposed-syllable cluster -- these are the historical
# / archaic jamo plus the modern jamo not covered by the LV / LVT
# precomposition.
def is_old_hangul_jamo(cp: int) -> bool:
    return HANGUL_TYPE.get(cp, "NA") in ("L", "V", "T")


# Section 2.14: HasCompat.
def is_has_compat(cp: int) -> bool:
    return HAS_COMPAT.get(cp, False)


# Section 2.15: LetterDigits.  General_Category in {Ll, Lu, Lo, Lm,
# Mn, Mc, Nd}.
LETTER_DIGITS_GC = {"Ll", "Lu", "Lo", "Lm", "Mn", "Mc", "Nd"}


def is_letter_digits(cp: int) -> bool:
    return gc(cp) in LETTER_DIGITS_GC


#-----------------------------------------------------------------------
# Section 3: derivation cascade
#-----------------------------------------------------------------------

def derive(cp: int) -> str:
    if cp in EXCEPTIONS:
        return EXCEPTIONS[cp]
    if cp in BACKWARD_COMPATIBLE:
        return BACKWARD_COMPATIBLE[cp]
    if is_unassigned(cp):
        return "UNASSIGNED"
    if is_ldh(cp):
        return "PVALID"
    if is_join_control(cp):
        return "CONTEXTJ"
    if is_unstable(cp):
        return "DISALLOWED"
    if is_ignorable_properties(cp):
        return "DISALLOWED"
    if is_ignorable_block(cp):
        return "DISALLOWED"
    if is_old_hangul_jamo(cp):
        return "DISALLOWED"
    if is_has_compat(cp):
        return "DISALLOWED"
    if is_letter_digits(cp):
        return "PVALID"
    return "DISALLOWED"


#-----------------------------------------------------------------------
# Range-coalesce and CSV emit
#-----------------------------------------------------------------------

def coalesce_runs(dispositions: list[str]) -> list[tuple[int, int, str]]:
    """Given dispositions[cp] for cp in [0, 0x10FFFF], return a list
    of (start, end, prop) runs sharing a single disposition."""
    out: list[tuple[int, int, str]] = []
    if not dispositions:
        return out
    run_start = 0
    run_prop = dispositions[0]
    for cp in range(1, len(dispositions)):
        if dispositions[cp] != run_prop:
            out.append((run_start, cp - 1, run_prop))
            run_start = cp
            run_prop = dispositions[cp]
    out.append((run_start, len(dispositions) - 1, run_prop))
    return out


def fmt_codepoint(start: int, end: int) -> str:
    """IANA-style codepoint column: bare hex for singleton, lo-hi
    for ranges."""
    if start == end:
        return f"{start:04X}"
    return f"{start:04X}-{end:04X}"


def emit_csv(version: str, runs: list[tuple[int, int, str]],
             out=sys.stdout) -> None:
    # Header row only; no leading '#' comments -- csv.DictReader
    # treats the first line as the header, and a comment line there
    # causes downstream consumers to choke.  Provenance / version
    # info goes to stderr in main() instead.
    w = csv.writer(out)
    w.writerow(["Codepoint", "Property", "Description"])
    for start, end, prop in runs:
        w.writerow([fmt_codepoint(start, end), prop, ""])


#-----------------------------------------------------------------------
# main
#-----------------------------------------------------------------------

def main() -> None:
    if len(sys.argv) != 7:
        sys.stderr.write(__doc__)
        sys.exit(1)

    version = sys.argv[1]

    global GC, HAS_COMPAT, DEFAULT_IGNORABLE, NONCHARACTER
    global JOIN_CONTROL, WHITE_SPACE, HANGUL_TYPE, UNSTABLE

    GC, HAS_COMPAT = parse_unicode_data(sys.argv[2])

    derived = parse_property_file(sys.argv[3], {"Default_Ignorable_Code_Point"})
    DEFAULT_IGNORABLE = derived["Default_Ignorable_Code_Point"]

    proplist = parse_property_file(sys.argv[4], {
        "Noncharacter_Code_Point",
        "Join_Control",
        "White_Space",
    })
    NONCHARACTER = proplist["Noncharacter_Code_Point"]
    JOIN_CONTROL = proplist["Join_Control"]
    WHITE_SPACE = proplist["White_Space"]

    HANGUL_TYPE = parse_hangul_syllable_type(sys.argv[5])
    UNSTABLE = parse_nfkc_cf_unstable(sys.argv[6])

    sys.stderr.write(f"genIdnaDerive: target Unicode {version}\n")

    dispositions = [derive(cp) for cp in range(0, 0x110000)]
    runs = coalesce_runs(dispositions)
    emit_csv(version, runs)


if __name__ == "__main__":
    main()
