# Conformance test vectors

This directory holds the conformance test vectors for the
`idna2008` library and the Haskell-side test driver that
consumes them.

The vectors are deliberately stored in a language-agnostic JSON
file (`vectors.json`) so future ports of the library to other
languages -- Rust, C++, Go, etc. -- can read the same file and
validate that they produce identical results.

## Files

| File | Contents |
| --- | --- |
| `vectors.json` | The test vectors. Source of truth. |
| `conformance.hs` | The Haskell test driver. Built as a `tasty` test-suite by `cabal test`. |
| `README.md` | This document. |

## Running

```
cabal test conformance
```

## Vector schema

A vector file is a JSON object:

```json
{
  "version": 1,
  "description": "...",
  "tests": [ ... ]
}
```

`version` is an integer; the current schema is `1`.  Each entry
in `tests` is an object describing one parser run:

```json
{
  "name": "short-unique-id",
  "input": "the domain name as the user types it",
  "classes": "host",
  "flags": "default",
  "expect": { ... }
}
```

### `input`

The presentation-form domain name.  A JSON string, so the
encoding is UTF-8.  Most non-ASCII content appears inline as raw
UTF-8 bytes for readability.  Three categories of content,
however, **must** be written using JSON `\uXXXX` escapes:

  * Combining marks and other non-NFC sequences.  Many editors
    and terminals silently NFC-normalise on save or on display,
    which would corrupt vectors whose whole point is to
    exercise non-NFC input.
  * Right-to-left codepoints (Hebrew, Arabic, etc.).  Editor and
    terminal Bidi reordering means the on-disk byte order is
    not what a reader sees, which makes inline RTL content
    unsafe to edit by hand.  Escaping it forces the source to
    read in logical (on-disk) order, the same order the parser
    sees.
  * Format characters (Unicode category `Cf`, e.g. ZWNJ
    U+200C and ZWJ U+200D) and control characters (`Cc`).
    These render invisibly or as substitution glyphs, so an
    inline occurrence is impossible to verify by eye.

The test driver sees the same Unicode characters either way.

### `classes`

A CLI-style token list selecting which `LabelForm` values the
parser should accept.  Optional; defaults to `"host"`
(letter-digit-hyphen plus reserved-LDH, A-labels, U-labels, and
fake A-labels -- the permissive set for hostname-shaped input).

Token names: `ldh`, `rldh`, `fakea`, `alabel`, `ulabel`,
`attrleaf`, `octet`, `wildlabel`, `laxulabel`.  Presets: `idn`
(LDH + ALABEL + ULABEL), `host` (the IDN preset plus RLDH and
FAKEA), `all` (every clean-path form -- the eight forms above
/excluding/ `laxulabel`).  Admitting `laxulabel` is always an
explicit opt-in (e.g. `"all,+laxulabel"`).

Comma-separated.  Tokens may be prefixed with `+` (additive,
the default) or `-` (subtractive).  See `parseLabelFormSet` in
`Text.IDNA2008.Internal.LabelFormSet` for the full grammar.

### `flags`

A CLI-style token list selecting parser flags.  Optional;
defaults to `"default"`, which is `ALABELCHECK` + `NFCCHECK`
+ `BIDICHECK`.

Token names: `alabel-check`, `nfc-check`, `emoji-ok`,
`map-dots`, `map-nfc`, `map-case`, `map-width`, `bidi-check`,
`ascii-fallback`, `map-uts46`.  Preset: `map` (the four
RFC 5895 mapping flags combined; does /not/ include
`map-uts46`).

Same syntax as `classes`.  See `parseIdnaFlags` in
`Text.IDNA2008.Internal.Flags`.

#### Editorial flags

`emoji-ok` (admits a subset of `Emoji=Yes` codepoints, excluding
the UTS #46 mapped-emoji subset) and `map-uts46` (applies the
hand-curated 57-entry UTS #46 mapping subset documented in
`Text.IDNA2008.Internal.UTS46`) are this library's editorial
extensions beyond strict IDNA2008 / RFC 5895.  Vectors exercising
them define what /this library/ does and ports of `idna2008` are
expected to match byte-for-byte; they do /not/ define what every
IDNA implementation should do, and other libraries (libidn2,
ICU, etc.) deliberately diverge on this surface.

### `expect`

Exactly one of `ok` (the parse should succeed and produce a
specific result) or `err` (the parse should fail with a
specific error).

#### `expect.ok`

```json
{
  "ok": {
    "wireHex": "037777770765 ... 00",
    "classes": ["WILDLABEL", "LDH", "LDH"]
  }
}
```

`wireHex` is the expected wire-form bytes as a hexadecimal
string.  Whitespace inside the hex is allowed and ignored.
The wire form is the standard DNS uncompressed encoding: each
label preceded by a one-byte length, terminated by a
zero-length root label.

`classes` is the per-label classification: a list with one
entry per label in the parsed name, in the order the labels
appear in the input.  Each entry is one of `LDH`, `RLDH`,
`FAKEA`, `ALABEL`, `ULABEL`, `ATTRLEAF`, `OCTET`, `WILDLABEL`,
or (only when the test's `classes` admission set explicitly
opts in) `LAXULABEL`.  An empty array means the input was
the root domain (zero labels).

For example, `*.example.com` parses to three labels in order
`*`, `example`, `com`, so `classes` is
`["WILDLABEL", "LDH", "LDH"]`.

##### Rendering-direction checks (optional)

Three further optional fields inside `expect.ok` exercise the
ToUnicode side of the library, run only after the parse succeeds
and the wire form and per-label classes have matched:

```json
{
  "ok": {
    "wireHex":         "...",
    "classes":         [...],

    "displayForm":      "münchen.example",
    "displayFormLax":   "💩.example",
    "displayFormAscii": "xn--mnchen-3ya.example",
    "displayFormOpts": {
      "flags": "bidi-check",
      "ok":   "münchen.example"
    }
  }
}
```

`displayForm` is compared against the result of `domainToUnicode`
applied to the parsed `Domain`.  Strict: an ACE label that
doesn't round-trip is rendered verbatim (as its `xn--` form);
non-LDH content in an OCTET label is escaped per zone-file
conventions (`\\C` for syntactic specials, `\\DDD` for other
non-printable / non-ASCII bytes).

`displayFormLax` is compared against a test-local
`domainToUnicodeLax` that the test driver builds on top of
`unparseDomainOpts` with a permissive `LabelFormSet` (every
clean-path form plus `LAXULABEL`).  This admits labels whose
Punycode body decodes to codepoints that strict IDNA2008 would
reject (`FAKEA` ACE bodies surface as their decoded codepoints
rather than their `xn--` form) while preserving the cross-label
Bidi check and the rest of the strict-form treatment.

`displayFormAscii` is compared against `domainToAscii`, which
keeps every label in ASCII form: `xn--` labels stay literal
(no decoding back to U-label), and OCTET / WILDLABEL / ATTRLEAF
bytes get the same `\\C`-or-`\\DDD` escape treatment as in the
other two paths.  Useful for callers who want the on-the-wire
text representation (zone files, DNS-bound output).

`displayFormOpts` invokes `unparseDomainOpts` with a caller-
supplied flag set and (optionally) a caller-supplied
`LabelFormSet`.  The shape is:

```json
"displayFormOpts": {
  "flags":   "bidi-check",
  "classes": "host,+laxulabel",
  "ok":      "..."
}
```

`flags` uses the same command-line option syntax as the top-level
`flags` field.  The most useful ones at render time are:

  * `BIDICHECK` enables the cross-label RFC 5893 check.  This
    is on by default (`defaultIdnaFlags` is the seed when no
    leading sign is given).
  * `ASCIIFALLBACK` renders the whole domain in its all-ASCII
    A-label form when the Bidi check would otherwise fail,
    instead of producing an error.
  * `EMOJIOK` admits emoji codepoints when checking the
    decoded form of an `xn--` label.
  * `NFCCHECK` requires the decoded form to be in NFC.

`classes` is optional; absent, the renderer reuses the
admission set from the parse step.  Including `LAXULABEL`
(`"...,+laxulabel"`) admits U-labels that fail strict IDN
validation and, when any such label is actually present,
suppresses the cross-label Bidi check on the rendered output
(since `LAXULABEL` labels can carry codepoints outside the
Bidi rule's purview).

The `displayFormOpts` object must specify exactly one of `ok`
(the call should return `Right`) or `err` (the call should
return `Left`).  The `err` shape is the same as `expect.err` (see
below).

Multiple rendering fields can coexist; each is checked
independently.  Absent fields are no-ops.

The root domain renders as the literal `"."` under all
three renderers.

#### `expect.err`

```json
{
  "err": {
    "kind": "ErrLabelInvalid",
    "labelIndex": 0,
    "reason": "LabelBidi",
    "rule": "BidiRule1FirstNotLRAL"
  }
}
```

`kind` is required; everything else is optional.  Only fields
present in the vector are checked, so a vector that says
`"kind": "ErrEmptyLabel"` and nothing else will accept any
`ErrEmptyLabel`-shaped error regardless of label index.

| Field | Applies to | Type |
| --- | --- | --- |
| `kind` | every error | one of `ErrEmptyLabel`, `ErrLabelTooLong`, `ErrNameTooLong`, `ErrBadEscape`, `ErrInvalidUtf8`, `ErrCodepointTooLarge`, `ErrUnpresentableLabel`, `ErrFormNotAllowed`, `ErrLabelInvalid`, `ErrAceInvalid`, `ErrPunycodeOverflow`, `ErrCrossLabelBidi` |
| `labelIndex` | most errors | integer; `-1` is reported as absent |
| `reason` | `ErrLabelInvalid`, `ErrAceInvalid` | one of `DisallowedCodepoint`, `ContextRule`, `NotNFC`, `LabelBidi`, `HyphenViolation`, `LeadingCombiningMark`, `BadPunycode`, `DecodedInvalid`, `RoundTripMismatch` |
| `innerReason` | `ErrAceInvalid (DecodedInvalid ...)` | a `LabelReason` tag |
| `rule` | `ErrLabelInvalid (LabelBidi ...)`, `ErrCrossLabelBidi` | `BidiRule1FirstNotLRAL` ... `BidiRule6LTRBadEnd` |
| `codepoint` | `ErrCodepointTooLarge`, `ErrLabelInvalid (DisallowedCodepoint ...)`, `ErrLabelInvalid (ContextRule ...)` | integer codepoint |
| `length` | `ErrLabelTooLong`, `ErrNameTooLong` | integer byte length |

## Adding vectors

Every new vector should pin down a specific behaviour: a known
positive case, a regression for a bug fix, or a known negative
case for a particular spec rule.  Vector names are free-form
identifiers; pick something specific
(`bidi-rule-3-trailing-mark-rejected` beats `test42`).

When adding positive vectors with non-trivial wire forms (IDN
encodings via Punycode), it's tempting to compute the expected
hex by running the library itself.  That's fine /once/ -- for a
fresh build of the same library -- but turns the vector into a
self-test rather than a conformance test.  Where possible,
cross-check the wire form against a second implementation
(another IDNA library, the IANA test vectors, RFC 5891 / 5893
worked examples).

## Cross-language ports

When porting the library to another language, point the new
implementation's test runner at this same `vectors.json`.  The
schema is intentionally simple enough to parse with whatever
JSON facility the language has, and the comparisons are
bytewise (wire form, per-label class names, error tags) so
there's no ambiguity in what counts as "matching".

The per-label `classes` array assumes the implementation
exposes its label classification as a vector (or array, or
list) indexed in label order.  A port whose API only returns a
union bitmask should derive the per-label vector during
parsing rather than throwing the per-label information away.
