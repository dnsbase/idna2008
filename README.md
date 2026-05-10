# idna2008

A Haskell library for parsing and validating internationalized
domain names: domain names that may contain characters from
non-Latin scripts (Greek, Hebrew, Arabic, CJK, ...) alongside the
conventional letters, digits, and hyphens.

## What it does

Given a domain name as a string (with whatever mix of ASCII and
non-ASCII characters the user typed), the library:

  * Checks that every label (the parts between dots) is allowed.
  * Encodes any non-ASCII label into its `xn--`-prefixed form,
    suitable for the wire and for DNS lookups.
  * Tells the caller what kind of label each one is (see below).
  * Optionally renders the parsed name back to display form
    (Unicode where possible, ASCII where not).

## Per-label classification

A single domain name often mixes different kinds of labels.  The
library reports each label as one of:

| Class       | What it is                                              |
|-------------|---------------------------------------------------------|
| `LDH`       | Letter-digit-hyphen.  The conventional hostname alphabet. |
| `RLDH`      | Legacy reserved labels with `--` at positions 3-4.      |
| `FAKEA`     | An `xn--`-prefixed label that doesn't actually decode.  |
| `ALABEL`    | An `xn--`-prefixed label that's a valid IDN.            |
| `ULABEL`    | A Unicode label, encoded to `xn--` form on the wire.    |
| `ATTRLEAF`  | An underscore-prefixed label (e.g. `_25._tcp`, `_dmarc`). |
| `OCTET`     | A label with characters outside the LDH alphabet.       |
| `WILDLABEL` | The DNS wildcard label `*`.                             |

A name like `_25._tcp.müllers.example.de` parses cleanly with
five labels in three different classes (`ATTRLEAF`, `ULABEL`,
`LDH`).  Most existing IDNA libraries don't make these
distinctions; they assume every label is the same kind, which
doesn't match how real DNS names look.

## What's distinctive

* **Strict.**  Some browsers and language standard libraries use
  a more permissive variant of the IDNA standard that accepts
  characters strict IDNA2008 rejects.  This library does not use
  that variant; if a name is admitted, it's by-the-book valid.

* **Bidirectional-text rules in two layers.**  When right-to-left
  scripts (Hebrew, Arabic) appear in a domain name, special rules
  prevent visual confusion with neighbouring left-to-right text.
  The library splits these rules into a per-label check (does the
  label make sense on its own?) and a cross-label check (do the
  labels make sense together?), each independently configurable.
  An ASCII-fallback option lets display code show a safe ASCII
  spelling when the cross-label check would otherwise reject the
  name.

* **Up-to-date Unicode coverage.**  The Unicode Consortium
  publishes new versions of its character database every year or
  so; this library derives its tables directly from those
  publications and stays current.

* **Conformance test vectors.**  Test cases are published as
  JSON, reusable by ports to other programming languages.

## Status

Initial package skeleton.  The implementation lands incrementally;
see `CHANGELOG.md`.

## License

BSD-3-Clause.
