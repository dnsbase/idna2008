# Changelog

## 1.0.0.0

First public release of `idna2008`, factored out of `dnsbase` so
it can be used independently of a full DNS stack.

- Strict IDNA2008 (RFC 5891) presentation-form parser with
  Punycode (RFC 3492), per-codepoint disposition (RFC 5892),
  and CONTEXTJ/CONTEXTO contextual rules.
- Per-label classification (`LabelForm`) into one of `LDH`,
  `RLDH`, `FAKEA`, `ALABEL`, `ULABEL`, `ATTRLEAF`, `OCTET`,
  `WILDLABEL`; the parser returns the wire-form `Domain` paired
  with an opaque `LabelInfo` carrying each label's form.
- Caller-controlled permitted set via `LabelFormSet`; the
  parser rejects any label not in the supplied set.  Pre-built
  sets `allLabelForms`, `idnLabelForms`, `hostnameLabelForms`
  cover the common policies.
- Four parser entry points (`parseDomain`, `parseDomainOpts`,
  `parseDomainUtf8`, `parseDomainShort`) plus a `Maybe`-returning
  `mkDomain` family.  Compile-time literals via `dnLit` /
  `dnLitAs` (Template Haskell), with cross-label Bidi checking
  at compile time.
- Default option set
  (`ALABELCHECK <> NFCCHECK <> BIDICHECK`) matches IDNA2008
  lookup-side processing; loose semantics available by passing
  `mempty` to the `Opts` entry points.
- RFC 5893 Bidi rules: per-label at parse time under
  `BIDICHECK`, cross-label at presentation time via
  `domainToUnicodeOpt`.  `ASCIIFALLBACK` renders in A-label form
  when the cross-label rules fire, rather than erroring.
- RFC 5895 input mappings (`MAPDOTS`, `MAPCASE`, `MAPNFC`,
  `MAPWIDTH`); all off by default and opt-in.
- `EMOJIOK` admits emoji codepoints at both parse and render
  time.  `LAXDECODE` makes `domainToUnicodeOpt` surface the
  codepoints of any `xn--` label whose Punycode body decodes
  cleanly, intended for forensic display of registrations that
  strict IDNA2008 excludes.
- Four rendering entry points: infallible `domainToUnicode` and
  `domainToUnicodeLax`, flag-driven `domainToUnicodeOpt`, and
  `domainToAscii` for the wire-flavoured form (every label as
  ASCII bytes, A-labels kept in `xn--` form, OCTETs escaped per
  RFC 1035 master-file conventions).
- Conformance suite of 142 JSON test vectors in
  `tests/vectors.json`, schema documented in `tests/README.md`
  so ports to other languages can reuse the same fixtures.
- CLI vocabulary helpers `parseIdnaFlags` and `parseLabelFormSet`
  with comma-separated `+`/`-` token syntax, presets, and
  unambiguous prefix matching, so downstream tools can expose
  the same flag/classification names consistently.
