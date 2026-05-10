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

Initial public release (`0.1.0.0`).  Full IDNA2008 parser, per-label
classification, three rendering paths (Unicode, lax Unicode, ASCII),
RFC 5895 input mappings, and RFC 5893 Bidi rules (per-label and
cross-label).  Conformance suite of 140+ JSON test vectors in
`tests/` is published with a documented schema so ports to other
languages can reuse the fixtures.  See `CHANGELOG.md` for the
feature list.

## Demo

Given the below `demo.hs`:
```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where
import qualified Data.Text.IO as T
import Text.IDNA2008

main :: IO()
main = do
    -- Print A-label form
    mapM_ ascOut $ mkDomain "αβγ.gr"
    -- Print U-label form
    mapM_ uniOut $ mkDomain "αβγ.gr"
    -- Print A-label + U-label forms and label types:
    mapM_ dump $ parseDomain allLabelForms "_25._tcp.*.\\097bc.αβγ.gr"
    -- An invalid domain, with code point 95 ('_') in the second label.
    -- Only LDH ASCII characters can appear in a U-label.  The offset
    -- within that label is non-specific because it may have gone
    -- through some "mappings" that mask the real byte offset.
    print $ parseDomain idnLabelForms "foo.αβ_γδ.gr"
  where
    ascOut, uniOut :: Domain -> IO ()
    ascOut = T.putStrLn . domainToAscii
    uniOut = T.putStrLn . domainToUnicode
    dump (dom, inf) = do
        ascOut dom
        uniOut dom
        print inf
```
Compiling and running it:
```sh
# build the library
$ cabal -v0 -j12 build
# determine full GHC version
$ gv=$(
    printf '%s\n%s\n%s\n%s\n' \
        'import System.Info' \
        'import Data.Version' \
        'vb = versionBranch fullCompilerVersion'
        'putStrLn $ Data.List.intercalate "." $ map show vb' |
    ghci -v0)
# Compile demo program (low-level bypassing Cabal)
$ ghc -v0 -package-db dist-newstyle/packagedb/ghc-$gv \
      -package-db ~/.cabal/store/ghc-$gv-inplace/package.db \
      -package primitive -package idna2008 \
      demo.hs
# Run it
$ ./demo
```
we get the below output:
```
xn--mxacd.gr
αβγ.gr
_25._tcp.*.abc.xn--mxacd.gr
_25._tcp.*.abc.αβγ.gr
[ATTRLEAF,ATTRLEAF,WILDLABEL,OCTET,ULABEL,LDH]
Left (ErrLabelInvalid (IdnaLoc {idnaLabelIndex = 1}) (DisallowedCodepoint 95))
```

## License

BSD-3-Clause.
