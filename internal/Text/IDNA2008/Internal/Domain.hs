-- |
-- Module      : Text.IDNA2008.Internal.Domain
-- Description : Wire-form domain name newtype
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- Minimal wire-form domain name representation produced by the
-- parser.  A 'Domain' is the byte sequence that goes on the
-- wire: each label is preceded by a one-byte length, and the
-- whole thing ends with a zero-length label (the DNS root).
--
-- Non-ASCII labels in a 'Domain' are always encoded as
-- @\"xn--\"@-prefixed Punycode A-labels; ASCII labels are
-- copied verbatim.  The parser is responsible for that
-- conversion, and the byte sequence stored here is what a DNS
-- resolver would actually transmit.
--
-- 'toLabels' walks the wire form and yields each label's bytes
-- (without the leading length byte and without the trailing
-- root).  'wireBytes' returns the whole wire-form buffer as a
-- regular 'ByteString'.
--
-- Equality and ordering are byte-exact on the wire form, so two
-- 'Domain' values compare equal iff their wire bytes are
-- identical.  Case-insensitive comparison is NOT provided by
-- this module; callers that need it should normalise to lower
-- case first (the parser already lower-cases ASCII labels by
-- default).
--
-- == Smart constructor
--
-- 'Domain' is exposed externally only as a bidirectional
-- pattern synonym.  As a pattern it matches any 'Domain' and
-- binds the underlying 'ShortByteString'; as an expression it
-- validates the bytes against the DNS wire-form rules
-- ('isValidDomainWire') and raises an error on malformed input.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.IDNA2008.Internal.Domain
    ( Domain (Domain, Domain_)
    , wireBytes
    , toLabels
    , toLabelsFromWire
    , isValidDomainWire
    ) where

import qualified Data.ByteString.Short as SB
import qualified Data.Primitive.ByteArray as A
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.Word (Word8)

import Text.IDNA2008.Internal.Util (baToShortByteString, sbsToByteArray)

-- | A wire-form fully-qualified domain name.
--
-- Stored as a 'ShortByteString' to avoid pinning, since the
-- typical name is short and the buffer outlives the parser
-- run.  The encoding is the standard DNS uncompressed wire
-- form: each label is one length byte followed by that many
-- content bytes, with a final zero-length label terminating
-- the name.
--
-- The bare data constructor is named 'Domain_' and is exposed
-- only via the internal library; external callers construct
-- 'Domain' values through the 'Domain' bidirectional pattern
-- synonym, which validates the wire bytes.
newtype Domain = Domain_ ShortByteString
    deriving (Eq, Ord, Show)

-- | Bidirectional pattern synonym for 'Domain'.
--
-- As a pattern (@case dom of Domain sbs -> ...@) it matches any
-- 'Domain' and binds the underlying wire-form
-- 'ShortByteString'.
--
-- As an expression (@Domain sbs@) it builds a 'Domain' from
-- the supplied bytes after checking that they form a
-- well-formed DNS wire encoding: overall length in @[1, 255]@,
-- last byte is the @NUL@ root terminator, every other length
-- byte is in @[1, 63]@, and the label walk lands exactly on the
-- terminator without overrunning the buffer or consuming the
-- root byte as content.  Malformed input raises an error;
-- callers who want a total construction path can pre-check
-- with 'isValidDomainWire' or use the parser entry points,
-- which never produce malformed wire bytes by construction.
pattern Domain :: ShortByteString -> Domain
pattern Domain sbs <- Domain_ sbs
  where
    Domain sbs
      | isValidDomainWire sbs = Domain_ sbs
      | otherwise = errorWithoutStackTrace
          "Text.IDNA2008.Domain: malformed wire bytes"

{-# COMPLETE Domain #-}

-- | The whole wire-form buffer as a regular 'ByteString'.
-- Zero-copy.
wireBytes :: Domain -> ByteString
wireBytes (Domain_ sbs) = SB.fromShort sbs

-- | Walk the wire form and yield each label's content bytes,
-- skipping the leading length byte and stopping at the
-- terminal zero-length root label.
toLabels :: Domain -> [ShortByteString]
toLabels (Domain_ sbs) = toLabelsFromWire sbs

-- | Wire-bytes variant of 'toLabels': accepts a 'ShortByteString'
-- assumed to be valid DNS wire form (caller's responsibility, no
-- validation performed) and yields each label's content bytes.
toLabelsFromWire :: ShortByteString -> [ShortByteString]
toLabelsFromWire sbs = go 0
  where
    sblen = SB.length sbs
    ba  = sbsToByteArray sbs
    go !off
        | off < sblen
        , llen <- fromIntegral (A.indexByteArray ba off :: Word8)
        , off' <- off + llen + 1
        = if | llen == 0 && off' == sblen -> []
             | llen > 0 && off' < sblen
             , lba <- A.cloneByteArray ba (off + 1) llen
             -> baToShortByteString lba : go off'
             | otherwise -> error "Invalid wire form domain"
        | otherwise = error "Invalid wire form domain"

-- | Check that the supplied 'ShortByteString' is a well-formed
-- DNS wire-form name:
--
--   * overall length is in @[1, 255]@ (DNS message-section cap
--     on a single name);
--   * the final byte is the @NUL@ root terminator;
--   * every other length byte is in @[1, 63]@ (no compression
--     pointers, no reserved length-byte values, no zero-length
--     non-root labels);
--   * the label walk lands exactly on the terminator (no
--     truncation, no overrun, no trailing junk past the root).
--
isValidDomainWire :: ShortByteString -> Bool
isValidDomainWire sbs =
    n >= 1 && n <= 255 && walk 0
  where
    !ba = sbsToByteArray sbs
    !n  = SB.length sbs
    walk !off
      | off >= n   = False
      | llen == 0  = off == n - 1
      | llen <= 63 = walk (off + llen + 1)
      | otherwise  = False
      where
        !llen = fromIntegral (A.indexByteArray ba off :: Word8) :: Int
