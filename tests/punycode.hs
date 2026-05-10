{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Main
-- Description : RFC 3492 section 7.1 Punycode conformance vectors,
--               tested as raw codec round-trips through
--               'Text.IDNA2008.Internal.Punycode'.
-- Copyright   : (c) Viktor Dukhovni, 2026
-- License     : BSD-3-Clause
--
-- Maintainer  : ietf-dane@dukhovni.org
-- Stability   : unstable
--
-- These vectors test the Punycode codec on its own terms, without
-- the IDNA validation layer wrapped around it.  They reproduce the
-- worked examples in RFC 3492 section 7.1 (\"Sample Strings\") for
-- Arabic (Egyptian), Chinese (simplified and traditional), Czech,
-- Hebrew, Hindi, Japanese, Korean, Russian, Spanish, Vietnamese,
-- plus the additional ad-hoc cases the RFC lists for short input,
-- punctuation, and mixed-case round-tripping.
--
-- Note on case preservation: Punycode preserves the case of the
-- basic-ASCII prefix in the encoded form.  Vectors @D@ (Czech),
-- @J@ (Spanish), @K@ (Vietnamese), @L@, @M@, @N@, @P@ feed
-- mixed-case U-labels and expect mixed-case Punycode bodies on
-- output.  The IDNA layer's @MAPCASE@ option (which lower-cases the
-- input before encoding) is not exercised here.
module Main (main) where

import Control.Monad.ST (runST)
import Data.Char (chr, ord)
import Data.Primitive.ByteArray
    ( ByteArray
    , byteArrayFromList
    , freezeByteArray
    , indexByteArray
    , newByteArray
    )
import Data.Primitive.PrimArray
    ( PrimArray
    , newPrimArray
    , primArrayFromList
    , readPrimArray
    )
import Data.Word (Word8)
import qualified Data.ByteString as BS

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit
    ( Assertion
    , assertEqual
    , assertFailure
    , testCase
    )

import Text.IDNA2008.Internal.Punycode
    ( PunycodeErr (..)
    , punycodeDecode
    , punycodeEncode
    )

----------------------------------------------------------------------
-- Ergonomic wrappers around the buffer-based Punycode API.
----------------------------------------------------------------------

-- | Encode a 'String' of Unicode codepoints to a Punycode ASCII
-- 'BS.ByteString' (without the @\"xn--\"@ prefix).
encode :: String -> Either PunycodeErr BS.ByteString
encode cps = runST do
    let !inBuf = primArrayFromList (map ord cps) :: PrimArray Int
        !n    = length cps
        !cap  = n * 8 + 64        -- ample for any well-formed input
    outBuf <- newByteArray cap
    res    <- punycodeEncode inBuf n outBuf 0 cap
    case res of
      Left e        -> pure (Left e)
      Right written -> do
          frozen <- freezeByteArray outBuf 0 written
          pure (Right (BS.pack [ indexByteArray frozen i :: Word8
                               | i <- [0 .. written - 1] ]))

-- | Decode a Punycode ASCII 'BS.ByteString' (without the @\"xn--\"@
-- prefix) to a 'String' of Unicode codepoints.
decode :: BS.ByteString -> Either PunycodeErr String
decode bs = runST do
    let !inBuf = byteArrayFromList (BS.unpack bs) :: ByteArray
        !n    = BS.length bs
        !cap  = max 16 (n * 4)
    outBuf <- newPrimArray cap
    res    <- punycodeDecode inBuf 0 n outBuf 0 cap
    case res of
      Left e        -> pure (Left e)
      Right written -> do
          let collect !i !acc
                | i < 0     = pure (Right acc)
                | otherwise = do
                    cp <- readPrimArray outBuf i
                    collect (i - 1) (chr cp : acc)
          collect (written - 1) []

----------------------------------------------------------------------
-- RFC 3492 section 7.1 vectors.
--
-- For each: (mnemonic, U-label as a Haskell String, expected Punycode
-- body without the "xn--" prefix).  Verified byte-for-byte against
-- the RFC text and against Python's bundled @encodings.punycode@
-- codec.
----------------------------------------------------------------------

type Vector = (String, String, BS.ByteString)

rfc3492Vectors :: [Vector]
rfc3492Vectors =
    [ ("(A) Arabic (Egyptian)"
      , "\x0644\x064A\x0647\x0645\x0627\x0628\x062A\x0643\x0644\
        \\x0645\x0648\x0634\x0639\x0631\x0628\x064A\x061F"
      , "egbpdaj6bu4bxfgehfvwxn")
    , ("(B) Chinese (simplified)"
      , "\x4ED6\x4EEC\x4E3A\x4EC0\x4E48\x4E0D\x8BF4\x4E2D\x6587"
      , "ihqwcrb4cv8a8dqg056pqjye")
    , ("(C) Chinese (traditional)"
      , "\x4ED6\x5011\x7232\x4EC0\x9EBD\x4E0D\x8AAA\x4E2D\x6587"
      , "ihqwctvzc91f659drss3x8bo0yb")
    , ("(D) Czech"
      , "Pro\x010Dprost\x011Bnemluv\x00ED\x010D\&esky"
      , "Proprostnemluvesky-uyb24dma41a")
    , ("(E) Hebrew"
      , "\x05DC\x05DE\x05D4\x05D4\x05DD\x05E4\x05E9\x05D5\x05D8\
        \\x05DC\x05D0\x05DE\x05D3\x05D1\x05E8\x05D9\x05DD\x05E2\
        \\x05D1\x05E8\x05D9\x05EA"
      , "4dbcagdahymbxekheh6e0a7fei0b")
    , ("(F) Hindi (Devanagari)"
      , "\x092F\x0939\x0932\x094B\x0917\x0939\x093F\x0928\x094D\
        \\x0926\x0940\x0915\x094D\x092F\x094B\x0902\x0928\x0939\
        \\x0940\x0902\x092C\x094B\x0932\x0938\x0915\x0924\x0947\
        \\x0939\x0948\x0902"
      , "i1baa7eci9glrd9b2ae1bj0hfcgg6iyaf8o0a1dig0cd")
    , ("(G) Japanese (kanji and hiragana)"
      , "\x306A\x305C\x307F\x3093\x306A\x65E5\x672C\x8A9E\x3092\
        \\x8A71\x3057\x3066\x304F\x308C\x306A\x3044\x306E\x304B"
      , "n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa")
    , ("(H) Korean (Hangul syllables)"
      , "\xC138\xACC4\xC758\xBAA8\xB4E0\xC0AC\xB78C\xB4E4\xC774\
        \\xD55C\xAD6D\xC5B4\xB97C\xC774\xD574\xD55C\xB2E4\xBA74\
        \\xC5BC\xB9C8\xB098\xC88B\xC744\xAE4C"
      , "989aomsvi5e83db1d2a355cv1e0vak1dwrv93d5xbh15a0dt30a5j\
        \psd879ccm6fea98c")
      -- RFC 3492 as originally published spells the encoded form
      -- with an uppercase @D@ in the middle:
      -- @\"b1abfaaepdrnnbgefbaDotcwatmq2g4l\"@.  Per the published
      -- erratum that's a typo -- the encoder produces an
      -- all-lowercase body, since the input is all-lowercase
      -- Cyrillic and Punycode preserves the case of the basic
      -- prefix, of which this example has none.
      -- See <https://www.rfc-editor.org/errata_search.php?rfc=3492>.
    , ("(I) Russian (Cyrillic)"
      , "\x043F\x043E\x0447\x0435\x043C\x0443\x0436\x0435\x043E\
        \\x043D\x0438\x043D\x0435\x0433\x043E\x0432\x043E\x0440\
        \\x044F\x0442\x043F\x043E\x0440\x0443\x0441\x0441\x043A\
        \\x0438"
      , "b1abfaaepdrnnbgefbadotcwatmq2g4l")
    , ("(J) Spanish"
      , "Porqu\x00E9nopuedensimplementehablarenEspa\x00F1ol"
      , "PorqunopuedensimplementehablarenEspaol-fmd56a")
    , ("(K) Vietnamese"
      , "T\x1EA1isaoh\x1ECDkh\x00F4ngth\x1EC3\&ch\x1EC9n\x00F3it\
        \i\x1EBFngVi\x1EC7t"
      , "TisaohkhngthchnitingVit-kjcr8268qyxafd2f1b9g")
    , ("(L) 3<nen>B<gumi>...kinpachi sensei"
      , "3\x5E74\&B\x7D44\x91D1\x516B\x5148\x751F"
      , "3B-ww4c5e180e575a65lsy2b")
    , ("(M) Amuro Namie -with-SUPER-MONKEYS"
      , "\x5B89\x5BA4\x5948\x7F8E\x6075-with-SUPER-MONKEYS"
      , "-with-SUPER-MONKEYS-pc58ag80a8qai00g7n9n")
    , ("(N) Hello-Another-Way-<sorezore-no-basho>"
      , "Hello-Another-Way-\x305D\x308C\x305E\x308C\x306E\x5834\
        \\x6240"
      , "Hello-Another-Way--fc4qua05auwb3674vfr0b")
    , ("(O) <hitotsu yane no shita>2"
      , "\x3072\x3068\x3064\x5C4B\x6839\x306E\x4E0B\&2"
      , "2-u9tlzr9756bt3uc0v")
    , ("(P) Maji-de-Koi-suru-5-byou-mae"
      , "Maji\x3067\&Koi\x3059\x308B\&5\x79D2\x524D"
      , "MajiKoi5-783gue6qz075azm5e")
    , ("(Q) <pafii> de Rumba"
      , "\x30D1\x30D5\x30A3\x30FC\&de\x30EB\x30F3\x30D0"
      , "de-jg4avhby1noc0d")
    , ("(R) <sono speed de>"
      , "\x305D\x306E\x30B9\x30D4\x30FC\x30C9\x3067"
      , "d9juau41awczczp")
    , ("(S) -> $1.00 <-"
      , "-> $1.00 <-"
      , "-> $1.00 <--")
    ]

----------------------------------------------------------------------
-- Test harness.
----------------------------------------------------------------------

-- | Check that encoding the U-label produces the expected Punycode
-- body, /and/ that decoding the body recovers the U-label.
roundTrip :: Vector -> TestTree
roundTrip (name, u, expected) = testCase name do
    case encode u of
      Left e     -> assertFailure ("encode failed: " ++ show e)
      Right got  -> assertEqual "encode" expected got
    case decode expected of
      Left e     -> assertFailure ("decode failed: " ++ show e)
      Right got  -> assertEqual "decode (round-trip)" u got

-- | Edge cases not in RFC 3492 section 7.1 but worth pinning down:
-- empty input, all-basic input (no delimiter, no extension), and a
-- single non-ASCII codepoint.
edgeCases :: TestTree
edgeCases = testGroup "edge cases"
    [ testCase "empty input encodes to empty" $
        encode "" @?= Right BS.empty
    , testCase "empty input decodes from empty" $
        decode BS.empty @?= Right ""
    , testCase "all-basic input gets a trailing delimiter" $
        -- RFC 3492 section 6.3: copy basics in order, followed by
        -- a delimiter if b > 0 -- including when b == length(input).
        encode "abc-123" @?= Right "abc-123-"
    , testCase "all-basic body round-trips through trailing delimiter" $
        decode "abc-123-" @?= Right "abc-123"
    , testCase "all-basic body without delimiter is malformed" $
        -- The last hyphen in a valid Punycode body terminates the
        -- basic prefix; in @\"abc-123\"@ that's the hyphen between
        -- @c@ and @1@, leaving @\"123\"@ as a base-36 integer that
        -- doesn't terminate.
        decode "abc-123" @?= Left PunycodeTruncated
    , testCase "single non-ASCII codepoint" $ do
        encode "\x00FC" @?= Right "tda"
        decode "tda"    @?= Right "\x00FC"
    ]
  where
    -- Locally-scoped operator to keep the import list tidy.
    (@?=) :: (Eq a, Show a) => a -> a -> Assertion
    x @?= y = assertEqual "" y x

main :: IO ()
main = defaultMain $ testGroup "Punycode (RFC 3492)"
    [ testGroup "section 7.1 sample strings"
        (map roundTrip rfc3492Vectors)
    , edgeCases
    ]
