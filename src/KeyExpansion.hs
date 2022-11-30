module KeyExpansion (generateKey) where

import Data.Array ((!))
import Data.Array qualified as A
import Data.Bits (Bits (xor))
import Data.ByteString qualified as B
import Data.Word (Word8)
import Lib (chunk, displayHex, shift, stringToByteString, xorByteString)
import SBox (sbox)
import Test.HUnit
  ( Counts,
    Test (TestList),
    runTestTT,
    (~?),
    (~?=),
  )

chunkKey128 :: [a] -> Maybe [a]
chunkKey128 x@[_, _, _, _] = Just x
chunkKey128 _ = Nothing

rotWord :: B.ByteString -> B.ByteString
rotWord w = B.pack (shift (B.unpack w))

rconArray :: A.Array Int Word8
rconArray =
  A.listArray
    (1, 10)
    [0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1B, 0x36]

rcon :: Int -> B.ByteString
rcon rc = B.pack [rconArray ! rc, 0, 0, 0]

generateKey :: Int -> B.ByteString -> Maybe B.ByteString
generateKey rc k =
  do
    [w0, w1, w2, w3] <- chunkKey128 (chunk 4 k)
    let w4 = xorByteString w0 (xorByteString (rcon rc) (sbox . rotWord $ w3))
    let w5 = xorByteString w4 w1
    let w6 = xorByteString w5 w2
    let w7 = xorByteString w6 w3
    return (B.concat [w4, w5, w6, w7])

testKeys :: B.ByteString -> Maybe Bool
testKeys k = do
  k1 <- generateKey 1 k
  k2 <- generateKey 2 k1
  k3 <- generateKey 3 k2
  k4 <- generateKey 4 k3
  k5 <- generateKey 5 k4
  k6 <- generateKey 6 k5
  k7 <- generateKey 7 k6
  k8 <- generateKey 8 k7
  k9 <- generateKey 9 k8
  k10 <- generateKey 10 k9
  return
    ( k10
        == B.pack
          [ 0x28,
            0xFD,
            0xDE,
            0xF8,
            0x6D,
            0xA4,
            0x24,
            0x4A,
            0xCC,
            0xC0,
            0xA4,
            0xFE,
            0x3B,
            0x31,
            0x6F,
            0x26
          ]
    )

key :: B.ByteString
key = stringToByteString "Thats my Kung Fu"

-- >>> testKeys key
-- Just True
