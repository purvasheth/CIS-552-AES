module KeyExpansion (generateKey, generateAllKeys) where

import Data.Array ((!))
import Data.Array qualified as A
import Data.Bits (Bits (xor))
import Data.ByteString qualified as B
import Data.Word (Word8)
import SBox (sbox)
import Test.HUnit
  ( Counts,
    Test (TestList),
    runTestTT,
    (~?),
    (~?=),
  )
import Utils
  ( Key,
    QuarterBlock,
    chunk,
    displayHex,
    leftShift,
    stringToByteString,
    xorByteString,
  )

chunkKey128 :: [a] -> Maybe [a]
chunkKey128 x@[_, _, _, _] = Just x
chunkKey128 _ = Nothing

rotWord :: QuarterBlock -> QuarterBlock
rotWord w = B.pack (leftShift (B.unpack w))

rconArray :: A.Array Int Word8
rconArray =
  A.listArray
    (1, 10)
    [0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1B, 0x36]

rcon :: Int -> QuarterBlock
rcon rc = B.pack [rconArray ! rc, 0, 0, 0]

generateKey :: Int -> Maybe Key -> Maybe Key
generateKey rc k =
  do
    key <- k
    [w0, w1, w2, w3] <- chunkKey128 (chunk 4 key)
    let w4 = xorByteString w0 (xorByteString (rcon rc) (sbox . rotWord $ w3))
    let w5 = xorByteString w4 w1
    let w6 = xorByteString w5 w2
    let w7 = xorByteString w6 w3
    return (B.concat [w4, w5, w6, w7])

generateAllKeys :: Maybe Key -> [Maybe Key]
generateAllKeys k = do
  let k1 = generateKey 1 k
  let k2 = generateKey 2 k1
  let k3 = generateKey 3 k2
  let k4 = generateKey 4 k3
  let k5 = generateKey 5 k4
  let k6 = generateKey 6 k5
  let k7 = generateKey 7 k6
  let k8 = generateKey 8 k7
  let k9 = generateKey 9 k8
  let k10 = generateKey 10 k9
  [k10, k9, k8, k7, k6, k5, k4, k3, k2, k1]

-- regression test that verifies the last key
testKeys :: Maybe Key -> Maybe Bool
testKeys k = do
  let k1 = generateKey 1 k
  let k2 = generateKey 2 k1
  let k3 = generateKey 3 k2
  let k4 = generateKey 4 k3
  let k5 = generateKey 5 k4
  let k6 = generateKey 6 k5
  let k7 = generateKey 7 k6
  let k8 = generateKey 8 k7
  let k9 = generateKey 9 k8
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

key :: Key
key = stringToByteString "Thats my Kung Fu"

-- >>> testKeys (Just key)
-- Just True
