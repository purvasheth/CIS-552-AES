module Utils (displayHex, stringToByteString, xorByteString, mapInd, shift, shiftBy, chunk, rightShiftBy) where

import Data.Bits (xor)
import Data.ByteString qualified as B
import Data.Char (chr, ord)
import Data.Map qualified as Map
import Data.Word (Word8)
import Numeric (readHex, showHex)
import Test.HUnit
  ( Counts,
    Test (TestList),
    runTestTT,
    (~?),
    (~?=),
  )

-- map a list with indices
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0 ..]

-- chunk a ByteString into parts
chunk :: Int -> B.ByteString -> [B.ByteString]
chunk k = takeWhile (not . B.null) . map (B.take k) . iterate (B.drop k)

testChunk :: Test
testChunk =
  TestList
    [ chunk 3 (B.pack [1, 2, 3, 4, 5, 6]) ~?= [B.pack [1, 2, 3], B.pack [4, 5, 6]],
      chunk 5 (B.pack [1, 2, 3, 4, 5, 6]) ~?= [B.pack [1, 2, 3, 4, 5], B.pack [6]]
    ]

-- >>> runTestTT testChunk
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

-- left circular shift
shift :: [a] -> [a]
shift [] = []
shift (x : xs) = xs ++ [x]

testShift :: Test
testShift =
  TestList
    [ shift [1] ~?= [1],
      shift [1, 2, 3] ~?= [2, 3, 1]
    ]

-- >>> runTestTT testShift
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

shiftBy :: Int -> [a] -> [a]
shiftBy _ [] = []
shiftBy 0 l = l
shiftBy n l = shiftBy (n - 1) (shift l)

testShiftBy :: Test
testShiftBy =
  TestList
    [ shiftBy 0 [1, 2, 3] ~?= [1, 2, 3],
      shiftBy 1 [1, 2, 3] ~?= [2, 3, 1],
      shiftBy 2 [1, 2, 3] ~?= [3, 1, 2],
      shiftBy 3 [1, 2, 3] ~?= [1, 2, 3]
    ]

rightShiftBy :: Int -> [a] -> [a]
rightShiftBy _ [] = []
rightShiftBy n l =
  if n < length l
    then drop (length l - n) l ++ take (length l - n) l
    else drop (length l - (n `mod` length l)) l ++ take (length l - (n `mod` length l)) l

testRightShiftBy :: Test
testRightShiftBy =
  TestList
    [ rightShiftBy 0 [1, 2, 3, 4, 5] ~?= [1, 2, 3, 4, 5],
      rightShiftBy 1 [1, 2, 3, 4, 5] ~?= [5, 1, 2, 3, 4],
      rightShiftBy 2 [1, 2, 3, 4, 5] ~?= [4, 5, 1, 2, 3],
      rightShiftBy 5 [1, 2, 3, 4, 5] ~?= [1, 2, 3, 4, 5],
      rightShiftBy 11 [1, 2, 3, 4, 5] ~?= [5, 1, 2, 3, 4]
    ]

-- >>> runTestTT testRightShiftBy
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

w32_1 :: B.ByteString
w32_1 = B.pack [0x54, 0x77, 0x6f, 0x20]

w32_2 :: B.ByteString
w32_2 = B.pack [0x54, 0x68, 0x61, 0x74]

stringToByteString :: String -> B.ByteString
stringToByteString str = B.pack (map (fromIntegral . ord) str)

-- bitwise xor bytes
xorByteString :: B.ByteString -> B.ByteString -> B.ByteString
xorByteString b1 b2 = B.pack $ map (uncurry xor) (B.zip b1 b2)

testXorByteString :: Test
testXorByteString =
  TestList
    [ xorByteString w32_1 w32_1 ~?= B.pack [0, 0, 0, 0],
      xorByteString w32_1 w32_2 ~?= B.pack [0x00, 0x1f, 0x0e, 0x54],
      xorByteString w32_2 w32_1 ~?= B.pack [0x00, 0x1f, 0x0e, 0x54]
    ]

-- >>> runTestTT testXorByteString
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

-- see bytestring in (slightly more) human readable form
displayHex :: B.ByteString -> [String]
displayHex bs = map (`showHex` "") (B.unpack bs)

testDisplayHex :: Test
testDisplayHex =
  displayHex w32_1 ~?= ["54", "77", "6f", "20"]

-- >>> runTestTT testDisplayHex
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
