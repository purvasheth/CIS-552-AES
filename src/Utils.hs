module Utils
  ( displayHex,
    stringToByteString,
    xorByteString,
    mapInd,
    leftShift,
    leftShiftBy,
    chunk,
    rightShiftBy,
    Block,
    QuarterBlock,
    Key,
    getString,
    getBlocks,
  )
where

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
import Test.QuickCheck (ASCIIString)

-- Types

type Block = B.ByteString

type QuarterBlock = B.ByteString

type Key = B.ByteString

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

leftShift :: [a] -> [a]
leftShift [] = []
leftShift (x : xs) = xs ++ [x]

testLeftShift :: Test
testLeftShift =
  TestList
    [ leftShift [1] ~?= [1],
      leftShift [1, 2, 3] ~?= [2, 3, 1]
    ]

leftShiftBy :: Int -> [a] -> [a]
leftShiftBy _ [] = []
leftShiftBy 0 l = l
leftShiftBy n l = leftShiftBy (n - 1) (leftShift l)

testLeftShiftBy :: Test
testLeftShiftBy =
  TestList
    [ leftShiftBy 0 [1, 2, 3] ~?= [1, 2, 3],
      leftShiftBy 1 [1, 2, 3] ~?= [2, 3, 1],
      leftShiftBy 2 [1, 2, 3] ~?= [3, 1, 2],
      leftShiftBy 3 [1, 2, 3] ~?= [1, 2, 3]
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

w32_1 :: B.ByteString
w32_1 = B.pack [0x54, 0x77, 0x6f, 0x20]

w32_2 :: B.ByteString
w32_2 = B.pack [0x54, 0x68, 0x61, 0x74]

stringToByteString :: String -> B.ByteString
stringToByteString str = B.pack (map (fromIntegral . ord) str)

byteStringToString :: B.ByteString -> String
byteStringToString bs = map (chr . fromIntegral) (B.unpack bs)

-- bitwise xor bytes
xorByteString :: B.ByteString -> B.ByteString -> B.ByteString
xorByteString b1 b2 = B.pack $ map (uncurry xor) (B.zip b1 b2)

testXorByteString :: Test
testXorByteString =
  TestList
    [ xorByteString w32_1 w32_1 ~?= B.pack [0, 0, 0, 0],
      xorByteString w32_1 w32_2 ~?= B.pack [0x00, 0x1f, 0x0e, 0x54],
      xorByteString w32_2 w32_1 ~?= B.pack [0x00, 0x1f, 0x0e, 0x54],
      xorByteString w32_2 (B.pack [0, 0, 0, 0]) ~?= w32_2
    ]

-- see bytestring in (slightly more) human readable form
displayHex :: B.ByteString -> [String]
displayHex bs = map (`showHex` "") (B.unpack bs)

testDisplayHex :: Test
testDisplayHex =
  displayHex w32_1 ~?= ["54", "77", "6f", "20"]

getBlocks :: String -> [Block]
getBlocks str =
  let bs = stringToByteString str
   in chunk 16 $ B.append (B.replicate (padLength bs) 0x00) bs
  where
    padLength bs =
      let l = mod (B.length bs) 16
       in if l == 0
            then 0
            else 16 - l

testGetBlocks :: Test
testGetBlocks =
  TestList
    [ getBlocks "just 12 bits"
        ~?= [B.append (B.replicate 4 0x00) (stringToByteString "just 12 bits")],
      getBlocks "pad nothing here"
        ~?= [stringToByteString "pad nothing here"]
    ]

getString :: [Maybe Block] -> Maybe String
getString mblocks =
  byteStringToString . unpad . B.concat <$> convert mblocks
  where
    convert :: [Maybe Block] -> Maybe [Block]
    convert [] = Just []
    convert (x : xs) = do
      block <- x
      tail <- convert xs
      return (block : tail)
    unpad bs = B.drop (countNull bs) bs
    countNull bs = fst $ B.foldl isNull (0, True) bs
    isNull (count, bool) x =
      if x == 0x00 && bool
        then (count + 1, True)
        else (count, False)

testGetString :: Test
testGetString =
  TestList
    [ getString
        [Just $ B.append (B.replicate 4 0x00) (stringToByteString "just 12 bits")]
        ~?= Just "just 12 bits",
      getString [Just (stringToByteString "pad nothing here")]
        ~?= Just "pad nothing here"
    ]

utilTests :: IO ()
utilTests = do
  putStrLn "Unit tests:"
  _ <- runTestTT $ TestList [testChunk, testLeftShift, testLeftShiftBy, testRightShiftBy, testXorByteString, testDisplayHex, testGetBlocks, testGetString]
  putStrLn ""