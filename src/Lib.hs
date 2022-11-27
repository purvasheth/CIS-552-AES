module Lib (Word32 (..), Word8 (..), Key128 (..), Hex (..), Block (..), getWord8, getHex, xorWord32) where

import Data.Bits (xor)
import Data.Map qualified as Map
import Numeric (readHex, showHex)
import Test.HUnit
  ( Counts,
    Test (TestList),
    runTestTT,
    (~?),
    (~?=),
  )

data Hex
  = H0
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | H7
  | H8
  | H9
  | HA
  | HB
  | HC
  | HD
  | HE
  | HF
  deriving (Eq, Show, Ord)

newtype Word8 = W8 (Hex, Hex) deriving (Eq, Show, Ord)

newtype Word32 = W32 (Word8, Word8, Word8, Word8) deriving (Eq, Show)

newtype Key128 = K128 (Word32, Word32, Word32, Word32) deriving (Eq, Show)

newtype Block = B (Word32, Word32, Word32, Word32) deriving (Eq, Show)

hexToChar :: Map.Map Hex Char
hexToChar =
  Map.fromList
    [ (H0, '0'),
      (H1, '1'),
      (H2, '2'),
      (H3, '3'),
      (H4, '4'),
      (H5, '5'),
      (H6, '6'),
      (H7, '7'),
      (H8, '8'),
      (H9, '9'),
      (HA, 'a'),
      (HB, 'b'),
      (HC, 'c'),
      (HD, 'd'),
      (HE, 'e'),
      (HF, 'f')
    ]

charToHex :: Map.Map Char Hex
charToHex =
  Map.fromList
    [ ('0', H0),
      ('1', H1),
      ('2', H2),
      ('3', H3),
      ('4', H4),
      ('5', H5),
      ('6', H6),
      ('7', H7),
      ('8', H8),
      ('9', H9),
      ('a', HA),
      ('b', HB),
      ('c', HC),
      ('d', HD),
      ('e', HE),
      ('f', HF)
    ]

w8_1 :: Word8
w8_1 = W8 (H0, H1)

w8_2 :: Word8
w8_2 = W8 (H2, H3)

w8_3 :: Word8
w8_3 = W8 (H4, H5)

w8_4 :: Word8
w8_4 = W8 (H6, H7)

w32_1 :: Word32
w32_1 = W32 (w8_1, w8_2, w8_3, w8_4)

getHex :: Word8 -> Int -> Maybe Hex
getHex (W8 (h1, h2)) n = case n of
  1 -> Just h1
  2 -> Just h2
  _ -> Nothing

testGetHex :: Test
testGetHex =
  TestList
    [ getHex (W8 (H0, H1)) 1 ~?= Just H0,
      getHex (W8 (H0, H1)) 2 ~?= Just H1,
      getHex (W8 (H0, H1)) 3 ~?= Nothing
    ]

-- >>> runTestTT testGetHex
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

getWord8 :: Word32 -> Int -> Maybe Word8
getWord8 (W32 (w1, w2, w3, w4)) n = case n of
  1 -> Just w1
  2 -> Just w2
  3 -> Just w3
  4 -> Just w4
  _ -> Nothing

testGetWord8 :: Test
testGetWord8 =
  TestList
    [ getWord8 (W32 (w8_1, w8_2, w8_3, w8_4)) 1 ~?= Just w8_1,
      getWord8 (W32 (w8_1, w8_2, w8_3, w8_4)) 2 ~?= Just w8_2,
      getWord8 (W32 (w8_1, w8_2, w8_3, w8_4)) 3 ~?= Just w8_3,
      getWord8 (W32 (w8_1, w8_2, w8_3, w8_4)) 4 ~?= Just w8_4,
      getWord8 (W32 (w8_1, w8_2, w8_3, w8_4)) 5 ~?= Nothing
    ]

getStringFromWord :: Word32 -> Maybe String
getStringFromWord (W32 (W8 (h1, h2), W8 (h3, h4), W8 (h5, h6), W8 (h7, h8))) =
  do
    c1 <- Map.lookup h1 hexToChar
    c2 <- Map.lookup h2 hexToChar
    c3 <- Map.lookup h3 hexToChar
    c4 <- Map.lookup h4 hexToChar
    c5 <- Map.lookup h5 hexToChar
    c6 <- Map.lookup h6 hexToChar
    c7 <- Map.lookup h7 hexToChar
    c8 <- Map.lookup h8 hexToChar
    return [c1, c2, c3, c4, c5, c6, c7, c8]

getWordfromString :: String -> Maybe Word32
getWordfromString [c1, c2, c3, c4, c5, c6, c7, c8] =
  do
    h1 <- Map.lookup c1 charToHex
    h2 <- Map.lookup c2 charToHex
    h3 <- Map.lookup c3 charToHex
    h4 <- Map.lookup c4 charToHex
    h5 <- Map.lookup c5 charToHex
    h6 <- Map.lookup c6 charToHex
    h7 <- Map.lookup c7 charToHex
    h8 <- Map.lookup c8 charToHex
    return (W32 (W8 (h1, h2), W8 (h3, h4), W8 (h5, h6), W8 (h7, h8)))
getWordfromString _ = Nothing

xorWord32 :: Word32 -> Word32 -> Maybe Word32
xorWord32 w1 w2 = do
  s1 <- getStringFromWord w1
  s2 <- getStringFromWord w2
  (h1 :: Int, _) <- headSafe (readHex s1)
  (h2 :: Int, _) <- headSafe (readHex s2)
  let s = showHex (xor h1 h2) ""
  getWordfromString (padLeft 8 '0' s)
  where
    headSafe :: [a] -> Maybe a
    headSafe [] = Nothing
    headSafe (x : _) = Just x

    padLeft :: Int -> a -> [a] -> [a]
    padLeft n x xs = replicate (n - length xs) x ++ xs

-- >>> xorWord32 w32_1 w32_1
-- Just (W32 (W8 (H0,H0),W8 (H0,H0),W8 (H0,H0),W8 (H0,H0)))
