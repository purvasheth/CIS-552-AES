module TestAES where

import Control.Monad (replicateM)
import Data.Array (Ix (inRange), bounds, (!))
import Data.Bits (xor)
import Data.ByteString as B
  ( ByteString,
    all,
    append,
    concat,
    cons,
    length,
    pack,
    replicate,
    sort,
    transpose,
    uncons,
  )
import Data.ByteString.Char8 as Char8 (pack)
import Data.Map as Map (Map, lookup)
import Data.Maybe (isJust)
import Data.Word (Word8)
import Decrypt (getPlainText, invShiftRows)
import Encrypt (getCipher, mixCloumns, shiftRows)
import GHC.Base (build)
import MixColumns (mixColumnsMatrix)
import SBox (invSbox, invSboxArray, sbox, sboxArray)
import System.Random (genByteString)
import Test.HUnit
  ( Counts,
    Test (TestList),
    runTestTT,
    (~?),
    (~?=),
  )
import Test.QuickCheck
  ( Arbitrary,
    Gen,
    PrintableString,
    arbitrary,
    getPrintableString,
  )
import Test.QuickCheck as QC
  ( Arbitrary (arbitrary),
    Gen,
    Testable,
    arbitraryPrintableChar,
    maxSuccess,
    quickCheckWith,
    stdArgs,
  )
import Utils (Block, Key, QuarterBlock, chunk, xorByteString)

------- Enable generation of arbitrary blocks ------

genNChars :: Int -> Gen String
genNChars n = replicateM n QC.arbitraryPrintableChar

genBlock :: Gen Block
genBlock = Char8.pack <$> genNChars 16

instance Arbitrary Block where
  arbitrary = genBlock

-- confirm length of a block is always 16 bytes
prop_blockLen :: Block -> Bool
prop_blockLen s = B.length s == 16

{- SubBytes and Inverse SubBytes
-------------------------------}

-- all bytes have an entry in Lookup Array
prop_subBytesHasAllBytes :: Word8 -> Bool
prop_subBytesHasAllBytes = inRange (bounds sboxArray)

-- Fill stateMat with one arbitrary value, make sure the output is all correct
prop_subBytesAllSame :: Word8 -> Bool
prop_subBytesAllSame w8 = B.all (== (sboxArray ! w8)) cypher
  where
    plain :: Block
    plain = B.replicate 16 w8
    cypher :: Block
    cypher = sbox plain

-- all bytes have an entry in the Inverse SubBytes Array
prop_invSubBytesHasAllBytes :: Word8 -> Bool
prop_invSubBytesHasAllBytes = inRange (bounds invSboxArray)

-- Fill stateMat with one arbitrary value, make sure the output is all correct
prop_invSubBytesAllSame :: Word8 -> Bool
prop_invSubBytesAllSame w8 = B.all (== (invSboxArray ! w8)) cypher
  where
    plain :: Block
    plain = B.replicate 16 w8
    cypher :: Block
    cypher = invSbox plain

{- ShiftRows and Inverse ShiftRows
-------------------------------}

{- Create a stateMat with only one row with unique entries. Then isolate the
shift of that one row and confirm it is correct. I.e.
shiftBytesR2([[1,1,1,1],[1,1,1,1],[x, y, z, w],[1,1,1,1]]) ==
             [[1,1,1,1],[1,1,1,1],[z, w, x, y],[1,1,1,1]]
-}

-- state matrix is supposed to be column major, tests were written for row major
transform :: ([QuarterBlock] -> [QuarterBlock]) -> Block -> Block
transform f block = B.concat (B.transpose (f (B.transpose (chunk 4 block))))

shiftBytes :: Block -> Block
shiftBytes = transform shiftRows

invShiftBytes :: Block -> Block
invShiftBytes = transform invShiftRows

extractBytes :: QuarterBlock -> Maybe [Word8]
extractBytes qb = do
  (x, xs) <- B.uncons qb
  (y, ys) <- B.uncons xs
  (z, zs) <- B.uncons ys
  (w, _) <- B.uncons zs
  return [x, y, z, w]

-- Row number -> Row -> filler byte
buildBlockFromQuarterRow :: Int -> QuarterBlock -> Word8 -> Block
buildBlockFromQuarterRow r qb f = B.append pre $ B.append qb post
  where
    pre = B.replicate (r * 4) f
    post = B.replicate ((3 - r) * 4) f

prop_shiftBytesR0 :: QuarterBlock -> Bool
prop_shiftBytesR0 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      shiftBytes
        (buildBlockFromQuarterRow 0 (B.pack [x, y, z, w]) filler)
        == buildBlockFromQuarterRow 0 (B.pack [x, y, z, w]) filler
    filler = 0x00

prop_shiftBytesR1 :: QuarterBlock -> Bool
prop_shiftBytesR1 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      shiftBytes
        (buildBlockFromQuarterRow 1 (B.pack [x, y, z, w]) filler)
        == buildBlockFromQuarterRow 1 (B.pack [y, z, w, x]) filler
    filler = 0x00

prop_shiftBytesR2 :: QuarterBlock -> Bool
prop_shiftBytesR2 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      shiftBytes
        (buildBlockFromQuarterRow 2 (B.pack [x, y, z, w]) filler)
        == buildBlockFromQuarterRow 2 (B.pack [z, w, x, y]) filler
    filler = 0x00

prop_shiftBytesR3 :: QuarterBlock -> Bool
prop_shiftBytesR3 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      shiftBytes
        (buildBlockFromQuarterRow 3 (B.pack [x, y, z, w]) filler)
        == buildBlockFromQuarterRow 3 (B.pack [w, x, y, z]) filler
    filler = 0x00

prop_invShiftBytesR0 :: QuarterBlock -> Bool
prop_invShiftBytesR0 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      invShiftBytes
        (buildBlockFromQuarterRow 0 (B.pack [x, y, z, w]) filler)
        == buildBlockFromQuarterRow 0 (B.pack [x, y, z, w]) filler
    filler = 0x00

prop_invShiftBytesR1 :: QuarterBlock -> Bool
prop_invShiftBytesR1 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      invShiftBytes
        (buildBlockFromQuarterRow 1 (B.pack [x, y, z, w]) filler)
        == buildBlockFromQuarterRow 1 (B.pack [w, x, y, z]) filler
    filler = 0x00

prop_invShiftBytesR2 :: QuarterBlock -> Bool
prop_invShiftBytesR2 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      invShiftBytes
        (buildBlockFromQuarterRow 2 (B.pack [x, y, z, w]) filler)
        == buildBlockFromQuarterRow 2 (B.pack [z, w, x, y]) filler
    filler = 0x00

prop_invShiftBytesR3 :: QuarterBlock -> Bool
prop_invShiftBytesR3 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      invShiftBytes
        (buildBlockFromQuarterRow 3 (B.pack [x, y, z, w]) filler)
        == buildBlockFromQuarterRow 3 (B.pack [y, z, w, x]) filler
    filler = 0x00

-- Sorted version of stateMat should be the same for input and output
prop_shiftBytesSortedSame :: Block -> Bool
prop_shiftBytesSortedSame b =
  B.sort b == B.sort (B.concat (shiftRows (chunk 4 b)))

prop_invShiftBytesSortedSame :: Block -> Bool
prop_invShiftBytesSortedSame b =
  B.sort b == B.sort (B.concat (invShiftRows (chunk 4 b)))

{- MixColumns
-------------------------------}
-- Unit tests based on wiki’s test vectors
{- These are sourced from wikipedia:
https://en.wikipedia.org/wiki/Rijndael_MixColumns
-}

testMixColumns :: Test
testMixColumns =
  TestList
    [ mixCloumns mixColumnsMatrix [B.pack [0xdb, 0x13, 0x53, 0x45]]
        ~?= B.pack [0x8e, 0x4d, 0xa1, 0xbc],
      mixCloumns mixColumnsMatrix [B.pack [0xf2, 0x0a, 0x22, 0x5c]]
        ~?= B.pack [0x9f, 0xdc, 0x58, 0x9d],
      mixCloumns mixColumnsMatrix [B.pack [0x01, 0x01, 0x01, 0x01]]
        ~?= B.pack [0x01, 0x01, 0x01, 0x01],
      mixCloumns mixColumnsMatrix [B.pack [0xc6, 0xc6, 0xc6, 0xc6]]
        ~?= B.pack [0xc6, 0xc6, 0xc6, 0xc6],
      mixCloumns mixColumnsMatrix [B.pack [0xd4, 0xd4, 0xd4, 0xd5]]
        ~?= B.pack [0xd5, 0xd5, 0xd7, 0xd6],
      mixCloumns mixColumnsMatrix [B.pack [0x2d, 0x26, 0x31, 0x4c]]
        ~?= B.pack [0x4d, 0x7e, 0xbd, 0xf8]
    ]

{- AddRoundKey
-------------------------------}

addRoundKey = xorByteString

zeroBlock :: Block
zeroBlock = buildBlockFromQuarterRow 0 (B.pack [0x00, 0x00, 0x00, 0x00]) 0x00

-- Something xored with itself is zero
prop_addRoundKeySelfIsZero :: Block -> Bool
prop_addRoundKeySelfIsZero b = addRoundKey b b == zeroBlock

{- Test overall encryption
-------------------------------}

encryptAES :: Block -> Key -> Block
encryptAES = getCipher

decryptAES :: Block -> Key -> Maybe Block
decryptAES = getPlainText

-- Encryption then decryption of anything should yield the original
prop_encThenDecIsOrig :: Block -> Key -> Bool
prop_encThenDecIsOrig s k =
  case decryptAES k (encryptAES k s) of
    Nothing -> False
    Just block -> block == s

-- All the tests in one convenient place:
quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs {maxSuccess = n}

tests :: IO ()
tests = do
  putStrLn "Unit tests:"
  _ <-
    runTestTT testMixColumns
  putStrLn "Quickcheck properties:"
  putStrLn "prop_blockLen"
  quickCheckN 500 prop_blockLen
  putStrLn "prop_subBytesHasAllBytes"
  quickCheckN 500 prop_subBytesHasAllBytes
  putStrLn "prop_subBytesAllSame"
  quickCheckN 500 prop_subBytesAllSame
  putStrLn "prop_invSubBytesHasAllBytes"
  quickCheckN 500 prop_invSubBytesHasAllBytes
  putStrLn "prop_invSubBytesAllSame"
  quickCheckN 500 prop_invSubBytesAllSame
  putStrLn "prop_shiftBytesR0"
  quickCheckN 500 prop_shiftBytesR0
  putStrLn "prop_shiftBytesR1"
  quickCheckN 500 prop_shiftBytesR1
  putStrLn "prop_shiftBytesR2"
  quickCheckN 500 prop_shiftBytesR2
  putStrLn "prop_shiftBytesR3"
  quickCheckN 500 prop_shiftBytesR3
  putStrLn "prop_invShiftBytesR0"
  quickCheckN 500 prop_invShiftBytesR0
  putStrLn "prop_invShiftBytesR1"
  quickCheckN 500 prop_invShiftBytesR1
  putStrLn "prop_invShiftBytesR2"
  quickCheckN 500 prop_invShiftBytesR2
  putStrLn "prop_invShiftBytesR3"
  quickCheckN 500 prop_invShiftBytesR3
  putStrLn "prop_shiftBytesSortedSame"
  quickCheckN 500 prop_shiftBytesSortedSame
  putStrLn "prop_invShiftBytesSortedSame"
  quickCheckN 500 prop_invShiftBytesSortedSame
  putStrLn "prop_addRoundKeySelfIsZero"
  quickCheckN 500 prop_addRoundKeySelfIsZero
  putStrLn "prop_encThenDecIsOrig"
  quickCheckN 500 prop_encThenDecIsOrig