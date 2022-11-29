import Control.Monad (replicateM)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as Char8 --qualified (pack)
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Word (Word8)
import System.Random (genByteString)
import Test.QuickCheck
  ( Gen,
    PrintableString,
    arbitrary,
    getPrintableString,
  )
import Test.QuickCheck qualified as QC

------- Enable generation of arbitrary blocks ------
type Block = B.ByteString

type QuarterBlock = B.ByteString

genNChars :: Int -> Gen String
genNChars n = replicateM n QC.arbitraryPrintableChar

genBlock :: Gen Block
genBlock = Char8.pack <$> genNChars 16

gen4Bytes :: Gen B.ByteString
gen4Bytes = Char8.pack <$> genNChars 4

-- confirm length of a block is always 16 bytes
prop_blockLen :: Block -> Bool
prop_blockLen s = B.length s == 16

-- confirm length of a quarterblock is always 4 bytes
prop_quarterBlockLen :: QuarterBlock -> Bool
prop_quarterBlockLen s = B.length s == 4

{- SubBytes
-------------------------------}
-- TODO remove fake func
subBytes = undefined

subBytesLUT :: Map Word8 Word8
subBytesLUT = undefined

-- all bytes have an entry in LUT
prop_subBytesHasAllBytes :: Word8 -> Bool
prop_subBytesHasAllBytes w8 = isJust $ Map.lookup w8 subBytesLUT

-- Fill stateMat with one arbitrary value, make sure the output is all correct
prop_subBytesAllSame :: Word8 -> Bool
prop_subBytesAllSame w8 = case Map.lookup w8 subBytesLUT of
  Nothing -> False
  Just x -> B.all (== x) cypher
  where
    plain :: Block
    plain = B.replicate 16 w8
    cypher :: Block
    cypher = subBytes plain

{- ShiftRows
-------------------------------}
-- TODO remove fake func
shiftBytes = undefined

{- Create a stateMat with only one row with unique entries. Then isolate the
shift of that one row and confirm it is correct. I.e.
shiftBytesR2([[1,1,1,1],[1,1,1,1],[x, y, z, w],[1,1,1,1]]) ==
             [[1,1,1,1],[1,1,1,1],[z, w, x, y],[1,1,1,1]]
-}

extractBytes :: QuarterBlock -> Maybe [Word8]
extractBytes qb = do
  (x, xs) <- B.uncons qb
  (y, ys) <- B.uncons xs
  (z, zs) <- B.uncons ys
  (w, _) <- B.uncons zs
  return [x, y, z, w]

buildBlockFromQuarterRow :: Int -> Block -> Block
buildBlockFromQuarterRow r qb = B.append pre $ B.append qb post
  where
    pre = B.replicate (r * 4) 0x00
    post = B.replicate ((3 - r) * 4) 0x00

prop_shiftBytesR0 :: QuarterBlock -> Bool
prop_shiftBytesR0 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      shiftBytes (buildBlockFromQuarterRow 0 (B.pack [x, y, z, w]))
        == buildBlockFromQuarterRow 0 (B.pack [x, y, z, w])

prop_shiftBytesR1 :: QuarterBlock -> Bool
prop_shiftBytesR1 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      shiftBytes (buildBlockFromQuarterRow 1 (B.pack [x, y, z, w]))
        == buildBlockFromQuarterRow 1 (B.pack [y, z, w, x])

prop_shiftBytesR2 :: QuarterBlock -> Bool
prop_shiftBytesR2 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      shiftBytes (buildBlockFromQuarterRow 2 (B.pack [x, y, z, w]))
        == buildBlockFromQuarterRow 2 (B.pack [z, w, x, y])

prop_shiftBytesR3 :: QuarterBlock -> Bool
prop_shiftBytesR3 qb = case extractBytes qb of
  Nothing -> False
  Just [x, y, z, w] -> aux x y z w
  _ -> False
  where
    aux :: Word8 -> Word8 -> Word8 -> Word8 -> Bool
    aux x y z w =
      shiftBytes (buildBlockFromQuarterRow 3 (B.pack [x, y, z, w]))
        == buildBlockFromQuarterRow 3 (B.pack [w, x, y, z])

-- Sorted version of stateMat should be the same for input and output
prop_shiftBytesSortedSame :: Block -> Bool
prop_shiftBytesSortedSame b = B.sort b == B.sort (shiftBytes b)

{- MixColumns
-------------------------------}
-- TODO remove fake func
mixColumns = undefined

-- Unit tests based on wiki’s test vectors

-- Isolate one columns and test arbitrary values

{- AddRoundKey
-------------------------------}
-- TODO remove fake func
addRoundKey = undefined

-- Unit tests for XOR (0000, 1111, etc.)

-- Something xored with itself is zero
