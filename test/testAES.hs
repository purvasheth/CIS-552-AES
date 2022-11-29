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

genNChars :: Int -> Gen String
genNChars n = replicateM n QC.arbitraryPrintableChar

genBlock :: Gen Block
genBlock = Char8.pack <$> genNChars 16

gen4Bytes :: Gen B.ByteString
gen4Bytes = Char8.pack <$> genNChars 4

-- confirm length of statemat is always 16 bytes
prop_len16 :: Block -> Bool
prop_len16 s = B.length s == 16

{- SubBytes
-------------------------------}
-- TODO remove fake func
subBytes = undefined

subBytesLUT :: Map Word8 Word8
subBytesLUT = undefined

-- all bytes have an entry in LUT
prop_subBytesHasAllBytes :: Word8 -> Bool
prop_subBytesHasAllBytes w8 = isJust $ Map.lookup w8 subBytesLUT

-- Fill stateMat with one arbitrary value, make sure the output is all the same
prop_subBytesAllSame :: Word8 -> Bool
prop_subBytesAllSame w8 = case Map.lookup w8 subBytesLUT of
  Nothing -> False
  Just x -> B.all (== x) cypher
  where
    plain :: Block
    plain = B.replicate 16 w8
    cypher :: Block
    cypher = subBytes plain

-- Fill stateMat with one arbitrary value, make sure the output is all correct

{- ShiftRows
-------------------------------}
-- TODO remove fake func
shiftBytes = undefined

{- Create a stateMat with only one row with unique entries. Then isolate the
shift of that one row and confirm it is correct. I.e.
shiftBytesR2([[1,1,1,1],[1,1,1,1],[x, y, z, w],[1,1,1,1]]) ==
             [[1,1,1,1],[1,1,1,1],[z, w, x, y],[1,1,1,1]]
-}

-- Sorted version of stateMat should be the same for input and output

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
