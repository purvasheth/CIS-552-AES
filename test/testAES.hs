import Control.Monad (replicateM)
import Data.Bits (xor)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as Char8 --qualified (pack)
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Word (Word8)
import GHC.Base (build)
import System.Random (genByteString)
import Test.QuickCheck
  ( Gen,
    PrintableString,
    arbitrary,
    getPrintableString,
  )
import Test.QuickCheck qualified as QC

------- Enable generation of arbitrary blocks ------
type Key = B.ByteString

type RoundKey = B.ByteString

type Block = B.ByteString

type QuarterBlock = B.ByteString

genNChars :: Int -> Gen String
genNChars n = replicateM n QC.arbitraryPrintableChar

genBlock :: Gen Block
genBlock = Char8.pack <$> genNChars 16

-- generate a 128 bit key
genKey128 :: Gen Key
genKey128 = Char8.pack <$> genNChars 16

-- generate 128 bit Round Key
genRoundKey :: Gen Key
genRoundKey = genKey128

-- For generating rows or columns
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
      shiftBytes (buildBlockFromQuarterRow 0 (B.pack [x, y, z, w])) filler
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
      shiftBytes (buildBlockFromQuarterRow 1 (B.pack [x, y, z, w])) filler
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
      shiftBytes (buildBlockFromQuarterRow 2 (B.pack [x, y, z, w])) filler
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
      shiftBytes (buildBlockFromQuarterRow 3 (B.pack [x, y, z, w])) filler
        == buildBlockFromQuarterRow 3 (B.pack [w, x, y, z]) filler
    filler = 0x00

-- Sorted version of stateMat should be the same for input and output
prop_shiftBytesSortedSame :: Block -> Bool
prop_shiftBytesSortedSame b = B.sort b == B.sort (shiftBytes b)

{- MixColumns
-------------------------------}
-- TODO remove fake func
mixColumns = undefined

-- functions to work with quarter blocks

-- column number -> col -> filler
buildBlockFromQuarterCol :: Int -> QuarterBlock -> Word8 -> Block
buildBlockFromQuarterCol r qb f = case extractBytes qb of
  Just [x, y, z, w] ->
    B.concat
      [ buildRow r x,
        buildRow r y,
        buildRow r z,
        buildRow r w
      ]
  _ -> undefined -- quarter block is invalid!
  where
    buildRow :: Int -> Word8 -> B.ByteString
    -- build row number 'r'. e.x. for row 1 there is
    -- one filler, the value, then three fillers
    buildRow r v = B.append (B.replicate r f) $ B.cons v (B.replicate (3 - r) f)

-- Unit tests based on wiki’s test vectors
{- These are sourced from wikipedia:
https://en.wikipedia.org/wiki/Rijndael_MixColumns
-}
knownValidPairs =
  [ ([0xdb, 0x13, 0x53, 0x45], [0x8e, 0x4d, 0xa1, 0xbc]),
    ([0xf2, 0x0a, 0x22, 0x5c], [0x9f, 0xdc, 0x58, 0x9d]),
    ([0x01, 0x01, 0x01, 0x01], [0x01, 0x01, 0x01, 0x01]),
    ([0xc6, 0xc6, 0xc6, 0xc6], [0xc6, 0xc6, 0xc6, 0xc6]),
    ([0xd4, 0xd4, 0xd4, 0xd5], [0xd5, 0xd5, 0xd7, 0xd6]),
    ([0x2d, 0x26, 0x31, 0x4c], [0x4d, 0x7e, 0xbd, 0xf8])
  ]

mixColumnsMatchesAtRow :: Int -> QuarterBlock -> QuarterBlock -> Bool
-- TODO extract column and verify it matches rather than compare whole block
mixColumnsMatchesAtRow i qb1 qb2 =
  mixColumns (buildBlockFromQuarterCol i qb1 filler)
    == buildBlockFromQuarterCol i qb2 filler
  where
    filler = 0x00

testAllKnownPairsEachRow :: Bool
-- for each known valid pair, build four blocks such that
-- the pair are in one column. Test that the function applied
-- to one block yields the other
testAllKnownPairsEachRow = and $ do
  (qb1, qb2) <- knownValidPairs
  i <- [0 .. 4]
  return $ mixColumnsMatchesAtRow i (B.pack qb1) (B.pack qb2)

-- Isolate one columns and test arbitrary values

{- AddRoundKey
-------------------------------}
-- TODO remove fake func
addRoundKey :: Block -> RoundKey -> Block
addRoundKey = undefined

zeroBlock :: Block
zeroBlock = buildBlockFromQuarterRow 0 (B.pack [0x00, 0x00, 0x00, 0x00]) 0x00

zeroRoundKey :: RoundKey
zeroRoundKey = zeroBlock

onesBlock :: Block
onesBlock = buildBlockFromQuarterRow 0 (B.pack [0xFF, 0xFF, 0xFF, 0xFF]) 0xFF

onesRoundKey :: RoundKey
onesRoundKey = onesBlock

-- Unit tests for XOR (0000, 1111, etc.)
testAddRoundKeyZeros :: Block -> Bool
testAddRoundKeyZeros b = addRoundKey b zeroRoundKey == b

-- testAddRoundKeyOnes :: Block -> Bool
-- testAddRoundKeyOnes b = addRoundKey b onesRoundKey == (`xor` 0xFF) <$> b

-- Something xored with itself is zero
prop_addRoundKeySelfIsZero :: Block -> Bool
prop_addRoundKeySelfIsZero b = addRoundKey b b == zeroBlock

{- Test overall encryption
-------------------------------}
-- TODO remove fake function

encryptAES :: String -> Key -> B.ByteString
encryptAES = undefined

decryptAES :: B.ByteString -> Key -> String
decryptAES = undefined

-- Encryption then decryption of anything should yield the original
prop_encThenDecIsOrig :: String -> Key -> Bool
prop_encThenDecIsOrig s k = decryptAES (encryptAES s k) k == s