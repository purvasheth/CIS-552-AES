module Decrypt where

import Control.Monad.State (State, StateT, evalState, evalStateT, get, put)
import Data.Array ((!))
import Data.Array qualified as A
import Data.Bits (Bits (bit), xor)
import Data.ByteString (empty)
import Data.ByteString qualified as B
import Data.Char (chr, ord)
import Data.Map qualified as Map
import Data.Word (Word8)
import Encrypt (Block, Key, QuarterBlock)
import KeyExpansion (generateKey)
import MixColumns (invMixColumnsMatrix, mixColumnsMatrix)
import SBox (invSbox, sbox)
import Utils (chunk, displayHex, mapInd, rightShiftBy, stringToByteString, xorByteString)

type Store = Maybe Block

block :: Block
block = stringToByteString ")\195P_W\DC4 \246@\"\153\179\SUB\STX\215:"

keys :: [Maybe Key]
keys =
  [ Just . stringToByteString $ "(\253\222\248m\164$J\204\192\164\254;1o&",
    Just . stringToByteString $ "\191\226\191\144EY\250\178\161d\128\180\247\241\203\216",
    Just . stringToByteString $ "\142Q\239!\250\187E\"\228=z\ACKV\149Kl",
    Just . stringToByteString $ "\204\150\237\SYNt\234\170\ETX\RS\134?$\178\168\&1j",
    Just . stringToByteString $ "\189=\194\135\184|G\NAKjl\149'\172.\SON",
    Just . stringToByteString $ "\177);3\ENQA\133\146\210\DLE\210\&2\198B\155i",
    Just . stringToByteString $ "\161\DC2\STX\201\180h\190\161\215QW\160\DC4RI[",
    Just . stringToByteString $ "\210`\r\231\NAKz\188hc9\233\SOH\195\ETX\RS\251",
    Just . stringToByteString $ "V\b \a\199\SUB\177\143vCUi\160:\247\250",
    Just . stringToByteString $ "\226\&2\252\241\145\DC2\145\136\177Y\228\230\214y\162\147"
  ]
    ++ [Just key]

key :: Key
key = stringToByteString "Thats my Kung Fu"

initStore :: [Maybe Key] -> Block -> Store
initStore [] b = Just b
initStore (x : xs) b = do
  key <- x
  return (xorByteString key b)

invShiftRows :: [QuarterBlock] -> [QuarterBlock]
invShiftRows rows = B.transpose (mapInd f (B.transpose rows))
  where
    f :: QuarterBlock -> Int -> QuarterBlock
    f b i = B.pack (rightShiftBy i (B.unpack b))

invMixCloumns :: [[A.Array Word8 Word8]] -> [QuarterBlock] -> Block
invMixCloumns matrix rows = B.concat (map (multiplyMatrix matrix) rows)
  where
    multiplyMatrix :: [[A.Array Word8 Word8]] -> QuarterBlock -> QuarterBlock
    multiplyMatrix matrix row = B.pack (map (multiplyRow row) matrix)
    multiplyRow :: QuarterBlock -> [A.Array Word8 Word8] -> Word8
    multiplyRow row matrixRow = combine (zipWith (!) matrixRow (B.unpack row))
    combine :: [Word8] -> Word8
    combine = foldr xor 0x00

addRoundKey :: Maybe Block -> Maybe Key -> Maybe Block
addRoundKey block key = do
  b <- block
  xorByteString b <$> key

-- add round

plainText :: Maybe Block
plainText = evalState (decrypt (drop 1 keys)) (initStore keys block)
  where
    decrypt :: [Maybe Key] -> State Store (Maybe Block)
    decrypt [] = do get
    decrypt (key : keys) =
      do
        block <- get
        let newBlock =
              case block of
                Nothing -> Nothing
                Just b ->
                  case keys of
                    [] -> Just $ B.concat (invShiftRows (chunk 4 (invSbox b)))
                    _ -> Just $ invMixCloumns invMixColumnsMatrix (invShiftRows (chunk 4 (invSbox b)))
        put (addRoundKey newBlock key)
        decrypt keys

-- >>> plainText
-- Just "4\250j\NAK\241_L\161\139\CAN\vSn\172\&1\190"
