module Encrypt (Key, Block, QuarterBlock) where

import Control.Monad.State (State, StateT, evalState, evalStateT, get, put)
import Data.Array ((!))
import Data.Array qualified as A
import Data.Bits (xor)
import Data.ByteString (empty)
import Data.ByteString qualified as B
import Data.Char (chr, ord)
import Data.Map qualified as Map
import Data.Word (Word8)
import KeyExpansion (Key, generateKey)
import MixColumns (mixColumnsMatrix)
import SBox (sbox)
import Utils (chunk, displayHex, mapInd, shiftBy, stringToByteString, xorByteString)

type RoundKey = B.ByteString

type Block = B.ByteString

type QuarterBlock = B.ByteString

type Store = (Block, Maybe Key, [Maybe Key])

key :: Key
key = stringToByteString "Thats my Kung Fu"

block :: Block
block = stringToByteString "Two One Nine Two"

initStore :: Store
initStore = (xorByteString block key, Just key, [])

addRoundKey :: Block -> Key -> Block
addRoundKey = xorByteString

shiftRows :: [QuarterBlock] -> [QuarterBlock]
shiftRows rows = B.transpose (mapInd f (B.transpose rows))
  where
    f :: QuarterBlock -> Int -> QuarterBlock
    f b i = B.pack (shiftBy i (B.unpack b))

mixCloumns :: [[A.Array Word8 Word8]] -> [QuarterBlock] -> Block
mixCloumns matrix rows = B.concat (map (multiplyMatrix matrix) rows)
  where
    multiplyMatrix :: [[A.Array Word8 Word8]] -> QuarterBlock -> QuarterBlock
    multiplyMatrix matrix row = B.pack (map (multiplyRow row) matrix)
    multiplyRow :: QuarterBlock -> [A.Array Word8 Word8] -> Word8
    multiplyRow row matrixRow = combine (zipWith (!) matrixRow (B.unpack row))
    combine :: [Word8] -> Word8
    combine = foldr xor 0x00

cipher :: (Block, [Maybe Key])
cipher = evalState (encrypt 1) initStore
  where
    encrypt :: Int -> State Store (Block, [Maybe Key])
    encrypt round =
      do
        (block, key, keys) <- get
        let subBlock = sbox block
        let diffusedBlock =
              case round of
                10 -> B.concat (shiftRows (chunk 4 subBlock))
                _ -> mixCloumns mixColumnsMatrix (shiftRows (chunk 4 subBlock))
        let newKey =
              case key of
                Just k -> generateKey round k
                Nothing -> Nothing
        let newBlock =
              case newKey of
                Just k -> xorByteString k diffusedBlock
                Nothing -> diffusedBlock
        put (newBlock, newKey, newKey : keys)
        case round of
          10 -> return (newBlock, newKey : keys)
          _ -> encrypt (round + 1)

-- >>> (fst cipher)
-- ")\195P_W\DC4 \246@\"\153\179\SUB\STX\215:"

-- >>> (snd cipher)
-- [Just "(\253\222\248m\164$J\204\192\164\254;1o&",Just "\191\226\191\144EY\250\178\161d\128\180\247\241\203\216",Just "\142Q\239!\250\187E\"\228=z\ACKV\149Kl",Just "\204\150\237\SYNt\234\170\ETX\RS\134?$\178\168\&1j",Just "\189=\194\135\184|G\NAKjl\149'\172.\SON",Just "\177);3\ENQA\133\146\210\DLE\210\&2\198B\155i",Just "\161\DC2\STX\201\180h\190\161\215QW\160\DC4RI[",Just "\210`\r\231\NAKz\188hc9\233\SOH\195\ETX\RS\251",Just "V\b \a\199\SUB\177\143vCUi\160:\247\250",Just "\226\&2\252\241\145\DC2\145\136\177Y\228\230\214y\162\147"]
