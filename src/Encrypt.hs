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
import KeyExpansion (generateKey)
import MixColumns (mixColumnsMatrix)
import SBox (sbox)
import Utils (chunk, displayHex, mapInd, shiftBy, stringToByteString, xorByteString)

type Key = B.ByteString

type RoundKey = B.ByteString

type Block = B.ByteString

type QuarterBlock = B.ByteString

type Store = (Block, Maybe Key)

key :: Key
key = stringToByteString "Thats my Kung Fu"

block :: Block
block = stringToByteString "Two One Nine Two"

initStore :: Store
initStore = (block, Just key)

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

aes :: Block -- cipher
aes = evalState (aesHelper 0) initStore
  where
    aesHelper :: Int -> State Store Block
    aesHelper round =
      do
        (block, key) <- get
        let subBlock =
              case (round, key) of
                (0, Just k) -> sbox (addRoundKey block k)
                _ -> sbox block
        let diffusedBlock =
              mixCloumns mixColumnsMatrix (shiftRows (chunk 4 subBlock))
        let newKey =
              case key of
                Just k -> generateKey (round + 1) k
                Nothing -> Nothing
        let newBlock =
              case newKey of
                Just k -> xorByteString k diffusedBlock
                Nothing -> diffusedBlock
        put (newBlock, newKey)
        case round + 1 of
          10 -> return newBlock
          _ -> aesHelper (round + 1)

-- >>> displayHex aes
-- ["58","47","8","8b","15","b6","1c","ba","59","d4","e2","e8","cd","39","df","ce"]
