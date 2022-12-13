module Encrypt (getCipher, mixCloumns, shiftRows, shiftRowsHelper) where

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
import Utils
  ( Block,
    Key,
    QuarterBlock,
    chunk,
    displayHex,
    leftShiftBy,
    mapInd,
    stringToByteString,
    xorByteString,
  )

type Store = (Block, Maybe Key)

initStore :: Block -> Key -> Store
initStore block key = (xorByteString block key, Just key)

addRoundKey :: Block -> Key -> Block
addRoundKey = xorByteString

shiftRowsHelper ::
  (Int -> [Word8] -> [Word8]) ->
  [QuarterBlock] ->
  [QuarterBlock]
shiftRowsHelper shiftFunction rows = B.transpose (mapInd f (B.transpose rows))
  where
    f :: QuarterBlock -> Int -> QuarterBlock
    f b i = B.pack (shiftFunction i (B.unpack b))

shiftRows :: [QuarterBlock] -> [QuarterBlock]
shiftRows = shiftRowsHelper leftShiftBy

mixCloumns :: [[A.Array Word8 Word8]] -> [QuarterBlock] -> Block
mixCloumns matrix rows = B.concat (map (multiplyMatrix matrix) rows)
  where
    multiplyMatrix :: [[A.Array Word8 Word8]] -> QuarterBlock -> QuarterBlock
    multiplyMatrix matrix row = B.pack (map (multiplyRow row) matrix)
    multiplyRow :: QuarterBlock -> [A.Array Word8 Word8] -> Word8
    multiplyRow row matrixRow = combine (zipWith (!) matrixRow (B.unpack row))
    combine :: [Word8] -> Word8
    combine = foldr xor 0x00

getCipher :: Key -> Block -> Block
getCipher key block = evalState (encrypt 1) (initStore block key)
  where
    encrypt :: Int -> State Store Block
    encrypt round =
      do
        (block, key) <- get
        let subBlock = sbox block
        let diffusedBlock =
              case round of
                10 -> B.concat (shiftRows (chunk 4 subBlock))
                _ -> mixCloumns mixColumnsMatrix (shiftRows (chunk 4 subBlock))
        let newKey = generateKey round key
        let newBlock =
              case newKey of
                Just k -> xorByteString k diffusedBlock
                Nothing -> diffusedBlock
        put (newBlock, newKey)
        case round of
          10 -> return newBlock
          _ -> encrypt (round + 1)
