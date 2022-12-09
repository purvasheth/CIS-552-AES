module Encrypt (Key, Block, QuarterBlock, cipher, mixCloumns) where

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

-- >>> displayHex (fst cipher)
-- ["29","c3","50","5f","57","14","20","f6","40","22","99","b3","1a","2","d7","3a"]

display x = do
  displayHex <$> x

-- >>> map display (snd cipher)
-- [Just ["28","fd","de","f8","6d","a4","24","4a","cc","c0","a4","fe","3b","31","6f","26"],Just ["bf","e2","bf","90","45","59","fa","b2","a1","64","80","b4","f7","f1","cb","d8"],Just ["8e","51","ef","21","fa","bb","45","22","e4","3d","7a","6","56","95","4b","6c"],Just ["cc","96","ed","16","74","ea","aa","3","1e","86","3f","24","b2","a8","31","6a"],Just ["bd","3d","c2","87","b8","7c","47","15","6a","6c","95","27","ac","2e","e","4e"],Just ["b1","29","3b","33","5","41","85","92","d2","10","d2","32","c6","42","9b","69"],Just ["a1","12","2","c9","b4","68","be","a1","d7","51","57","a0","14","52","49","5b"],Just ["d2","60","d","e7","15","7a","bc","68","63","39","e9","1","c3","3","1e","fb"],Just ["56","8","20","7","c7","1a","b1","8f","76","43","55","69","a0","3a","f7","fa"],Just ["e2","32","fc","f1","91","12","91","88","b1","59","e4","e6","d6","79","a2","93"]]
