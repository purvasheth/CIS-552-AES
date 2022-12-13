module Decrypt (getPlainText, invShiftRows) where

import Control.Monad.State (State, StateT, evalState, evalStateT, get, put)
import Data.Array ((!))
import Data.Array qualified as A
import Data.Bits (Bits (bit), xor)
import Data.ByteString (empty)
import Data.ByteString qualified as B
import Data.Char (chr, ord)
import Data.Map qualified as Map
import Data.Word (Word8)
import Encrypt (getCipher, mixCloumns)
import KeyExpansion (generateAllKeys, generateKey)
import MixColumns (invMixColumnsMatrix, mixColumnsMatrix)
import SBox (invSbox)
import Utils
  ( Block,
    Key,
    QuarterBlock,
    chunk,
    displayHex,
    getBlocks,
    getString,
    mapInd,
    rightShiftBy,
    stringToByteString,
    xorByteString,
  )

type Store = Maybe Block

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

addRoundKey :: Maybe Block -> Maybe Key -> Maybe Block
addRoundKey block key = do
  b <- block
  xorByteString b <$> key

transformKeys :: [Maybe Key] -> [Maybe Key]
transformKeys = map transform
  where
    transform key = do
      mixCloumns invMixColumnsMatrix . chunk 4 <$> key

getPlainText :: Key -> Block -> Maybe Block
getPlainText k cipher =
  let keys = generateAllKeys (Just k)
   in evalState (decrypt (drop 1 keys ++ [Just k])) (initStore keys cipher)
  where
    decrypt :: [Maybe Key] -> State Store (Maybe Block)
    decrypt [] = do get
    decrypt (key : keys) =
      do
        block <- get
        let newBlock =
              case block of
                Nothing -> Nothing
                Just b -> Just $ invSbox (B.concat (invShiftRows (chunk 4 b)))
        let finalBlock =
              case addRoundKey newBlock key of
                Nothing -> Nothing
                Just b ->
                  if key == Just k
                    then Just b
                    else Just $ mixCloumns invMixColumnsMatrix (chunk 4 b)
        put finalBlock
        decrypt keys

-- Example
str :: String
str = "long string which is the message to be sent"

key :: Key
key = stringToByteString "some key 16 bits"

-- could also use concat on the encrypt side and chunk on the decrypt side
-- >>> getString (map (getPlainText key) (map (getCipher key) (getBlocks str)))
-- Just "long string which is the message to be sent"
