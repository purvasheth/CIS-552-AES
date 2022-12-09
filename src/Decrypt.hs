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
import Encrypt (Block, Key, QuarterBlock, cipher, mixCloumns)
import KeyExpansion (generateKey)
import MixColumns (invMixColumnsMatrix, mixColumnsMatrix)
import SBox (invSbox, sbox)
import Utils (chunk, displayHex, mapInd, rightShiftBy, stringToByteString, xorByteString)

type Store = Maybe Block

block :: Block
block = fst cipher

ogKey :: Key
ogKey = stringToByteString "Thats my Kung Fu"

keys :: [Maybe Key]
keys = snd cipher

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

-- plainText :: Maybe Block
-- plainText = evalState (decrypt (transformKeys (drop 1 keys) ++ [Just ogKey])) (initStore keys block)
--   where
--     decrypt :: [Maybe Key] -> State Store (Maybe Block)
--     decrypt [] = do get
--     decrypt (key : keys) =
--       do
--         block <- get
--         let newBlock =
--               case block of
--                 Nothing -> Nothing
--                 Just b ->
--                   case keys of
--                     [] -> Just $ B.concat (invShiftRows (chunk 4 (invSbox b)))
--                     _ -> Just $ mixCloumns invMixColumnsMatrix (invShiftRows (chunk 4 (invSbox b)))
--         put (addRoundKey newBlock key)
--         decrypt keys

display :: Maybe Block -> Maybe [String]
display b = do
  displayHex <$> b

ogBlock :: Block
ogBlock = stringToByteString "Two One Nine Two"

-- >>> displayHex ogBlock
-- ["54","77","6f","20","4f","6e","65","20","4e","69","6e","65","20","54","77","6f"]

-- >>> display plainText
-- Just ["54","77","6f","20","4f","6e","65","20","4e","69","6e","65","20","54","77","6f"]

-- plainText :: Maybe Block
-- plainText = evalState (decrypt (drop 1 keys++[Just ogKey])) (initStore keys block)
--   where
--     decrypt :: [Maybe Key] -> State Store (Maybe Block)
--     decrypt [] = do get
--     decrypt (key : keys) =
--       do
--         block <- get
--         let newBlock =
--               case block of
--                 Nothing -> Nothing
--                 Just b -> Just $ invSbox (B.concat (invShiftRows (chunk 4 b)))
--         let finalBlock =
--               case addRoundKey newBlock key of
--                 Nothing -> Nothing
--                 Just b ->
--                   if key == Just ogKey
--                     then Just b
--                     else Just $ mixCloumns invMixColumnsMatrix (chunk 4 b)
--         put finalBlock
--         decrypt keys
