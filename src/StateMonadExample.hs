module StateMonandExample where

import Control.Monad.State (State, StateT, evalState, evalStateT, get, modify, put)
import Data.Bits (xor)
import Data.ByteString (empty)
import Data.ByteString qualified as B
import Data.Char (chr, ord)
import Data.Map qualified as Map
import Data.Word (Word8)
import Lib (chunk, displayHex, mapInd, shiftBy, stringToByteString, xorByteString)
import SBox (sbox)

type Store = (B.ByteString, B.ByteString, Int)

key :: B.ByteString
key = stringToByteString "Thats my Kung Fu"

block :: B.ByteString
block = stringToByteString "Two One Nine Two"

initStore :: Store
initStore = (block, key, 0)

addRoundKey :: B.ByteString -> B.ByteString -> B.ByteString
addRoundKey = xorByteString

shiftRows :: [B.ByteString] -> B.ByteString
shiftRows rows = B.concat (mapInd f rows)
  where
    f :: B.ByteString -> Int -> B.ByteString
    f b i = B.pack (shiftBy i (B.unpack b))

aes :: B.ByteString -- cipher
aes = evalState aesHelper initStore
  where
    aesHelper :: State Store B.ByteString
    aesHelper =
      do
        (block, key, round) <- get
        let newBlock = sbox (addRoundKey block key)
        return (shiftRows (chunk 4 newBlock))

-- >>> displayHex aes
-- ["63","c0","ab","20","2f","30","cb","eb","af","2b","9f","93","a2","a0","92","c7"]
