import Data.Bits (xor)
import Data.ByteString qualified as B
import Data.ByteString.Char8
import Data.Char (chr, ord)
import Data.Word (Word8)
import Numeric (showHex)

{-
pack an array of ints* into a byte string:
* these are actually implicitly casted to word8, because ByteStrings
are an array of Word8s
-}
foo :: B.ByteString
foo = B.pack [0xde, 0xad, 0xbe, 0xef]

-- >>> foo
-- "\222\173\190\239"

-- unpack a bytestring to an array of Word8s
foo' = B.unpack foo

-- >>> foo'
-- [222,173,190,239]

-- map a function over the bytestring
bar :: B.ByteString
bar = B.map (`xor` 0x32) foo

-- >>> bar
-- "\236\159\140\221"

-- see bytestring in (slightly more) human readable form
bsToHex :: B.ByteString -> [String]
bsToHex bs = map (`showHex` "") (B.unpack bs)

-- >>> bsToHex bar
-- ["ec","9f","8c","dd"]

-------
-- Dealing with ascii
-------

-- ord converts from char to int
msgInInts :: [Int]
msgInInts = map ord "hello world!"

-- >>> msgInInts
-- [104,101,108,108,111,32,119,111,114,108,100,33]

-- chr converts from int to an ascii char
msg :: String
msg = map chr msgInInts

-- >>> msg
-- "hello world!"

-- integers are 32 bit, we have to explicitly convert between Ints and word8
-- fromIntegral accomplishes this, but I wrote this because the name isn't obvious
intToByte :: Integer -> Word8
intToByte = fromIntegral

-- converting from a bytestring to ascii looks like it wouldn't be too
-- involved because Show shows us the ascii values of the internal bytes
-- >>> B.pack (map fromIntegral msgInInts) :: B.ByteString
-- "hello world!"

-- but it can't actually be treated as a string
-- >>> B.pack (map fromIntegral msgInInts) :: String
-- Couldn't match type ‘ByteString’ with ‘[Char]’
-- Expected: String
--   Actual: ByteString

-- this converts a bytestring to an ascii string (not an array of hex digits)
bsToAsciiTheLongWay :: B.ByteString -> String
bsToAsciiTheLongWay bs = map (chr . fromIntegral) (B.unpack bs)

-- >>> bsToAscii (B.pack (map fromIntegral msgInInts))
-- "hello world!"
