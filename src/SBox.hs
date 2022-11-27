module SBox where

import Data.Map qualified as Map
import Lib (Hex (..), Word8 (..))

sbox :: Map.Map Word8 Word8
sbox = Map.fromList [(W8 (H0, H1), W8 (H0, H2))]

-- >>> Map.lookup (W8 (H0, H1)) sbox
-- Just (W8 (H0,H2))
