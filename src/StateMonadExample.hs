module StateMonandExample where

import Control.Monad.State (State, StateT, evalState, evalStateT, get, modify, put)
import Lib (Block (..), Hex (..), Key128 (..), Word32 (W32), Word8 (..), xorWord32)

type Store = (Block, Key128)

initStore :: Store
initStore =
  ( B
      ( W32 (W8 (H5, H4), W8 (H7, H7), W8 (H6, HF), W8 (H2, H0)),
        W32 (W8 (H4, HF), W8 (H6, HE), W8 (H6, H5), W8 (H2, H0)),
        W32 (W8 (H4, HE), W8 (H6, H9), W8 (H6, HE), W8 (H6, H5)),
        W32 (W8 (H2, H0), W8 (H5, H4), W8 (H7, H7), W8 (H6, HF))
      ),
    K128
      ( W32 (W8 (H5, H4), W8 (H6, H8), W8 (H6, H1), W8 (H7, H4)),
        W32 (W8 (H7, H3), W8 (H2, H0), W8 (H6, HD), W8 (H7, H9)),
        W32 (W8 (H2, H0), W8 (H4, HB), W8 (H7, H5), W8 (H6, HE)),
        W32 (W8 (H6, H7), W8 (H2, H0), W8 (H4, H6), W8 (H7, H5))
      )
  )

addRoundKey :: Block -> Key128 -> Maybe Block
addRoundKey (B (b1, b2, b3, b4)) (K128 (k1, k2, k3, k4)) =
  do
    nb1 <- xorWord32 b1 k1
    nb2 <- xorWord32 b2 k2
    nb3 <- xorWord32 b3 k3
    nb4 <- xorWord32 b4 k4
    return (B (nb1, nb2, nb3, nb4))

aes :: Maybe Block -- cipher
aes = evalState aesHelper initStore
  where
    aesHelper :: State Store (Maybe Block)
    aesHelper =
      do
        (block@(B (b1, b2, b3, b4)), key@(K128 (k1, k2, k3, k4))) <- get
        return (addRoundKey block key)

-- >>> aes
-- Just (B (W32 (W8 (H0,H0),W8 (H1,HF),W8 (H0,HE),W8 (H5,H4)),W32 (W8 (H3,HC),W8 (H4,HE),W8 (H0,H8),W8 (H5,H9)),W32 (W8 (H6,HE),W8 (H2,H2),W8 (H1,HB),W8 (H0,HB)),W32 (W8 (H4,H7),W8 (H7,H4),W8 (H3,H1),W8 (H1,HA))))
