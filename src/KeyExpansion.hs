module KeyExpansion where

import Lib (Key128 (..))
import Test.HUnit
  ( Counts,
    Test (TestList),
    runTestTT,
    (~?),
    (~?=),
  )

-- left circular shift
rotateWord :: Key128 -> Key128
rotateWord (K128 (w1, w2, w3, w4)) = K128 (w2, w3, w4, w1)


