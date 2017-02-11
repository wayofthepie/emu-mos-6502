module Main where

import Cpu
import Criterion.Main

-- | This is just a reminder as I keep forgetting how to do this,
-- not a real benchmark yet.
main :: IO ()
main = defaultMain [
  bgroup "status register"
    [
      bench "1" $ whnf (setFlag Sign) (initStatus 0x00)
    ]
  ]


