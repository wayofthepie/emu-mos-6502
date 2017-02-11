module Main where

import Cpu
import Criterion.Main


main :: IO ()
main = defaultMain [
  bgroup "status register"
    [
      bench "1" $ whnf (set Sign) initStatusRegister
    ]
  ]


