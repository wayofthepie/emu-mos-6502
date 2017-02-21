{-# LANGUAGE DataKinds #-}
module Main where

import Criterion.Main

import Data.Int
import Data.Ord
import Data.List

import Executor
import Cpu.Instruction


main :: IO ()
main = defaultMain [
    bgroup "exec"
      [
        bench "1" $ whnf execute 0x00
      ]
  ]


