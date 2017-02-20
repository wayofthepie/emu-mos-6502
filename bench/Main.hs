module Main where

import Criterion.Main

import Data.Int
import Data.Ord
import Data.List

searchTo = 1000000

nextNumber :: Int64 -> Int64
nextNumber n = case n `quotRem` 2 of
  (n2,0) -> n2
  _      -> 3 * n + 1

sequenceLength :: Int64 -> Int64
sequenceLength 1 = 1
sequenceLength n = 1 + (sequenceLength next)
    where next = nextNumber n

longestSequence s = maximumBy (comparing sequenceLength) [1..s]

-- | This is just a reminder as I keep forgetting how to do this,
-- not a real benchmark yet.
main :: IO ()
main = defaultMain [
  bgroup "status register"
    [
      bench "1" $ whnf longestSequence searchTo
    ]
  ]


