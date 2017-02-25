{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Cpu.InstructionSpec where

import Data.Bits
import Data.Maybe (fromJust)
import GHC.Word
import Numeric (showHex)

import Cpu.Instruction

import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

spec :: Spec
spec = do
  instructionInvariantsSpec


-- Construct instruction info from the given size 'size', cycles 'cycles' and
-- extra cycles 'oops'.
ii :: Word8 -> Int -> Int -> Int -> InstructionInfo
ii opCode size cycles oops = InstructionInfo
  { _opCode = opCode
  , _size = size
  , _cycles = cycles
  , _oops = oops
  }


instructionInvariantsSpec = do
  describe "instructionInfo" $ do
    -- LDA
    it "should build an InstructionInfo value with correct constraints" $
      instructionInfo (Instruction LDA Immediate :: OpBuild 0xA9)  `shouldBe` (ii 0xA9 2 2 0)

