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

-- Makes it easier to create common functions
it' = uncurry it

-- Construct an instruction info from the given operand size 'osize', cycles 'cycles' and
-- extra cycles 'oops'.
ii :: Word8 -> Int -> Int -> Int -> InstructionInfo
ii opCode osize cycles oops = InstructionInfo
  { _opCode = opCode
  , _operandSize = osize
  , _cycles = cycles
  , _oops = oops
  }

invariantsCheck inv info =
  ( "should give correct invariants for " ++ show inv
  , instructionInfo inv  `shouldBe` info
  )

-- This test encodes the size and cycles specified in
-- http://obelisk.me.uk/6502/reference.html.
instructionInvariantsSpec = do
  describe "instructionInfo should return the correct invariants for each instruction" $ do
    it' (invariantsCheck ((invariants lda zeroPage :: OpBuild 0xA5)) (ii 0xA5 1 3 0))


