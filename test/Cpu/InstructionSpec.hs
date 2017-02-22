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

-- Construct instruction info from the given size 'size', cycles 'cycles' and
-- extra cycles 'oops'.
ii :: Word8 -> Int -> Int -> Int -> InstructionInfo
ii opCode size cycles oops = InstructionInfo
  { _opCode = opCode
  , _size = size
  , _cycles = cycles
  , _oops = oops
  }

invariantsCheck inv info =
  ( "should give correct invariants for " ++ show inv
  , instructionInfo inv  `shouldBe` info
  )

-- This test encodes info on instructions using
-- http://obelisk.me.uk/6502/reference.html as the reference.
instructionInvariantsSpec = do
  describe "instructionInfo should return the correct invariants for each instruction" $ do
    -- LDA
    it' (invariantsCheck ((Invariants LDA Immediate :: OpBuild 0xA9)) (ii 0xA9 2 2 0))
    it' (invariantsCheck ((Invariants LDA ZeroPage  :: OpBuild 0xA5)) (ii 0xA5 2 3 0))
    it' (invariantsCheck ((Invariants LDA ZeroPageX :: OpBuild 0xB5)) (ii 0xB5 2 4 0))
    it' (invariantsCheck ((Invariants LDA Absolute  :: OpBuild 0xAD)) (ii 0xAD 3 4 0))
    it' (invariantsCheck ((Invariants LDA AbsoluteX :: OpBuild 0xBD)) (ii 0xBD 3 4 1))
    it' (invariantsCheck ((Invariants LDA AbsoluteY :: OpBuild 0xB9)) (ii 0xB9 3 4 1))
    it' (invariantsCheck ((Invariants LDA IndexedIndirect :: OpBuild 0xA1)) (ii 0xA1 2 6 0))
    it' (invariantsCheck ((Invariants LDA IndirectIndexed :: OpBuild 0xB1)) (ii 0xB1 2 5 1))

    -- LDX
    it' (invariantsCheck ((Invariants LDX Immediate :: OpBuild 0xA2)) (ii 0xA2 2 2 0))
    it' (invariantsCheck ((Invariants LDX ZeroPage  :: OpBuild 0xA6)) (ii 0xA6 2 3 0))
    it' (invariantsCheck ((Invariants LDX ZeroPageY :: OpBuild 0xB6)) (ii 0xB6 2 4 0))
    it' (invariantsCheck ((Invariants LDX Absolute  :: OpBuild 0xAE)) (ii 0xAE 3 4 0))
    it' (invariantsCheck ((Invariants LDX AbsoluteY :: OpBuild 0xBE)) (ii 0xBE 3 4 1))
