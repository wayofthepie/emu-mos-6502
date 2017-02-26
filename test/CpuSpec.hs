{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module CpuSpec where

import Control.Monad.State
import Data.Char (toUpper)
import Data.Proxy
import GHC.TypeLits
import GHC.Word
import Numeric (showHex)

import Cpu
import Cpu.Instruction
import Executor

import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC


spec :: Spec
spec = do
  readWithModeSpec

readWithModeSpec = do
  describe "readWithMode" $ do

    describe "Immediate" $
      it "should return byte at PC + 1" $
        let (w, _) = runState (readWithMode Immediate) (genProg,initCpu)
        in w `shouldBe` 0xDE

    describe "ZeroPage" $
      it "should return the value at the address pointed to by PC + 1" $
        let (w, _) = runState (readWithMode ZeroPage) (genProg, initCpu)
        in w `shouldBe` 0xF2

    describe "ZeroPageX" $ do
      it "should return the value at the address built with the value at the address PC + 1, added to the X register" $
        let x = 0x01
            (w, _) = runState (readWithMode ZeroPageX) (genProg, initCpuForX x)
        in  w `shouldBe` 0xA1

      -- With ZeroPageX the address of the value is calculated by:
      --  1. Getting the 8-bit zero page address from the instruction (mem[pc + 1]).
      --  2. Adding the value of the x register to it.
      it "should wrap around if (mem[pc + 1] + X > 0xFF)" $
        let x = 0x23
            (w, _) = runState (readWithMode ZeroPageX) (genProg, initCpuForX x)
        in  w `shouldBe` 0xDE


-- Initialize the Cpu setting the X register
initCpuForX x = initCpu { _x = (X x) }

genProg :: Ram
genProg = initRam // [(0x0000,0xA9),(0x0001,0xDE),(0x00DE,0xF2),(0x00DF,0xA1)]


