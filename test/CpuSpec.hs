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
  writeWithModeSpec

-- Initialize the Cpu setting the register
initCpuForX x = initCpu { _x = (X x) }
initCpuForY y = initCpu { _y = (Y y) }

genProg :: Ram
genProg = initRamZero // [(0x0000,0xA9),(0x0001,0xDE),(0x0002, 0xEA), (0x00DE,0xF2),(0x00DF,0xA1)]


------------------------------------------------------------------------------------------
-- Read
------------------------------------------------------------------------------------------
readWithModeSpec = do
  describe "readWithMode" $ do

    describe "Immediate" $
      it "should return byte at PC + 1" $
        let (byte, _) = runState (readWithMode Immediate) (genProg,initCpu)
        in byte `shouldBe` 0xDE

    describe "ZeroPage" $
      it "should return the value at the address pointed to by PC + 1" $
        let (byte, _) = runState (readWithMode ZeroPage) (genProg, initCpu)
        in byte `shouldBe` 0xF2

    describe "ZeroPageX" $ do
      it "should return the value at the address built with the value at the address PC + 1, added to the X register" $
        let x = 0x01
            (byte, _) = runState (readWithMode ZeroPageX) (genProg, initCpuForX x)
        in  byte `shouldBe` 0xA1

      -- With ZeroPageX the address of the value is calculated by:
      --  1. Getting the 8-bit zero page address from the instruction (mem[pc + 1]).
      --  2. Adding the value of the x register to it.
      it "should wrap around if (mem[pc + 1] + X > 0xFF)" $
        let x = 0x23
            (byte, _) = runState (readWithMode ZeroPageX) (genProg, initCpuForX x)
        in  byte `shouldBe` 0xDE

    describe "ZeroPageY" $ do
      it "should return the value at the address built with the value at the address PC + 1, added to the Y register" $
        let y = 0x01
            (byte, _) = runState (readWithMode ZeroPageY) (genProg, initCpuForY y)
        in  byte `shouldBe` 0xA1

      it "should wrap around if (mem[pc + 1] + Y > 0xFF)" $
        let y = 0x23
            (byte, _) = runState (readWithMode ZeroPageY) (genProg, initCpuForY y)
        in  byte `shouldBe` 0xDE


------------------------------------------------------------------------------------------
-- Write
------------------------------------------------------------------------------------------
writeWithModeSpec = do
  describe "writeWithMode" $ do
    describe "ZeroPage" $
      it "should write to the address given by the value at pc + 1" $
        let byte = 0x05
            (_, (ram, _)) = runState (writeWithMode ZeroPage byte) (genProg, initCpu)
        in  (ram ! 0xDE) `shouldBe` byte




