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
        let ((byte,_), _) = runState (readWithMode Immediate) (genProg,initCpu)
        in byte `shouldBe` 0xDE

    describe "ZeroPage" $
      it "should return the value at the address pointed to by PC + 1" $
        let ((byte,_), _) = runState (readWithMode ZeroPage) (genProg, initCpu)
        in byte `shouldBe` 0xF2

    describe "ZeroPageX" $ do
      let run x = runState (readWithMode ZeroPageX) (genProg, initCpuForX x)

      it "should return the value at the address built with the value at the address PC + 1, added to the X register" $
        let x = 0x01
            ((byte,_), _) = run x
        in  byte `shouldBe` 0xA1

      -- With ZeroPageX the address of the value is calculated by:
      --  1. Getting the 8-bit zero page address from the instruction (mem[pc + 1]).
      --  2. Adding the value of the x register to it.
      it "should wrap around if (mem[pc + 1] + X > 0xFF)" $
        let x = 0x23
            ((byte,_), _) = run x
        in  byte `shouldBe` 0xDE

    describe "ZeroPageY" $ do
      let run y = runState (readWithMode ZeroPageY) (genProg, initCpuForY y)

      it "should return the value at the address built with the value at the address PC + 1, added to the Y register" $
        let y = 0x01
            ((byte,_), _) = run y
        in  byte `shouldBe` 0xA1

      it "should wrap around if (mem[pc + 1] + Y > 0xFF)" $
        let y = 0x23
            ((byte,_), _) = run y
        in  byte `shouldBe` 0xDE


------------------------------------------------------------------------------------------
-- Write
------------------------------------------------------------------------------------------
writeWithModeSpec = do
  describe "writeWithMode" $ do
    let byte = 0x05

    describe "ZeroPage" $
      it "should write to the address given by the value at pc + 1" $
        let (_, (ram, _)) = runState (writeWithMode ZeroPage byte) (genProg, initCpu)
        in  (ram ! 0xDE) `shouldBe` byte

    describe "ZeroPageX" $ do
      let run x = runState (writeWithMode ZeroPageX byte) (genProg, initCpuForX x)

      it "should write to the address given by ZeroPageX addressing" $
        let x = 0x01
            (_, (ram, _)) = run x
        in  (ram ! 0xDF) `shouldBe` byte
      it "should wrap around if (mem[pc + 1] + X > 0xFF)" $
        let x = 0x23
            (_, (ram, _)) = run x
        in  (ram ! 0x01) `shouldBe` byte

    describe "ZeroPageY" $ do
      let run y = runState (writeWithMode ZeroPageY byte) (genProg, initCpuForY y)

      it "should write to the address given by ZeroPageY addressing" $
        let y = 0x01
            (_, (ram, _)) = run y
        in  (ram ! 0x0DF) `shouldBe` byte
      it "should wrap around if (mem[pc + 1] + X > 0xFF)" $
        let y = 0x23
            (_, (ram,_)) =  run y
        in  (ram ! 0x01) `shouldBe` byte

