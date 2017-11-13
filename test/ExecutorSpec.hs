{-# LANGUAGE ScopedTypeVariables #-}
module ExecutorSpec where

import Control.Monad.State
import Data.Bits
import Data.Char (toUpper)
import Data.Proxy
import qualified Data.Vector as V
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
  executeSpec

executeSpec = do
  describe "execute" $ do
    describe "LDA" $ do
      let execLdaImmediate memory = flip runState (memory, initCpu) $ do execute (buildInstruction 0xA9)
      it "should set zero flag if acc is 0" $
        let (_, (_, cpu)) = execLdaImmediate initRamZero
            (Status stats) = status cpu
        in  testBit (stats) 1  `shouldBe` True

      it "should set sign flag if byte is negative" $
        let (_, (_, cpu)) = execLdaImmediate (Ram (V.fromList [0xA9, 0x80]))
            (Status byte) = status cpu
        in  testBit byte (statusBit Sign) `shouldBe` True

      it "should load byte into acc" $
        let expected = 0x05
            (_, (_, cpu)) = execLdaImmediate (Ram (V.fromList [0xA9, expected]))
            (Accumulator acc) = accumulator cpu
        in  acc `shouldBe` expected

    describe "ADC" $ do
      let execAdcImmediate cpu memory = flip runState (memory, cpu) $ do execute (buildInstruction 0x69)
      it "should set zero flag if acc is 0" $
        let (_, (_, cpu)) = execAdcImmediate initCpu initRamZero
            (Status stats) = status cpu
        in  testBit (stats) 1 `shouldBe` True

      it "should set carry flag if result greater than 0xFF" $
        let (_, (_, cpu)) = execAdcImmediate (initCpuForAcc 0xFF) (Ram (V.fromList [0x69, 0xFF]))
            (Status stats) = status cpu
        in  testBit stats 0 `shouldBe` True

      let execAdcImmCarryIn cpu memory = flip runState (memory, cpu) $ do
              execute (buildInstruction 0x69)
              setFlag Carry
      it "should set overflow flag if (twos complement) overflow occurs" $
        let (_, (_, cpu)) = execAdcImmCarryIn (initCpuForAcc 0x7F) (Ram (V.fromList [0x69, 0x01]))
            (Status stats) = status cpu
        in  testBit stats 6 `shouldBe` True

      it "should set negative flag if result is negative" $
        let (_, (_, cpu)) = execAdcImmediate (initCpuForAcc 0x7F) (Ram (V.fromList [0x69, 0x01]))
            (Status stats) = status cpu
        in  testBit stats 7 `shouldBe` True

initCpuForAcc acc = Cpu
  (PC 0x00)
  (X 0x00)
  (Y 0x00)
  (initStatus 0x00)
  (SP 0x00)
  (Accumulator acc)
