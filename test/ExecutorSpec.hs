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
