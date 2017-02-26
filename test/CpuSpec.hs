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

genProg :: Ram
genProg = initRam // [(0,0xA9),(1,0x11)]

readWithModeSpec = do
  describe "readWithMode" $ do
    it "should return byte at current program counter + 1 if mode is immediate" $
      let ((), (Ram ram, cpu)) = runState (ldaImmediateExec) (genProg,initCpu)
      in  accumulator cpu == (Accumulator 0x11)


ldaImmediateExec = execute (Executable (Instruction LDA Immediate :: OpBuild 0xA9))

