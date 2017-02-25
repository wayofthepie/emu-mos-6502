{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Executor where

--import Data.ByteString
import Control.Monad.State
import Data.Bits
import Data.Maybe (maybe)
import qualified Data.Vector as V
import GHC.TypeLits
import GHC.Word (Word8)

import Cpu
import Cpu.Instruction


-- | Wraps an instruction so we can build and pass around 'Instruction's.
data Executable = forall m a o s c e.
  ( InstructionConstraints o s c e
  , IsMnemonic m
  , IsAddr a
  ) => Executable (Instruction m a o s c e)
deriving instance Show Executable

decodeOpCode :: Word8 -> Executable
decodeOpCode w = case w of
  -- ADC
  0x69 -> Executable (Instruction ADC Immediate :: OpBuild 0x69)

  0xA2 -> Executable (Instruction LDX Immediate :: OpBuild 0xA2)
  0xA5 -> Executable (Instruction LDA ZeroPage  :: OpBuild 0xA5)
  _ -> error ("Opcode " ++ show w ++ " is not implemented yet")


execute :: Executable -> State (Ram, Cpu) ()
execute (Executable ins) = execute' ins

-- Just testing.
execute' :: (InstructionConstraints o s c e) => Instruction m a o s c e -> State (Ram, Cpu) ()
execute' i@(Instruction m mode) = case m of
  LDA -> do
    let ii = instructionInfo i
    pc <- getRegister programCounter
    loadRegister (Accumulator 0x90)
    loadRegister (Accumulator 0x40)
    loadRegister (PC 0x2004)
    setFlag Sign
    setFlag Zero
  LDX -> do
    let ii = instructionInfo i
    loadRegister (Accumulator 0x80)
    loadRegister (PC 0x0004)
    setFlag Carry
    setFlag Zero

