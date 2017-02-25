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
  0x65 -> Executable (Instruction ADC ZeroPage :: OpBuild 0x65)
  0x75 -> Executable (Instruction ADC ZeroPageX :: OpBuild 0x75)
  0x6D -> Executable (Instruction ADC Absolute :: OpBuild 0x6D)
  0x7D -> Executable (Instruction ADC AbsoluteX :: OpBuild 0x7D)
  0x79 -> Executable (Instruction ADC AbsoluteY :: OpBuild 0x79)
  0x61 -> Executable (Instruction ADC IndexedIndirect :: OpBuild 0x61)
  0x71 -> Executable (Instruction ADC IndirectIndexed :: OpBuild 0x71)
  -- LDA
  0xA5 -> Executable (Instruction LDA ZeroPage  :: OpBuild 0xA5)
  -- LDX
  0xA2 -> Executable (Instruction LDX Immediate :: OpBuild 0xA2)

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

