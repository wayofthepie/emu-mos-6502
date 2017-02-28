{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
module Executor where

--import Data.ByteStringo
import Control.Monad
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


loadAndExecute prog = flip runState (initRamZero // prog, initCpu) $ do
  (PC pc) <- getRegister programCounter
  unless (fromIntegral pc == length prog) (exec pc)
 where
  exec pc = do
    (ram, cpu) <- get
    let inst = decodeOpCode (ram ! fromIntegral pc)
    execute inst

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
  0xA9 -> Executable (Instruction LDA Immediate :: OpBuild 0xA9)
  0xA5 -> Executable (Instruction LDA ZeroPage  :: OpBuild 0xA5)
  0xB5 -> Executable (Instruction LDA ZeroPageX :: OpBuild 0xB5)
  0xAD -> Executable (Instruction LDA Absolute  :: OpBuild 0xAD)
  0xBD -> Executable (Instruction LDA AbsoluteX :: OpBuild 0xBD)
  0xB9 -> Executable (Instruction LDA AbsoluteY :: OpBuild 0xB9)
  0xA1 -> Executable (Instruction LDA IndexedIndirect :: OpBuild 0xA1)
  0xB1 -> Executable (Instruction LDA IndirectIndexed :: OpBuild 0xB1)
  -- LDX
  0xA2 -> Executable (Instruction LDX Immediate :: OpBuild 0xA2)
  -- STA
  0x85 -> Executable (Instruction STA ZeroPage  :: OpBuild 0x85)
  0x95 -> Executable (Instruction STA ZeroPageX :: OpBuild 0x95)
  0x8D -> Executable (Instruction STA Absolute  :: OpBuild 0x8D)
  0x9D -> Executable (Instruction STA AbsoluteX :: OpBuild 0x9D)
  0x99 -> Executable (Instruction STA AbsoluteY :: OpBuild 0x99)
  0x81 -> Executable (Instruction STA IndexedIndirect :: OpBuild 0x81)
  0x91 -> Executable (Instruction STA IndirectIndexed :: OpBuild 0x91)

  _ -> error ("Opcode " ++ show w ++ " is not implemented yet")


-- | Execute a single instruction.
execute :: Executable -> State (Ram, Cpu) ()
execute (Executable ins) = execute' ins

execute' :: InstructionConstraints o s c e => Instruction m a o s c e -> State (Ram, Cpu) ()
execute' inst@(Instruction mnem mode) = do
  case mnem of
    LDA -> do
      byte <- readWithMode mode
      loadRegister (Accumulator byte)
      incPcBy instructionSize
 where
  info = instructionInfo inst
  instructionSize = instSize info


-- | Is the given 'Word8' negative.
isNegative :: Word8 -> Bool
isNegative w = testBit w 7

-- | Is the given 'Word8' zero.
isZero :: Word8 -> Bool
isZero w = w == 0
