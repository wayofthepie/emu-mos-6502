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
import GHC.Word (Word8, Word16)

import Cpu
import Cpu.Instruction

import Debug.Trace

-- For testing. Load prog into memory, point program counter at /pc/ and execute.
loadAndExecute prog pc = flip runState (initRamZero // prog, initCpuForPc pc) $ do
  (PC pc) <- getRegister programCounter
  exec pc pc
 where
  exec initPc pc = do
    get >>= \(ram,_) -> execute (buildInstruction (ram ! fromIntegral pc))
    getRegister programCounter >>= \(PC next) ->
      unless (next >= initPc + (fromIntegral . length $ prog)) (exec initPc next)


-- | Execute a single instruction.
execute :: Instruction -> State (Ram, Cpu) ()
execute ins = execute' ins >>= \crossed -> do
  incPcBy instructionSize
  -- FIXME: Do something with cycles!
 where
  instructionSize = instSize ins

-- | Execute the instruction, returning whether the instruction crossed the page boundary.
execute' :: Instruction -> State (Ram, Cpu) Bool
execute' inst@(Instruction mnem mode isize icycles oops) =
  case mnem of
    LDA -> do
      (byte, crossed) <- readWithMode mode
      when (isZero byte) (setFlag Zero)
      when (isNegative byte) (setFlag Sign)
      loadRegister (Accumulator byte)
      pure crossed
    STA -> do
      (Accumulator byte) <- getRegister accumulator
      writeWithMode mode byte
      pure False
    ADC -> do
      (Accumulator accByte) <- getRegister accumulator
      (byte, crossed) <- readWithMode mode
      isCarryInSet <- isFlagSet Carry
      let sum = toWord16 byte + toWord16 accByte + if isCarryInSet then 1 else 0
      let w8sum = toWord8 sum
      if sum > 0xFF then setFlag Carry else clearFlag Carry
      when (isZero w8sum) (setFlag Zero)
      when (isNegative w8sum) (setFlag Sign)
      when (hasOverflow accByte byte w8sum) (setFlag Overflow)
      pure crossed

-- | Is the given 'Word8' negative.
isNegative :: Word8 -> Bool
isNegative w = testBit w 7

-- | Is the given 'Word8' zero.
isZero :: Word8 -> Bool
isZero w = w == 0

toWord16 :: Integral a => a -> Word16
toWord16 = fromIntegral

toWord8 :: Integral a => a -> Word8
toWord8 = fromIntegral

-- | True if addition in two's compliment of the given bytes
-- yields overflow in the result.
hasOverflow :: Word8 -> Word8 -> Word8 -> Bool
hasOverflow opOne opTwo result =
  complement (opOne .|. opTwo) .&. (opOne .|. result) .&. 0x80 /= 0