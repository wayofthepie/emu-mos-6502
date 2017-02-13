module Cpu (
  -- * Cpu
  -- $cpu
  Cpu
  , programCounter
 ) where

import GHC.Word (Word8, Word16)

import Cpu.Register

--------------------------------------------------------------------------------
-- $cpu
-- The 6502 Cpu has 6 registers:
--
--  * 'programCounter' - the Program Counter.


data Cpu = Cpu
  { pc     :: Word16  -- ^ Program counter
  , y      :: Word8   -- ^ Y register
  , x      :: Word8   -- ^ X register
  , s      :: Status  -- ^ Status register
  , sp     :: Word8   -- ^ Stack pointer
  , acc    :: Word8   -- ^ Accumulator
  } deriving (Eq, Show)

programCounter = pc

data AddressingMode = Immediate | Absolute | ZeroPageAbsolute
  | Implied | Accumulator | Indexed | ZeroPageIndexed | Indirect
  | PreIndexedIndirect | PostIndexedIndirect | Relative deriving (Eq, Show)

data Mnemonic = LDA deriving (Eq, Show)

data OpCode = OpCode
  { opCode      :: Word8
  , instruction :: Mnemonic
  , addrMode    :: AddressingMode
  } deriving (Eq, Show)

data Instruction = Instruction OpCode Word8 deriving (Eq, Show)


decodeOpCode :: Word8 -> OpCode
decodeOpCode w = let op = OpCode w in case w of
  0xA9 -> op LDA Immediate

