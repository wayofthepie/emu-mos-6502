{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Cpu where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
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

data Mnemonic = LDA deriving (Eq, Show)
{-
data OpCode = OpCode
  { opCode      :: Word8
  , instruction :: Mnemonic
  , addrMode    :: AddressingMode
  } deriving (Eq, Show)

data Instruction = Instruction OpCode Word8 deriving (Eq, Show)

decodeOpCode :: Word8 -> OpCode
decodeOpCode w = let op = OpCode w in case w of
  0xA9 -> op LDA Immediate
-}
-- | The number of bytes to read when we encounter a certain addressing mode, /after/ we
-- have already read the opcode byte corresponding to the instruction. Addressing modes
-- which do not require any bytes beyond the /opcode/ to be read are not part of this type
-- family
{- type family AddrModeBytes n = mode | mode -> n where
  AddrModeBytes 1 = Immediate
  AddrModeBytes 2 = Absolute
  AddrModeBytes 1 = ZeroPageAbsolute
  AddrModeBytes 2 = Indexed
  AddrModeBytes 1 = ZeroPageIndexed
  AddrModeBytes 2 = Indirect
  AddrModeBytes 1 = PreIndexedIndirect
  AddrModeBytes 1 = PostIndexedIndirect -}

data AddrModeBytes (n :: Nat) where
  Immediate           :: AddrModeBytes 1
  Absolute            :: AddrModeBytes 2
  ZeroPageAbsolute    :: AddrModeBytes 1
  Indexed             :: AddrModeBytes 2
  ZerPageIndexed      :: AddrModeBytes 1
  Indirect            :: AddrModeBytes 2
  PreIndexedIndirect  :: AddrModeBytes 1
  PostIndexedIndirect :: AddrModeBytes 1


addrModeBytes :: KnownNat a => AddrModeBytes a -> Int
addrModeBytes (_ :: AddrModeBytes a) = fromIntegral (natVal (Proxy :: Proxy a))

