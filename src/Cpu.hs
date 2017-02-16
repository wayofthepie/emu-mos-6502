{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
module Cpu where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import GHC.Word (Word8, Word16)
import Numeric (showHex)

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

data Immediate = Immediate
data ZeroPage = ZeroPage
data ZeroPageX = ZeroPageX
data Absolute = Absolute
data AbsoluteX = AbsoluteX
data AbsoluteY = AbsoluteY
data IndirectX = IndirectX
data IndirectY = IndirectY

data Mnemonic where
  ACC :: Mnemonic

data LDA a = LDA a

data Instruction a = Instruction a

data Cycles a b
data OpCode a
data AddressModeBytes b

-- | Relates instructions and their possible addressing modes to their size, 'OpCode' and
-- number of 'Cycles'.
type family InstructionInfoType a c o  = r | r -> a c o where
  --  LDA
  InstructionInfoType (AddressModeBytes 2) (Cycles 2 0) (OpCode 0xA9) = Instruction (LDA Immediate)
  InstructionInfoType (AddressModeBytes 2) (Cycles 3 0) (OpCode 0xA5) = Instruction (LDA ZeroPage)
  InstructionInfoType (AddressModeBytes 2) (Cycles 4 0) (OpCode 0xB5) = Instruction (LDA ZeroPageX)
  InstructionInfoType (AddressModeBytes 3) (Cycles 4 0) (OpCode 0xAD) = Instruction (LDA Absolute)
  InstructionInfoType (AddressModeBytes 3) (Cycles 4 1) (OpCode 0xBD) = Instruction (LDA AbsoluteX)
  InstructionInfoType (AddressModeBytes 3) (Cycles 4 1) (OpCode 0xB9) = Instruction (LDA AbsoluteY)
  InstructionInfoType (AddressModeBytes 2) (Cycles 6 0) (OpCode 0xA1) = Instruction (LDA IndirectX)
  InstructionInfoType (AddressModeBytes 2) (Cycles 5 1) (OpCode 0xB1) = Instruction (LDA IndirectY)

-- | Information about an instruction and addressing mode pair.
data InstructionInfo = InstructionInfo
  { _size   :: Int
  , _opCode :: Word8
  , _cycles :: Int
  , _oops   ::  Int
  } deriving (Eq)

-- FIXME: Clean this up!
instance Show InstructionInfo where
  show (InstructionInfo s o c oo) = show $
    "InstructionInfo size = " ++ (show s)
      ++ " opCode = 0x"  ++ showHex o " cycles = " ++ (show c)
      ++ " oops = " ++ (show oo)

-- | The size of the instruction and its possible operand.
size :: InstructionInfo -> Int
size   = _size

-- |The opcode corresponding to this instruction.
opCode :: InstructionInfo -> Word8
opCode = _opCode

-- | The number of cycles this instruction incurs, not including page boundary crosses
--  (these are defined in 'oops')
cycles :: InstructionInfo -> Int
cycles = _cycles

-- | The number of extra cycles if the instruction crosses a page boundary.
oops :: InstructionInfo -> Int
oops = _oops

-- | Given an 'InstructionInfoType' construct a value level representation of the
-- instruction information.
instructionInfo :: (KnownNat a, KnownNat c, KnownNat e, KnownNat o) =>
                InstructionInfoType (AddressModeBytes a) (Cycles c e) (OpCode o)
                -> InstructionInfo
instructionInfo (_ :: InstructionInfoType (AddressModeBytes a) (Cycles c e) (OpCode o)) =
  InstructionInfo
    (fromIntegral (natVal (Proxy :: Proxy a)))
    (fromIntegral (natVal (Proxy :: Proxy o)))
    (fromIntegral (natVal (Proxy :: Proxy c)))
    (fromIntegral (natVal (Proxy :: Proxy e)))

