{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Cpu.Instruction where

import Data.Kind
import Data.Proxy
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits
import GHC.Word (Word8, Word16)


-- Singleton types for mnemonics.
data SADC; data SAND; data SASL

data SLDA; data SLDX


-- | Singleton types for addressing modes.
data SImplied
data SImmediate
data SZeroPage
data SZeroPageX
data SZeroPageY
data SAbsolute
data SAbsoluteX
data SAbsoluteY
data SIndirect
data SIndexedIndirect
data SIndirectIndexed


-- | Represents all singleton type address modes.
class IsAddr a
instance IsAddr SImmediate
instance IsAddr SZeroPage
instance IsAddr SZeroPageX
instance IsAddr SZeroPageY
instance IsAddr SAbsolute
instance IsAddr SAbsoluteX
instance IsAddr SAbsoluteY
instance IsAddr SIndirect
instance IsAddr SIndexedIndirect
instance IsAddr SIndirectIndexed

-- | Represents all singleton type mnemonics
class IsMnemonic a
instance IsMnemonic SADC
instance IsMnemonic SLDA
instance IsMnemonic SLDX


data Mnemonic a where

  -- | Add the contents of a memory location to the accumulator together with the carry bit.
  -- If overflow occues, sets the carry bit, this enables multiple byte addition to be
  -- performed.
  ADC :: Mnemonic SADC
  LDA :: Mnemonic SLDA
  LDX :: Mnemonic SLDX

deriving instance Show (Mnemonic a)

data AddressMode a where
  Implied   :: AddressMode SImplied
  Immediate :: AddressMode SImmediate
  ZeroPage  :: AddressMode SZeroPage
  ZeroPageX :: AddressMode SZeroPageX
  ZeroPageY :: AddressMode SZeroPageY
  Absolute  :: AddressMode SAbsolute
  AbsoluteX :: AddressMode SAbsoluteX
  AbsoluteY :: AddressMode SAbsoluteY
  Indirect  :: AddressMode SIndirect
  IndexedIndirect :: AddressMode SIndexedIndirect
  IndirectIndexed :: AddressMode SIndirectIndexed
deriving instance Show (AddressMode a)

-- | 'Instruction' holds the invariants relating to an instruction.
data Instruction :: Type -> Type -> Nat -> Nat -> Nat -> Nat -> Type where
  Instruction :: (IsMnemonic m, IsAddr a)
             => Mnemonic m
             -> AddressMode a
             -> Instruction m a op size cycles oops

deriving instance Show (Instruction a b c d e f)

-- | Build the invariants for the given operator. The information about each instruction is
-- taken from http://obelisk.me.uk/6502/reference.html.
type family OpBuild o = r where
  -- ADC
  OpBuild 0x69 = Instruction SADC SImmediate 0x69 2 2 0
  OpBuild 0x65 = Instruction SADC SZeroPage  0x65 2 3 0
  OpBuild 0x75 = Instruction SADC SZeroPageX 0x75 2 4 0
  OpBuild 0x6D = Instruction SADC SAbsolute  0x6D 3 4 0
  OpBuild 0x7D = Instruction SADC SAbsoluteX 0x7D 3 4 1
  OpBuild 0x79 = Instruction SADC SAbsoluteY 0x79 3 4 1
  OpBuild 0x61 = Instruction SADC SIndexedIndirect 0x61 2 6 0
  OpBuild 0x71 = Instruction SADC SIndirectIndexed 0x71 2 5 1

  -- LDA
  OpBuild 0xA9 = Instruction SLDA SImmediate 0xA9 2 2 0
  OpBuild 0xA5 = Instruction SLDA SZeroPage  0xA5 2 3 0
  OpBuild 0xB5 = Instruction SLDA SZeroPageX 0xB5 2 4 0
  OpBuild 0xAD = Instruction SLDA SAbsolute  0xAD 3 4 0
  OpBuild 0xBD = Instruction SLDA SAbsoluteX 0xBD 3 4 1
  OpBuild 0xB9 = Instruction SLDA SAbsoluteY 0xB9 3 4 1
  OpBuild 0xA1 = Instruction SLDA SIndexedIndirect 0xA1 2 6 0
  OpBuild 0xB1 = Instruction SLDA SIndirectIndexed 0xB1 2 5 1

  -- LDX
  OpBuild 0xA2 = Instruction SLDX SImmediate 0xA2 2 2 0
  OpBuild 0xA6 = Instruction SLDX SZeroPage  0xA6 2 3 0
  OpBuild 0xB6 = Instruction SLDX SZeroPageY 0xB6 2 4 0
  OpBuild 0xAE = Instruction SLDX SAbsolute  0xAE 3 4 0
  OpBuild 0xBE = Instruction SLDX SAbsoluteY 0xBE 3 4 1


data InstructionInfo = InstructionInfo
  { _opCode :: Word8
  , _size   :: Int
  , _cycles :: Int
  , _oops   :: Int
  } deriving (Eq, Show)

instOpCode = _opCode
instSize = _size
instCycles = _cycles
instOops = _oops

type InstructionConstraints o s c p = (KnownNat o, KnownNat s, KnownNat c, KnownNat p)

instructionInfo :: InstructionConstraints o s c p => Instruction mm a o s c p -> InstructionInfo
instructionInfo (_ :: Instruction mm a o s c p) = InstructionInfo
  (fromIntegral . natVal $ (Proxy :: Proxy o))
  (fromIntegral . natVal $ (Proxy :: Proxy s))
  (fromIntegral . natVal $ (Proxy :: Proxy c))
  (fromIntegral . natVal $ (Proxy :: Proxy p))

