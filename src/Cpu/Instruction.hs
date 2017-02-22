{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Cpu.Instruction where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import GHC.Word (Word8, Word16)


-- | Singleton types for mnemonics.
data SLDA
data SLDX


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

-- | represents all singleton type mnemonics
class IsMnemonic a
instance IsMnemonic SLDA
instance IsMnemonic SLDX


data Mnemonic a where
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

-- | 'Invariants' builds the invariants relating to an instruction. It takes the following
-- arguments:
--
--  [@singleton instruction type@]
--    A singleton type representing an instruction
--
data Invariants :: Type -> Type -> Nat -> Nat -> Nat -> Nat -> Type where
  Invariants :: (IsMnemonic m, IsAddr a)
             => Mnemonic m
             -> AddressMode a
             -> Invariants m a op size cycles oops
deriving instance Show (Invariants a b c d e f)
type InstructionConstraints o s c p = (KnownNat o, KnownNat s, KnownNat c, KnownNat p)

type family OpBuild o = r | r -> o where
  -- LDA
  OpBuild 0xA9 = Invariants SLDA SImmediate 0xA9 2 2 0
  OpBuild 0xA5 = Invariants SLDA SZeroPage  0xA5 2 3 0
  OpBuild 0xB5 = Invariants SLDA SZeroPageX 0xB5 2 4 0
  OpBuild 0xAD = Invariants SLDA SAbsolute  0xAD 3 4 0
  OpBuild 0xBD = Invariants SLDA SAbsoluteX 0xBD 3 4 1
  OpBuild 0xB9 = Invariants SLDA SAbsoluteY 0xB9 3 4 1
  OpBuild 0xA1 = Invariants SLDA SIndexedIndirect 0xA1 2 6 0
  OpBuild 0xB1 = Invariants SLDA SIndirectIndexed 0xB1 2 5 1

  -- LDX
  OpBuild 0xA2 = Invariants SLDX SImmediate 0xA2 2 2 0
  OpBuild 0xA6 = Invariants SLDX SZeroPage  0xA6 2 3 0
  OpBuild 0xB6 = Invariants SLDX SZeroPageY 0xB6 2 4 0
  OpBuild 0xAE = Invariants SLDX SAbsolute  0xAE 3 4 0
  OpBuild 0xBE = Invariants SLDX SAbsoluteY 0xBE 3 4 1

data InstructionInfo = InstructionInfo
  { _opCode      :: Word8
  , _size :: Int
  , _cycles      :: Int
  , _oops        :: Int
  } deriving (Eq, Show)

instructionInfo :: InstructionConstraints o s c p => Invariants mm a o s c p -> InstructionInfo
instructionInfo (_ :: Invariants mm a o s c p) = InstructionInfo
  (fromIntegral . natVal $ (Proxy :: Proxy o))
  (fromIntegral . natVal $ (Proxy :: Proxy s))
  (fromIntegral . natVal $ (Proxy :: Proxy c))
  (fromIntegral . natVal $ (Proxy :: Proxy p))



