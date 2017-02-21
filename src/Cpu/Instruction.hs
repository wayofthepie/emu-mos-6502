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

invariants m a = Invariants m a

-- Convenience functions
lda = LDA SLDA
sta = STA SSTA

zeroPage = ZeroPage SZeroPage
zeroPageX = ZeroPageX SZeroPageX

-- | Singleton types for mnemonics.
data SLDA = SLDA deriving Show
data SSTA = SSTA deriving Show

-- | Singleton types for addressing modes.
data SZeroPage = SZeroPage
data SZeroPageX = SZeroPageX

-- | Represents all singleton type address modes.
class IsAddr a
instance IsAddr SZeroPage
instance IsAddr SZeroPageX

-- | represents all singleton type mnemonics
class IsMnemonic a
instance IsMnemonic SLDA
instance IsMnemonic SSTA

data Mnemonic a where
  LDA :: a -> Mnemonic a
  STA :: a -> Mnemonic a
instance Show (Mnemonic a) where
  show (LDA _) = "LDA"
  show (STA _) = "STA"

data AddressMode a where
  ZeroPage :: a -> AddressMode a
  ZeroPageX :: a -> AddressMode a
instance Show (AddressMode a) where
  show (ZeroPage _) = "ZeroPage"
  show (ZeroPageX _) = "ZeroPageX"

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
  OpBuild 0xA5 = Invariants SLDA SZeroPage 0xA5 1 3 0

data InstructionInfo = InstructionInfo
  { _opCode      :: Word8
  , _operandSize :: Int
  , _cycles      :: Int
  , _oops        :: Int
  } deriving (Eq, Show)

instructionInfo :: InstructionConstraints o s c p => Invariants mm a o s c p -> InstructionInfo
instructionInfo (_ :: Invariants mm a o s c p) = InstructionInfo
  (fromIntegral . natVal $ (Proxy :: Proxy o))
  (fromIntegral . natVal $ (Proxy :: Proxy s))
  (fromIntegral . natVal $ (Proxy :: Proxy c))
  (fromIntegral . natVal $ (Proxy :: Proxy p))



