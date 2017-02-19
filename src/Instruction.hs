{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
module Instruction where

import Data.Kind
import Data.Proxy
import GHC.TypeLits
import GHC.Word (Word8, Word16)

------------------------------------------------------------------------------------------
-- * Instructions
-- $instructions
-- The 6502 has 56 instructions and a number of addressing modes.

-- ** Addressing Modes

-- | With 'Immediate' addressing the operand is used directly to perform the computation. It
-- is denoted in assembly with a /#/ before the operand.
-- e.g.
--
-- @
--  LDA #$22
-- @
--
-- This says load the value $22 directly into the accumulator.
data Immediate = Immediate deriving Show
data ZeroPage = ZeroPage
data ZeroPageX = ZeroPageX
data Absolute = Absolute
data AbsoluteX = AbsoluteX
data AbsoluteY = AbsoluteY
data IndirectX = IndirectX
data IndirectY = IndirectY

-- ** Mnemonics

data LDA a = LDA a Word8 deriving Show
data ACC a = ACC a Word8 deriving Show

-- ** Invariants And Instruction Information

-- | Holds all information about an instruction. It's 'Mnemonic', it's addressing mode,
--  it's 'OperandBytes' (size of the operand in bytes) and 'Cycles' (which holds cycles and
--  extra /oops/ cycles if crossing a page boundary). 'OperandBytes' and 'Cycles' are
--  encoded in types (see the 'IsInstruction' type class) but are pulled to the value level
--  and  exposed in 'InstructionInfo'.
data Instruction = forall a. (IsInstruction a, Show a) => Instruction a InstructionInfo
deriving instance Show Instruction

data Cycles a b

data OperandBytes b

-- | Information about an instruction and addressing mode pair.
data InstructionInfo = InstructionInfo
  { _size   :: Int
  , _cycles :: Int
  , _oops   :: Int
  } deriving (Eq, Show)

-- | Relates mnemonics and addressing mode pairs. Each instance encodes the operand size and
-- cycles.
class IsInstruction a where
  info :: a -> Instruction

instance IsInstruction (LDA Immediate) where info = info'
instance IsInstruction (ACC Immediate) where info = info'

-- Convenience function, less characters to create 'IsInstruction' instances :P.
info' ::
  ( IsInstruction (Invariants (OperandBytes a) (Cycles c e) i)
  , KnownNat e, KnownNat c, KnownNat a
  , Show (Invariants (OperandBytes a) (Cycles c e) i)
  ) => Invariants (OperandBytes a) (Cycles c e) i -> Instruction
info' a = Instruction a (instructionInfo a)


-- | Invariants about a given instruction - mnemonic and addressing mode. The following
-- invariants are defined:
--
--  [@opBytes@]
--    The size of the operand in bytes. This can be 0, 1 or 2.
--
--  [@cycles@]
--    The number of cycles in an execution that does not cross page boundaries.
--
--  [@oops@]
--    The number of /extra/ cycles if the instruction execution crosses page boundaries.
type family Invariants opBytes cycles oops = r | r -> opBytes cycles oops where
  Invariants (OperandBytes 1) (Cycles 2 0) (LDA Immediate) = LDA Immediate
  Invariants (OperandBytes 1) (Cycles 2 0) (ACC Immediate) = ACC Immediate


-- | From the given 'Invariants' pull the 'OperandBytes' size and number of 'Cycles'
-- (including normal 'cycles' and 'oops') 'oops' down to the value level.
instructionInfo :: (KnownNat a, KnownNat c, KnownNat e) =>
                Invariants (OperandBytes a) (Cycles c e) i
                -> InstructionInfo
instructionInfo (_ :: Invariants (OperandBytes a) (Cycles c e) i) =
  InstructionInfo
    (fromIntegral (natVal (Proxy :: Proxy a)))
    (fromIntegral (natVal (Proxy :: Proxy c)))
    (fromIntegral (natVal (Proxy :: Proxy e)))

-- | The size of the instruction and its possible operand.
size :: InstructionInfo -> Int
size   = _size

-- | The number of cycles this instruction incurs, not including page boundary crosses
--  (these are defined in 'oops').
cycles :: InstructionInfo -> Int
cycles = _cycles

-- | The number of extra cycles if the instruction crosses a page boundary.
oops :: InstructionInfo -> Int
oops = _oops



