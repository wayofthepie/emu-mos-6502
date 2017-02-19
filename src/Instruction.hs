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
data ZeroPage = ZeroPage deriving Show
data ZeroPageX = ZeroPageX deriving Show
data Absolute = Absolute deriving Show
data AbsoluteX = AbsoluteX deriving Show
data AbsoluteY = AbsoluteY deriving Show
data IndirectX = IndirectX deriving Show
data IndirectY = IndirectY deriving Show

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

-- | Information about an instruction pair.
data InstructionInfo = InstructionInfo
  { _size   :: Int
  , _cycles :: Int
  , _oops   :: Int
  } deriving (Eq, Show)

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

-- | The phantom types in 'Cycles' are used to encode the number of cycles needed for an
-- instruction using 'Nat's in 'Invariants'.
data Cycles cycles oops

-- | The phantom type in 'OperandBytes' is used to encode the size of an instructions
-- operand using a 'Nat' in 'Invariants'.
data OperandBytes size

-- | Invariants about a given instruction - mnemonic and addressing mode. The following
-- invariants are defined:
--
--  [@opBytes@]
--    The size of the operand in bytes, defined with 'OperandBytes'. This can be 0, 1 or 2.
--
--  [@cycles@]
--    A pair of numbers defined with 'Cycles'. The first represents the number of cycles
--    in an execution which does not cross a page boundary and the second is the /extra/
--    cycles if executioin crosses a page boundary.
--
--  [@inst@]
--    The mnemonic and addressing mode pair.
--
type family Invariants opBytes cycles inst = r | r -> opBytes cycles inst where
  -- LDA
  Invariants (OperandBytes 1) (Cycles 2 0) (LDA Immediate) = LDA Immediate
  Invariants (OperandBytes 1) (Cycles 3 0) (LDA ZeroPage)  = LDA ZeroPage
  Invariants (OperandBytes 1) (Cycles 4 0) (LDA ZeroPageX) = LDA ZeroPageX
  Invariants (OperandBytes 2) (Cycles 4 0) (LDA Absolute)  = LDA Absolute
  Invariants (OperandBytes 2) (Cycles 4 1) (LDA AbsoluteX) = LDA AbsoluteX
  Invariants (OperandBytes 2) (Cycles 4 1) (LDA AbsoluteY) = LDA AbsoluteY
  Invariants (OperandBytes 1) (Cycles 6 0) (LDA IndirectX) = LDA IndirectX
  Invariants (OperandBytes 1) (Cycles 5 1) (LDA IndirectY) = LDA IndirectY


-- | Relates mnemonics and addressing mode pairs which the instruction set actually allows.
class IsInstruction a

-- LDA instances.
instance IsInstruction (LDA Immediate)
instance IsInstruction (LDA ZeroPage)
instance IsInstruction (LDA ZeroPageX)
instance IsInstruction (LDA Absolute)
instance IsInstruction (LDA AbsoluteX)
instance IsInstruction (LDA AbsoluteY)
instance IsInstruction (LDA IndirectX)
instance IsInstruction (LDA IndirectY)


-- | Build an 'Instruction' from the given byte. 'Nothing' if the byte does not map to a
-- known instruction.
decodeOpCode :: Word8 -> Maybe Instruction
decodeOpCode op = case op of
  -- LDA
  0xA9 -> buildInst $ LDA Immediate op; 0xA5 -> buildInst $ LDA ZeroPage op;
  0xB5 -> buildInst $ LDA ZeroPageX op; 0xAD -> buildInst $ LDA Absolute op;
  0xBD -> buildInst $ LDA AbsoluteX op; 0xB9 -> buildInst $ LDA AbsoluteY op
  0xA1 -> buildInst $ LDA IndirectX op; 0xB1 -> buildInst $ LDA IndirectY op;
  _    -> Nothing


-- | Build an 'Instruction' which contains all known information about an instruction.
buildInst a = Just $ Instruction a (instructionInfo a)


-- | From the given 'Invariants' pull the 'OperandBytes' size and number of 'Cycles'
-- (including normal 'cycles' and 'oops') down to the value level.
instructionInfo :: (KnownNat a, KnownNat c, KnownNat e) =>
                Invariants (OperandBytes a) (Cycles c e) i
                -> InstructionInfo
instructionInfo (_ :: Invariants (OperandBytes a) (Cycles c e) i) =
  InstructionInfo
    (fromIntegral (natVal (Proxy :: Proxy a)))
    (fromIntegral (natVal (Proxy :: Proxy c)))
    (fromIntegral (natVal (Proxy :: Proxy e)))

