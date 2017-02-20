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
-- $addressingModes
-- The 6502 has 13 addressing modes.


-- | For instructions which use an 'Implied' addressing mode the information to be
-- manipulated is omplied by the instruction itself.
data Implied = Implied deriving Show


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


-- | Sometimes referred to as zero page absolute, as it is 'Absolute' addressing with
-- /00/ for the high byte. In this addressing mode the operands address in memory is given.
-- If the address is on zero page - any address where the high byte is /00/ - only a single
-- byte is needed for the address, the processor will fill in the /00/ high byte.
-- e.g.
--
-- @
--  LDA $00F4
-- @
--
-- This says load the value found at the address $F4 into the accumulator.
data ZeroPage = ZeroPage deriving Show


-- | Similar to 'ZeroPage', except the address given is added to the value in the 'X'
-- register to give the actual address to be used. Note that the target address is
-- limited to the first /0xFF/ bytes so overflow when computing the actual address will wrap
-- around. For example, if the instruction is /LDA $C0,X/, and X is $60 then the target
-- address will be /$20/. /$C0 +  $60 = $120/ but the value wraps around, i.e.
-- /(x + y) mod 256/.
-- e.g.
--
-- @
--  LDA $20, X
-- @
data ZeroPageX = ZeroPageX deriving Show


-- | See also 'ZeroPage'. With 'Absolute' addressing the operand is a full 16-nit address to
-- the target location.
-- e.g.
--
-- @
--  LDA $31F6
-- @
--
-- This says load the value at address $31F6. Note also that for 2 byte addresses the
-- /low/ byte is store first, in the above case it would be stored as /$AD $F6 $31/.
data Absolute = Absolute deriving Show


-- | Similar to 'Absolute', except the address given is added to the value of the 'X'
-- register to get the address of the actual value.
-- e.g.
--
-- @
--  LDA $31F6, X
-- @
data AbsoluteX = AbsoluteX deriving Show


-- | Similar to 'Absolute', except the address given is added to the value of the 'Y'
-- register to get the actual value of the address to be used.
-- e.g.
--
-- @
--  LDA $31F6, Y
-- @
data AbsoluteY = AbsoluteY deriving Show


-- | This only applies to the 'JMP' instruction. The operand for the instruction is a 16-bit
-- address which itself identifies another 16-bit address which is the real target of the
-- instruction. For example if location $0120 contains $FC and location $0121 contains $BA
-- then the instruction JMP ($0120) will cause the next instruction execution to occur at $BAFC
-- (e.g. the contents of $0120 and $0121). e.g.
--
-- @
--  JMP ($0120)
-- @
data Indirect = Indirect deriving Show


-- | Normally used in conjunction with a table of addresses held on zero page. The address
-- of the table is built using the value of the given byte added to the 'X' register (with
-- zero page wrap around, see 'ZeroPageX'), this gives the location of the least significant
-- byte of the target address. e.g.
--
-- @
--  LDA ($40, X)
-- @
--
-- Loads the byte at the address /($40 + X) mod 256/ into the accumulator.
data IndexedIndirect = IndexedIndirect deriving Show


-- | Using this addressing mode, the instruction takes the zero page location of the least
-- significant byte of a 16-bit address and adds the 'Y' register to it to get the value of
-- the actual address to be used. e.g.
--
-- @
--  LDA ($40),Y
-- @
data IndirectIndexed = IndirectIndexed deriving Show


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


-- | Relates mnemonics and addressing mode pairs which the instruction set actually allows.
class IsInstruction a

-- LDA instances.
instance IsInstruction (LDA Immediate)
instance IsInstruction (LDA ZeroPage)
instance IsInstruction (LDA ZeroPageX)
instance IsInstruction (LDA Absolute)
instance IsInstruction (LDA AbsoluteX)
instance IsInstruction (LDA AbsoluteY)


-- | Build an 'Instruction' from the given byte. 'Nothing' if the byte does not map to a
-- known instruction.
decodeOpCode :: Word8 -> Maybe Instruction
decodeOpCode op = case op of
  -- LDA
  0xA9 -> buildInst $ LDA Immediate op; 0xA5 -> buildInst $ LDA ZeroPage op;
  0xB5 -> buildInst $ LDA ZeroPageX op; 0xAD -> buildInst $ LDA Absolute op;
  0xBD -> buildInst $ LDA AbsoluteX op; 0xB9 -> buildInst $ LDA AbsoluteY op
  _    -> Nothing


-- | Build an 'Instruction' which contains all known information about an instruction.
buildInst a = Just $ Instruction a (instructionInfo a)


-- | From the given 'Invariants' pull the 'OperandBytes' size and number of 'Cycles'
-- (including normal 'cycles' and 'oops') down to the value level.
instructionInfo :: (KnownNat a, KnownNat c, KnownNat e)
                => Invariants (OperandBytes a) (Cycles c e) i
                -> InstructionInfo
instructionInfo (_ :: Invariants (OperandBytes a) (Cycles c e) i) =
  InstructionInfo
    (fromIntegral (natVal (Proxy :: Proxy a)))
    (fromIntegral (natVal (Proxy :: Proxy c)))
    (fromIntegral (natVal (Proxy :: Proxy e)))

