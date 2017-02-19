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
module Cpu where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.Kind
import Data.Proxy
import qualified Data.Vector as V
import GHC.TypeLits
import GHC.Word (Word8, Word16)
import Numeric (showHex)

--import Cpu.Register

--------------------------------------------------------------------------------
---- * Cpu
-- $cpu
-- The 6502 Cpu has 6 registers:
--
--  * 'programCounter' - the Program Counter.

newtype Ram = Ram (V.Vector Word8)

(!?) :: Ram -> Int -> Maybe Word8
(Ram v) !? i = v V.!? i

slice start size (Ram v) = V.slice start size v

data Cpu = Cpu
  { pc     :: Word16  -- ^ Program counter
  , y      :: Word8   -- ^ Y register
  , x      :: Word8   -- ^ X register
  , status :: Status  -- ^ Status register
  , sp     :: Word8   -- ^ Stack pointer
  , acc    :: Word8   -- ^ Accumulator
  } deriving (Eq, Show)

newtype Accumulator = Accumulator Word8 deriving (Eq, Show)

loadAcc :: Word8 -> Accumulator
loadAcc = Accumulator

readAcc :: Accumulator  -> Word8
readAcc (Accumulator w) = w

newtype X = X Word8 deriving (Eq, Show)

loadX = X
readX (X w) = w

newtype Y = Y Word8 deriving (Eq, Show)

loadY = Y
readY (Y w) = w

newtype PC = PC Word16 deriving (Eq, Show)
loadPc = PC
readPc (PC w) = w

newtype SP = SP Word8 deriving (Eq, Show)
loadSp = SP
readSp (SP w) = w

-- | Initialize a Cpu.
-- FIXME: Figure out how to correctly initialize.
initCpu = Cpu 0x0000 0x00 0x00 (initStatus 0x00) 0x00 0x00

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

-- Convenience function, less characters to create 'IsInstruction' typeclasses :P.
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


--------------------------------------------------------------------------------
-- * Status Register
-- $statusRegister
-- The status register has 7 flags each corresponding to a single bit in a
-- byte - with bit 5 unused and always set.

newtype Status = Status Word8 deriving (Eq, Show)

-- | Builds a 'Status' from a 'Word8', setting bit 5.
initStatus :: Word8 -> Status
initStatus b = Status $ setBit b 5

-- | The 'Word8' corresponding to 'Status'.
statusBits :: Status -> Word8
statusBits (Status b) = b

-- | Relates status flags to their corresponding bit locations.
data StatusBit (b :: Nat) where
  Carry         :: StatusBit 0
  Zero          :: StatusBit 1
  InterruptMode :: StatusBit 2
  DecimalMode   :: StatusBit 3
  SoftInterrupt :: StatusBit 4
  Unused        :: StatusBit 5
  Overflow      :: StatusBit 6
  Sign          :: StatusBit 7
deriving instance Show (StatusBit b)

-- | Unifies all of our status register flags under a single type.
data StatusFlag = forall a. (KnownNat a) => StatusFlag (StatusBit a)
deriving instance Show StatusFlag

-- | Set the bit corresponding to the given 'StatusBit'.
setFlag :: KnownNat a => StatusBit a -> State Cpu ()
setFlag Unused = pure ()
setFlag f = withStatusFlag f setBit


-- | Clear the bit corresponding to the given 'StatusBit'.
clearFlag :: KnownNat a => StatusBit a -> State Cpu ()
clearFlag Unused = pure ()
clearFlag f = withStatusFlag f clearBit


-- | True if the given flag is set in the given 'Status' register.
isFlagSet :: KnownNat a => StatusBit a -> Cpu -> Bool
isFlagSet Unused _ = True
isFlagSet f (Cpu _ _ _ (Status b) _ _) = testBit b (statusBit f)


-- | With the given 'StatusBit' location and a function, update the status register with that
-- function.
withStatusFlag :: KnownNat a => StatusBit a -> (Word8 -> Int -> Word8) -> State Cpu ()
withStatusFlag (flag :: StatusBit a) f = do
  cpu <- get
  let (Status byte) = status cpu
  put $ cpu { status = Status $ f byte (fromIntegral (natVal (Proxy :: Proxy a))) }

statusBit :: KnownNat a => StatusBit a -> Int
statusBit (_ :: StatusBit a) = fromIntegral (natVal (Proxy :: Proxy a))


