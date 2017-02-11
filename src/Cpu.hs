{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cpu (
  -- * Cpu
  -- $cpu
  Cpu
  , programCounter
  -- * Status Register
  -- $statusRegister
  , Status
  , initStatus
  , statusBits
  , Flag(..)
  , FlagBit
  , setFlag
  , clearFlag
  , Carry(..)
  , Zero(..)
  , InterruptMode(..)
  , DecimalMode(..)
  , SoftInterrupt(..)
  , Overflow(..)
  , Sign(..)
  ) where

import Control.Monad (void)
import Data.Bits
import Data.Kind
import Data.Proxy
import GHC.TypeLits (TypeError, ErrorMessage(..), KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import GHC.Word

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

--------------------------------------------------------------------------------
-- $statusRegister
--  The status register has 7 flags each corresponding to a single bit in a
-- byte - with bit 5 unused and always set.

newtype Status = Status Word8 deriving (Eq, Show)

-- | Builds a 'Status' from a 'Word8', setting bit 5.
initStatus :: Word8 -> Status
initStatus b = Status $ setBit b 5

-- | The 'Word8' corresponding to 'Status'.
statusBits :: Status -> Word8
statusBits (Status b) = b

-- | Carry flag, bit 0.
data Carry = Carry deriving (Eq, Show)

-- | Zero flag, bit 1.
data Zero = Zero deriving (Eq, Show)

-- | Interrupt enable/disable flag, bit 2
data InterruptMode = InterruptMode deriving (Eq, Show)

-- | Decimal mode flag, bit 3.
data DecimalMode = DecimalMode deriving (Eq, Show)

-- | B flag, for software interrupts (BRK instruction), bit 4
data SoftInterrupt = SoftInterrupt deriving (Eq, Show)

-- | Overflow flag, bit 6.
data Overflow = Overflow deriving (Eq, Show)

-- | Sign flag, bit 7.
data Sign = Sign deriving (Eq, Show)

-- | Relates status flags to their corresponding bit locations. Bit 5 is unused, but should
-- be 1 at all times.
type family FlagBit f = i | i -> f where
  FlagBit 0 = Carry
  FlagBit 1 = Zero
  FlagBit 2 = InterruptMode
  FlagBit 3 = DecimalMode
  FlagBit 4 = SoftInterrupt
  FlagBit 6 = Overflow
  FlagBit 7 = Sign

-- | Unifies all of our flags under a single type.
data Flag = forall a. (Eq a, Flag_ a, Show a) => Flag a

instance Show Flag where
  show (Flag f) = show f

class Flag_ a where
instance Flag_ Carry where
instance Flag_ Zero where
instance Flag_ InterruptMode where
instance Flag_ DecimalMode where
instance Flag_ SoftInterrupt where
instance Flag_ Overflow where
instance Flag_ Sign where


-- | Set the bit corresponding to the given 'FlagBit'. In the following example we set the
-- bit corresponding to the 'Sign' flag in an empty 'Status' register. This sets the last
-- (read left to right below, so right-most 'Int' in the list) bit, note that the fifth bit is
-- also set, this is /always/ set in the status register.
--
-- @
--  > let toBits x = [if testBit x i then 1 else 0 | i <-[ 0 .. finiteBitSize x - 1]]
--  > let status = setFlag Sign $ initStatus 0x00
--  > toBits $ statusBits status
--  [0,0,0,0,0,1,0,1]
-- @
setFlag :: KnownNat a => FlagBit a -> Status -> Status
setFlag f s = withStatusFlag f s setBit


-- | Clear the bit corresponding to the given 'FlagBit'. In the following example we set
-- and then clear the bit corresponding to the 'Sign' flag. Leaving the 'Status' register
-- with only bit 5 set.
--
-- @
--  > let toBits x = [if testBit x i then 1 else 0 | i <-[ 0 .. finiteBitSize x - 1]]
--  > let status = clearFlag Sign . setFlag Sign $ initStatus 0x00
--  >toBits $ statusBits status
--  [0,0,0,0,0,1,0,0]
-- @
clearFlag :: KnownNat a => FlagBit a -> Status -> Status
clearFlag f s = withStatusFlag f s clearBit

isSetF :: KnownNat a => FlagBit a -> Status -> Bool
isSetF f (Status b) = testBit b (flagBit f)

-- | With the given 'FlagBit' location and a function, update the status register with that
-- function.
withStatusFlag :: KnownNat a => FlagBit a -> Status -> (Word8 -> Int -> Word8) -> Status
withStatusFlag (flag :: FlagBit a) (Status b) f =
  Status $ f b (fromIntegral (natVal (Proxy :: Proxy a)))

flagBit :: KnownNat a => FlagBit a -> Int
flagBit (_ :: FlagBit a) = fromIntegral (natVal (Proxy :: Proxy a))

