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
  , StatusFlag(..)
  , Flag(..)
  , StatusBit
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
type family StatusBit b = f | f -> b where
  StatusBit 0 = Carry
  StatusBit 1 = Zero
  StatusBit 2 = InterruptMode
  StatusBit 3 = DecimalMode
  StatusBit 4 = SoftInterrupt
  StatusBit 6 = Overflow
  StatusBit 7 = Sign

-- | Unifies all of our status register flags under a single type.
data StatusFlag = forall a. (Eq a, Flag a, Show a) => StatusFlag a

instance Show StatusFlag where
  show (StatusFlag f) = show f


-- | A flag in th status register.
class Flag a where
  -- | Set the bit corresponding to the given 'StatusBit'. In the following example we set the
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
  setFlag :: a -> Status -> Status

  -- | Clear the bit corresponding to the given 'StatusBit'. In the following example we set
  -- and then clear the bit corresponding to the 'Sign' flag. Leaving the 'Status' register
  -- with only bit 5 set.
  --
  -- @
  --  > let toBits x = [if testBit x i then 1 else 0 | i <-[ 0 .. finiteBitSize x - 1]]
  --  > let status = clearFlag Sign . setFlag Sign $ initStatus 0x00
  --  >toBits $ statusBits status
  --  [0,0,0,0,0,1,0,0]
  -- @
  clearFlag :: a -> Status -> Status

  -- | True if the given flag is set in the given 'Status' register.
  isFlagSet :: a -> Status -> Bool

instance Flag Carry where
  setFlag   = setBitF
  clearFlag = clearBitF
  isFlagSet = isSetF

instance Flag Zero where
  setFlag   = setBitF
  clearFlag = clearBitF
  isFlagSet = isSetF

instance Flag InterruptMode where
  setFlag   = setBitF
  clearFlag = clearBitF
  isFlagSet = isSetF

instance Flag DecimalMode where
  setFlag   = setBitF
  clearFlag = clearBitF
  isFlagSet = isSetF

instance Flag SoftInterrupt where
  setFlag   = setBitF
  clearFlag = clearBitF
  isFlagSet = isSetF

instance Flag Overflow where
  setFlag   = setBitF
  clearFlag = clearBitF
  isFlagSet = isSetF

instance Flag Sign where
  setFlag   = setBitF
  clearFlag = clearBitF
  isFlagSet = isSetF

setBitF :: KnownNat a => StatusBit a -> Status -> Status
setBitF f s = withStatusFlag f s setBit

clearBitF :: KnownNat a => StatusBit a -> Status -> Status
clearBitF f s = withStatusFlag f s clearBit

isSetF :: KnownNat a => StatusBit a -> Status -> Bool
isSetF f (Status b) = testBit b (statusBit f)

-- | With the given 'StatusBit' location and a function, update the status register with that
-- function.
withStatusFlag :: KnownNat a => StatusBit a -> Status -> (Word8 -> Int -> Word8) -> Status
withStatusFlag (flag :: StatusBit a) (Status b) f =
  Status $ f b (fromIntegral (natVal (Proxy :: Proxy a)))

statusBit :: KnownNat a => StatusBit a -> Int
statusBit (_ :: StatusBit a) = fromIntegral (natVal (Proxy :: Proxy a))

