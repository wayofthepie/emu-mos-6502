{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cpu (
  -- * Cpu
  -- $cpu
  Cpu
  , programCounter
  -- * Status Register
  -- $statusRegister
  , initStatusRegister
  , Flag
  , setFlag
  , clearFlag
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

-- | This constructor builds a 'Status' with bit 5 set and all other bits unset.
initStatusRegister :: Status
initStatusRegister = Status $ setBit 0x00 5

-- | Carry flag, bit 0.
data Carry = Carry

-- | Zero flag, bit 1.
data Zero = Zero

-- | Interrupt enable/disable flag, bit 2
data InterruptMode = InterruptMode

-- | Decimal mode flag, bit 3.
data DecimalMode = DecimalMode

-- | B flag, for software interrupts (BRK instruction), bit 4
data SoftInterrupt = SoftInterrupt

-- | Overflow flag, bit 6.
data Overflow = Overflow

-- | Sign flag, bit 7.
data Sign = Sign

-- | Relates status flags to their corresponding bit locations. Bit 5 is unused, but should
-- be 1 at all times.
type family FlagBit f = i | i -> f where
  FlagBit 0 = Carry
  FlagBit 2 = Zero
  FlagBit 3 = DecimalMode
  FlagBit 4 = SoftInterrupt
  FlagBit 6 = Overflow
  FlagBit 7 = Sign

-- | A flag is a bit in the status register which can be set or cleared.
class Flag a where
  setFlag   :: a -> Status -> Status
  clearFlag :: a -> Status -> Status
  isSet     :: a -> Status -> Bool

instance Flag Carry where
  setFlag   = setBitF
  clearFlag = clearBitF
  isSet     = isSetF

instance Flag Zero where
  setFlag   = setBitF
  clearFlag = clearBitF
  isSet     = isSetF

instance Flag DecimalMode where
  setFlag   = setBitF
  clearFlag = clearBitF
  isSet     = isSetF

instance Flag SoftInterrupt where
  setFlag   = setBitF
  clearFlag = clearBitF
  isSet     = isSetF

instance Flag Overflow where
  setFlag   = setBitF
  clearFlag = clearBitF
  isSet     = isSetF

instance Flag Sign where
  setFlag   = setBitF
  clearFlag = clearBitF
  isSet     = isSetF


-- | Set the bit corresponding to the given 'FlagBit'.
setBitF :: KnownNat a => FlagBit a -> Status -> Status
setBitF f s = withStatusFlag f s setBit

-- | Clear the bit corresponding to the given 'FlagBit'.
clearBitF :: KnownNat a => FlagBit a -> Status -> Status
clearBitF f s = withStatusFlag f s clearBit

isSetF :: KnownNat a => FlagBit a -> Status -> Bool
isSetF f (Status b) = testBit b (flagBit f)

-- | With the given 'FlagBit' location and a function, update the status register with that
-- function.
withStatusFlag :: KnownNat a => FlagBit a -> Status -> (Word8 -> Int -> Word8) -> Status
withStatusFlag (flag :: FlagBit a) (Status b) f =
  Status $ f b (fromIntegral (natVal (Proxy :: Proxy a)))

flagBit :: KnownNat a => FlagBit a -> Int
flagBit (_ :: FlagBit a) = fromIntegral (natVal (Proxy :: Proxy a))

