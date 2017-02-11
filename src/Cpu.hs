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
  , Status
  , initStatusRegister
  , Flag
  , clear
  , set
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

data Carry = Carry
data Zero = Zero
data InterruptMode = InterruptMode
data DecimalMode = DecimalMode
data SoftInterrupt = SoftInterrupt -- BRK instruction
data Overflow = Overflow

-- | The Sign flag.
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
  set   :: a -> Status -> Status
  clear :: a -> Status -> Status

instance Flag Carry where
  set _ s   = setBitF s Carry
  clear _ s = clearBitF s Carry

instance Flag Zero where
  set _ s   = setBitF s Zero
  clear _ s = clearBitF s Zero

instance Flag DecimalMode where
  set _ s   = setBitF s DecimalMode
  clear _ s = clearBitF s DecimalMode

instance Flag SoftInterrupt where
  set _ s   = setBitF s SoftInterrupt
  clear _ s = clearBitF s SoftInterrupt

instance Flag Overflow where
  set _ s   = setBitF s Overflow
  clear _ s = clearBitF s Overflow

instance Flag Sign where
  set _ s   = setBitF s Sign
  clear _ s = clearBitF s Sign


-- | Set the bit corresponding to the given 'FlagBit'.
setBitF :: KnownNat a => Status -> FlagBit a -> Status
setBitF s f  = withStatusFlag f s setBit

-- | Clear the bit corresponding to the given 'FlagBit'.
clearBitF :: KnownNat a => Status -> FlagBit a -> Status
clearBitF s f = withStatusFlag f s clearBit

-- | With the given 'FlagBit' location and a function, update the status register with that
-- function.
withStatusFlag :: KnownNat a => FlagBit a -> Status -> (Word8 -> Int -> Word8) -> Status
withStatusFlag (flag :: FlagBit a) (Status b) f =
  Status $ f b (fromIntegral (natVal (Proxy :: Proxy a)))

