{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeInType #-}
module Cpu.Register (
  -- * Status Register
  -- $statusRegister
  Status
  , StatusBit(..)
  , StatusFlag(..)
  , initStatus
  , statusBits
  , setFlag
  , clearFlag
  , isFlagSet
  ) where

import Data.Bits
import Data.Kind
import Data.Proxy
import GHC.TypeLits (TypeError, ErrorMessage(..), KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import GHC.Word

newtype AccRegister = AccRegister Word8 deriving (Eq, Show)

loadAcc :: Word8 -> AccRegister
loadAcc = AccRegister

readAcc :: AccRegister  -> Word8
readAcc (AccRegister w) = w

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

instance Show StatusFlag where
  show (StatusFlag f) = show "StatusFlag: " ++ show f


-- | Set the bit corresponding to the given 'StatusBit'. In the following example we set the
-- bit corresponding to the /Sign/ flag in an empty 'Status' register. This sets the last
-- (read left to right below, so right-most 'Int' in the list) bit, note that the fifth bit is
-- also set, this is /always/ set in the status register.
--
-- @
--  > let toBits x = [if testBit x i then 1 else 0 | i <-[ 0 .. finiteBitSize x - 1]]
--  > let status = setFlag Sign $ initStatus 0x00
--  > toBits $ statusBits status
--  [0,0,0,0,0,1,0,1]
-- @
setFlag :: KnownNat a => StatusBit a -> Status -> Status
setFlag Unused s = s
setFlag f s = withStatusFlag f s setBit


-- | Clear the bit corresponding to the given 'StatusBit'. In the following example we set
-- and then clear the bit corresponding to the /Sign/ flag. Leaving the 'Status' register
-- with only bit 5 set.
--
-- @
--  > let toBits x = [if testBit x i then 1 else 0 | i <-[ 0 .. finiteBitSize x - 1]]
--  > let status = clearFlag Sign . setFlag Sign $ initStatus 0x00
--  >toBits $ statusBits status
--  [0,0,0,0,0,1,0,0]
-- @
clearFlag :: KnownNat a => StatusBit a -> Status -> Status
clearFlag Unused s = s
clearFlag f s = withStatusFlag f s clearBit


-- | True if the given flag is set in the given 'Status' register.
isFlagSet :: KnownNat a => StatusBit a -> Status -> Bool
isFlagSet Unused _ = True
isFlagSet f (Status b) = testBit b (statusBit f)


-- | With the given 'StatusBit' location and a function, update the status register with that
-- function.
withStatusFlag :: KnownNat a => StatusBit a -> Status -> (Word8 -> Int -> Word8) -> Status
withStatusFlag (flag :: StatusBit a) (Status b) f =
  Status $ f b (fromIntegral (natVal (Proxy :: Proxy a)))

statusBit :: KnownNat a => StatusBit a -> Int
statusBit (_ :: StatusBit a) = fromIntegral (natVal (Proxy :: Proxy a))

