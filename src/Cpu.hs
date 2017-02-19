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


