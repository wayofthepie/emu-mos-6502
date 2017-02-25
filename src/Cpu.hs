{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Cpu where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import qualified Data.IntMap as I
import Data.Kind
import Data.Proxy
import qualified Data.Vector as V
import GHC.TypeLits
import GHC.Word (Word8, Word16)
import Numeric (showHex)


newtype Machine a = Machine (State (a, Cpu) ()) -- ???

newtype Ram = Ram (I.IntMap Word8)


-- Temporary interface for memory.
{-
readMem :: Word16 -> Int ->State (Ram, a) (Maybe [Word8])
readMem addr size =
  get >>= \(Ram mem,_) -> pure $ go mem (fromIntegral addr) [] size
 where
  go :: I.IntMap Word8 -> Int -> [Word8] -> Int -> Maybe [Word8]
  go m a ws 0 =  Just ws
  go m a ws size = (lookup a m) >>= \w -> go m (a + 1) (w : ws) (size - 1)
-}
--------------------------------------------------------------------------------
---- * Cpu
-- $cpu
-- The 6502 has 6 registers.
data Cpu = Cpu
  { _pc     :: PC  -- ^ Program counter
  , _x      :: X   -- ^ X register
  , _y      :: Y   -- ^ Y register
  , _status :: Status  -- ^ Status register
  , _sp     :: SP   -- ^ Stack pointer
  , _accumulator :: Accumulator -- ^ Accumulator
  } deriving (Eq, Show)


-- | The Program Counter.
newtype PC = PC Word16 deriving (Eq, Show)

-- | The Accumulator register.
newtype Accumulator = Accumulator Word8 deriving (Eq, Show)

-- | The X register.
newtype X = X Word8 deriving (Eq, Show)

-- | The Y register.
newtype Y = Y Word8 deriving (Eq, Show)

-- | The Status register.
newtype Status = Status Word8 deriving (Eq, Show)

-- | The Stack Pointer register.
newtype SP = SP Word8 deriving (Eq, Show)


-- | All registers except the 'Status' register are instances of 'Register'.
--
-- To load a value into a register using 'loadRegister' within the monad:
--
-- @
--  loadRegister (Accumulator 0x80)
-- @
--
-- To get a value from a register using 'getRegister' within the monad:
--
-- @
--  pc <- getRegister programCounter
-- @
class Register a where

  loadRegister :: a -> State (b, Cpu) ()

  getRegister :: (Cpu -> a) -> State (b, Cpu) a
  getRegister f = get >>= pure . f . snd

instance Register PC where
  loadRegister pc = modify (\(a, cpu) -> (a, cpu { _pc = pc }))

instance Register Accumulator where
  loadRegister acc = modify (\(a, cpu) -> (a, cpu { _accumulator = acc }))

instance Register X where
  loadRegister x = modify (\(a, cpu) -> (a, cpu { _x = x }))

instance Register Y where
  loadRegister y = modify (\(a, cpu) -> (a, cpu { _y = y }))

instance Register SP where
  loadRegister sp = modify (\(a, cpu) -> (a, cpu { _sp = sp }))


-- ** Accessors for registers
programCounter = _pc
accumulator    = _accumulator
xRegister      = _x
yRegister      = _y
status         = _status
stackPointer   = _sp


-- | Initialize a Cpu.
-- FIXME: Figure out how to correctly initialize.
initCpu = Cpu
  (PC 0x0000)
  (X 0x00)
  (Y 0x00)
  (initStatus 0x00)
  (SP 0x00)
  (Accumulator 0x00)

--------------------------------------------------------------------------------
-- * Status Register Actions
-- $statusRegister
-- The status register has 7 flags each corresponding to a single bit in a
-- byte - with bit 5 unused and always set.

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
setFlag :: KnownNat a => StatusBit a -> State (b, Cpu) ()
setFlag Unused = pure ()
setFlag f = withStatusFlag f setBit


-- | Clear the bit corresponding to the given 'StatusBit'.
clearFlag :: KnownNat a => StatusBit a -> State (b, Cpu) ()
clearFlag Unused = pure ()
clearFlag f = withStatusFlag f clearBit


-- | True if the given flag is set in the given 'Status' register.
isFlagSet :: KnownNat a => StatusBit a -> Cpu -> Bool
isFlagSet Unused _ = True
isFlagSet f (Cpu _ _ _ (Status b) _ _) = testBit b (statusBit f)


-- | With the given 'StatusBit' location and a function, update the status register with that
-- function.
withStatusFlag :: KnownNat a => StatusBit a -> (Word8 -> Int -> Word8) -> State (b, Cpu) ()
withStatusFlag (flag :: StatusBit a) f = do
  (a, cpu) <- get
  let (Status byte) = _status cpu
  put $ (a, cpu { _status = Status $ f byte (fromIntegral (natVal (Proxy :: Proxy a))) })


-- | Return the bit location corresponding to the given 'StatusBit'.
statusBit :: KnownNat a => StatusBit a -> Int
statusBit (_ :: StatusBit a) = fromIntegral (natVal (Proxy :: Proxy a))

