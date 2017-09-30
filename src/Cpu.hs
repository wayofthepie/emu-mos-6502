{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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

import Cpu.Instruction

newtype Machine a = Machine (State (a, Cpu) ()) -- ???


--------------------------------------------------------------------------------
-- * Memory
--------------------------------------------------------------------------------

newtype Ram = Ram (V.Vector Word8) deriving Show


-- | Initialize all memory to zero.
initRamZero :: Ram
initRamZero = Ram $ (V.generate (fromIntegral 0xffff) (\_ -> 0x00))


-- | Load a list of address/value pairs into memory. e.g. load prog:
--
-- @
-- let prog = [(0x000,0xA9),(0x0001, 0xDE)] in
-- flip runState (initRamZero // prog, initCpu) $ do
--   ... work with loaded program ...
-- @
(//) :: Ram -> [(Word16, Word8)] -> Ram
(Ram ram) // idxValList =
  Ram (ram V.// (fmap (\(i,v) -> (fromIntegral i, v)) idxValList))


-- | Retrieve the byte at the given index. e.g. within the monad, get the byte at address:
--
-- @
-- let prog = [(0x000,0xA9),(0x0001, 0xDE)] in
-- flip runState (initRamZero // prog, initCpu) $ do
--   (ram, _) <- get
--   byte <- ram ! 0x0001
--   ... work with byte ...
-- @
(!) :: Ram -> Word16 -> Word8
(Ram ram) ! idx = ram V.! (fromIntegral idx)


-- | Read a byte from memory using the given addressing mode. Returns a tuple containing the
-- byte read whether the page boundary was crossed. e.g.
--
-- @
-- let prog = [(0x0000,0xA9),(0x0001,0xDE),(0x00DE,0xF2),(0x00DF,0xA1)] in
-- flip runState (initRamZero // prog, initCpu) $ do
--   (byte, crossed) <- readWithMode ZeroPage -- byte here is 0xF2
--   ... work with byte/crossed ...
-- @
-- In the above example, the program counter ('PC') is zero, so reading with 'ZeroPage'
-- addressing will give the byte whos address is @ram ! (pc + 1)@, this address is 0xDE,
-- following that address we get the value 0xF2. There cannot be a page boundary cross with
-- any type of 'ZeroPage' addressing, so /crossed/ will be false.
--
-- Calling 'readWithMode' implies the program counter is pointing at the address of the
-- currently executing operation. So the first address 'readWithMode' will look at is 'PC'
-- + 1 .
readWithMode :: AddressMode -> State (Ram, Cpu) (Word8, Bool)
readWithMode Immediate = do
  (ram, cpu) <- get
  let (PC pc) = programCounter cpu
  let byte = ram ! (pc + 1)
  pure (byte, False)

readWithMode ZeroPage = do
  (ram, cpu) <- get
  let (PC pc) = programCounter cpu
  let byte = ram ! fromIntegral (ram ! (pc + 1))
  pure (byte, False)

readWithMode ZeroPageX = do
  (ram, cpu) <- get
  let (PC pc) = programCounter cpu
  let (X x) = xRegister cpu
  let byte = readZeroPageRegister ram pc x
  pure (byte, False)

readWithMode ZeroPageY = do
  (ram, cpu) <- get
  let (PC pc) = programCounter cpu
  let (Y y) = yRegister cpu
  let byte = readZeroPageRegister ram pc y
  pure (byte, False)


-- | Read from memory using the given 'Word8' value from the X or Y register. This is used
-- for 'ZeropageX' or 'ZeroPageY' reads.
readZeroPageRegister :: Ram -> Word16 -> Word8 -> Word8
readZeroPageRegister ram pc regVal =
  let addr = fromIntegral $ (ram ! (pc + 1)) + regVal
  in  ram ! addr


-- | Write to a memory address using the given addressing mode.
--
--Calling 'writeWithMode' implies the program counter is pointing at the address of the
-- currently executing operation. So the first address 'writeWithMode' will look at is 'PC'
-- + 1 .
writeWithMode :: AddressMode -> Word8 -> State (Ram, Cpu) ()
writeWithMode ZeroPage byte =  do
  (ram, cpu) <- get
  let (PC pc) = programCounter cpu
  let addr = fromIntegral $ ram ! (pc + 1)
  put (ram // [(addr, byte)], cpu)

writeWithMode ZeroPageX byte = do
  (X x) <- getRegister xRegister
  writeZeroPageRegister x byte

writeWithMode ZeroPageY byte = do
  (Y y) <- getRegister yRegister
  writeZeroPageRegister y byte

writeWithMode Absolute byte = do
  (ram, cpu) <- get
  (PC pc) <- getRegister programCounter
  let low = fromIntegral $ ram ! (pc + 1)
  let high = shiftLeftEight $ ram ! (pc + 2)
  let addr = high + low
  put (ram // [(addr, byte)], cpu)


-- | Write to memory using the semantics of 'ZeroPageX' and 'ZeroPageY', with the register
-- value to use given with /regVal/.
writeZeroPageRegister :: Word8 -> Word8 -> State (Ram, Cpu) ()
writeZeroPageRegister regVal byte = do
  (ram, cpu) <- get
  (PC pc) <- getRegister programCounter
  let addr = fromIntegral $ (ram ! (pc + 1)) + regVal
  put (ram // [(addr, byte)], cpu)


-- | Shifts a byte left 8 bits, turning a 'Word8' into a 'Word16'.
shiftLeftEight :: Word8 -> Word16
shiftLeftEight w = (fromIntegral w :: Word16) `shiftL` 8


--------------------------------------------------------------------------------
-- * Cpu
--------------------------------------------------------------------------------
-- $cpu
-- The 6502 has 6 registers.
data Cpu = Cpu
  { _pc     :: PC  -- ^ Program counter
  , _x      :: X   -- ^ X register
  , _y      :: Y   -- ^ Y register
  , _status :: Status  -- ^ Status register
  , _sp     :: SP   -- ^ Stack pointer
  , _accumulator :: Accumulator -- ^ Accumulator
  } deriving Eq

instance Show Cpu where
  show (Cpu (PC pc) (X x) (Y y) (Status stat) (SP sp) (Accumulator acc)) =
    "Cpu { _pc = " ++ showHex pc
      ", x = " ++ showHex x
      ", y = " ++ showHex y
      ", status = " ++ showHex stat
      ", sp = " ++ showHex sp
      ", acc = " ++ showHex acc " }"


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


-- Helpers

-- | Increment the program counter ('PC') register by the number passed.
incPcBy :: Int -> State (a, Cpu) ()
incPcBy i = do
  (PC pc) <- getRegister programCounter
  loadRegister (PC (pc + fromIntegral i))


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

initCpuForPc pc = Cpu
  (PC pc)
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
isFlagSet :: KnownNat a => StatusBit a -> State (b, Cpu) Bool
isFlagSet Unused = pure True
isFlagSet f = do
  (_, cpu) <- get
  let (Status byte) = _status cpu
  pure $ testBit byte (statusBit f)


-- | With the given 'StatusBit' location and a function, update the status register with that
-- function.
withStatusFlag :: KnownNat a => StatusBit a -> (Word8 -> Int -> Word8) -> State (b, Cpu) ()
withStatusFlag (_ :: StatusBit a) f = do
  (a, cpu) <- get
  let (Status byte) = _status cpu
  put $ (a, cpu { _status = Status $ f byte (fromIntegral (natVal (Proxy :: Proxy a))) })


-- | Return the bit location corresponding to the given 'StatusBit'.
statusBit :: KnownNat a => StatusBit a -> Int
statusBit (_ :: StatusBit a) = fromIntegral (natVal (Proxy :: Proxy a))
