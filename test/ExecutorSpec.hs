{-# LANGUAGE ScopedTypeVariables #-}
module ExecutorSpec where

import Data.Char (toUpper)
import Data.Proxy
import GHC.TypeLits
import GHC.Word
import Numeric (showHex)

import Cpu.Instruction
import Executor

import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC


spec :: Spec
spec = do
  decodeOpSpec

decodeOpSpec = do
  describe "decodeOpSpec" $ do
    it "should decode given op to correct instruction" $
      property prop_decodeOpCode_createsCorrectInstruction


data KnownOpCode = KnownOpCode Word8 deriving Eq

instance Show KnownOpCode where
  show (KnownOpCode w) = "KnownOpCode 0x" ++ (map toUpper $ showHex w "")

-- Neccessary untl all op codes are implemented.
instance Arbitrary KnownOpCode where
  arbitrary = do
    e <- elements
      [ 0x69, 0x65, 0x75, 0x6D, 0x7D, 0x79, 0x61, 0x71 -- ADC
      , 0xA5 -- LDA
      , 0xA2 -- LDX
      ]
    pure $ KnownOpCode e


-- This is instructionInfo from Cpu.Instruction.
ii :: InstructionConstraints o s c p => Instruction mm a o s c p -> InstructionInfo
ii (_ :: Instruction mm a o s c p) = InstructionInfo
  (fromIntegral . natVal $ (Proxy :: Proxy o))
  (fromIntegral . natVal $ (Proxy :: Proxy s))
  (fromIntegral . natVal $ (Proxy :: Proxy c))
  (fromIntegral . natVal $ (Proxy :: Proxy p))


prop_decodeOpCode_createsCorrectInstruction :: KnownOpCode -> Bool
prop_decodeOpCode_createsCorrectInstruction (KnownOpCode w) =
  case decodeOpCode w of
    (Executable i@(Instruction mnem mode)) -> let info = ii i in
      (instOpCode info == w)
