{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Cpu.RegisterSpec where

import Data.Bits
import GHC.Word

import Cpu.Register

import Test.Tasty.Hspec
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

spec :: Spec
spec = do
  statusRegisterSpec

statusRegisterSpec = do
  describe "Status register" $ do
    describe "setFlag" $ do
      it "should set only the bit corresponding to the given flag" (property prop_setFlag_setsBitForFlag)
      it "should always leave bit 5 set" (property prop_setFlag_bit5ShouldAlwaysBeSet)
    describe "clearFlag" $ do
      it "should clear only the bit corresponding to the given flag" (property prop_clearFlag_clearsBitForFlag)
    describe "isFlagSet" $ do
      it "should be true if flag was set" (property prop_isFlagSet_trueIfSet)


instance Arbitrary Status where
  arbitrary = do
    w <- arbitrary :: Gen Word8
    pure (initStatus w)

instance Arbitrary StatusFlag where
  arbitrary = do
    flag <- elements [
      StatusFlag Carry
      , StatusFlag Zero
      , StatusFlag DecimalMode
      , StatusFlag SoftInterrupt
      , StatusFlag Overflow
      , StatusFlag Sign
      ]
    pure flag

prop_setFlag_setsBitForFlag :: Status -> Bool
prop_setFlag_setsBitForFlag s = let b = statusBits s in
  setFlag Sign s == (initStatus $ setBit b 7)

prop_setFlag_bit5ShouldAlwaysBeSet :: StatusFlag -> Status -> Bool
prop_setFlag_bit5ShouldAlwaysBeSet (StatusFlag f) s =
  let b = statusBits (setFlag f s)
  in  testBit b 5

prop_clearFlag_clearsBitForFlag :: Status -> Bool
prop_clearFlag_clearsBitForFlag s = let b = statusBits s in
  clearFlag Sign s == initStatus (clearBit b 7)

prop_isFlagSet_trueIfSet :: StatusFlag -> Status -> Bool
prop_isFlagSet_trueIfSet (StatusFlag f) s = let b = statusBits s in
  isFlagSet f . setFlag f $ s

