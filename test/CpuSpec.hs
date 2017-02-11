{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module CpuSpec where

import Data.Bits
import GHC.Word

import Cpu

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


instance Arbitrary Status where
  arbitrary = do
    w <- arbitrary :: Gen Word8
    pure (initStatus w)

instance Arbitrary Flag  where
  arbitrary = do
    flag <- elements [Flag Carry, Flag Zero, Flag DecimalMode, Flag SoftInterrupt, Flag Overflow, Flag Sign]
    pure flag

prop_setFlag_setsBitForFlag :: Status -> Bool
prop_setFlag_setsBitForFlag s = let b = statusBits s in
  setFlag Sign s == (initStatus $ setBit b 7)

prop_setFlag_bit5ShouldAlwaysBeSet (Flag f) s = let b = statusBits s in
  let b = statusBits (setFlag f $ s)
  in  testBit b 5

prop_clearFlag_clearsBitForFlag :: Status -> Bool
prop_clearFlag_clearsBitForFlag s = let b = statusBits s in
  clearFlag Sign s == initStatus (clearBit b 7)


