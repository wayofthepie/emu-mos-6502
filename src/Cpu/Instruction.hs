{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Cpu.Instruction where

import GHC.Word (Word8)


data Mnemonic = ADC | LDA | LDX |  STA deriving (Eq, Show)

data AddressMode =  
  Immediate | ZeroPage  | ZeroPageX | ZeroPageY
  | Absolute  | AbsoluteX | AbsoluteY 
  | IndexedIndirect | IndirectIndexed 
  deriving (Eq, Show)

data Instruction = Instruction
  { instMnem   :: Mnemonic
  , instAddressMode :: AddressMode 
  , instSize   :: Int
  , instCycles :: Int
  , instOops   :: Int
  } deriving (Eq, Show)

buildInstruction :: Word8 -> Instruction
buildInstruction opcode = case opcode of
  -- ADC
  0x69 -> Instruction ADC Immediate 2 2 0
  0x65 -> Instruction ADC ZeroPage 2 3 0
  0x75 -> Instruction ADC ZeroPageX 2 4 0
  0x6D -> Instruction ADC Absolute 3 4 0
  0x7D -> Instruction ADC AbsoluteX 3 4 1
  0x79 -> Instruction ADC AbsoluteY 3 4 1
  0x61 -> Instruction ADC IndexedIndirect 2 6 0
  0x71 -> Instruction ADC IndirectIndexed 2 5 1
  -- LDA
  0xA9 -> Instruction LDA Immediate 2 2 0
  0xA5 -> Instruction LDA ZeroPage 2 3 0
  0xB5 -> Instruction LDA ZeroPageX 2 4 0
  0xAD -> Instruction LDA Absolute 3 4 0
  0xBD -> Instruction LDA AbsoluteX 3 4 1
  0xB9 -> Instruction LDA AbsoluteY 3 4 1
  0xA1 -> Instruction LDA IndexedIndirect 2 6 0
  0xB1 -> Instruction LDA IndirectIndexed 2 5 1
  -- LDX
  0xA2 -> Instruction LDX Immediate 2 2 0
  0xA6 -> Instruction LDX ZeroPage 2 3 0
  0xB6 -> Instruction LDX ZeroPageY 2 4 0
  0xAE -> Instruction LDX Absolute 3 4 0
  0xBE -> Instruction LDX AbsoluteY 3 4 1
  -- STA
  0x85 -> Instruction STA ZeroPage 2 3 0
  0x95 -> Instruction STA ZeroPageX 2 4 0
  0x8D -> Instruction STA Absolute 3 4 0
  0x9D -> Instruction STA AbsoluteX 3 5 0
  0x99 -> Instruction STA AbsoluteY 3 5 0
  0x81 -> Instruction STA IndexedIndirect 2 6 0
  0x91 -> Instruction STA IndirectIndexed 2 6 0
