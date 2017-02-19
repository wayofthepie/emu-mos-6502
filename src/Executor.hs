{-# LANGUAGE DataKinds #-}
module Executor where

--import Data.ByteString
import Control.Monad
import Data.Bits
import Data.Maybe (maybe)
import qualified Data.Vector as V
import GHC.Word (Word8)

import Cpu
import Instruction

data Executable a = Executable a [Word8] deriving Show

readOperand :: Ram -> Int -> Int -> [Word8]
readOperand _ _ 0          = []
readOperand mem start size = V.toList $ slice start size mem

readInstruction :: Ram -> Int -> Maybe (Executable Instruction)
readInstruction mem pc =
  mem !? pc >>= \op -> do
    instruction@(Instruction _ info) <- decodeOpCode op
    pure $ Executable instruction (readOperand mem (pc + 1) (size info))

decodeOpCode :: Word8 -> Maybe Instruction
decodeOpCode op = case op of
  0xA9 -> Just . info $ LDA Immediate op
  0xA5 -> Just . info $ ACC Immediate op
  _    -> Nothing
