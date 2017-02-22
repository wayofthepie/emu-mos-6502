{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
module Executor where

--import Data.ByteString
import Control.Monad.State
import Data.Bits
import Data.Maybe (maybe)
import qualified Data.Vector as V
import GHC.Word (Word8)

import Cpu
import Cpu.Instruction

execute :: Word8 -> Machine
execute w = case w of
  0xA5 -> exec ((Invariants LDA ZeroPage) :: OpBuild 0xA5)

exec :: Invariants m a o s c e -> Machine
exec (Invariants m mode) = case m of
  lda -> do
    loadRegister (Accumulator 0x90)
