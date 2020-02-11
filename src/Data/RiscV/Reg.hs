{-# LANGUAGE DataKinds #-}

module Data.RiscV.Reg where

import Prelude hiding (Word)
import Data.Peano
import Data.Word.General

newtype Reg = Reg (Word (Succ (Succ (Succ (Succ (Succ Zero))))))
  deriving (Eq, Ord)
