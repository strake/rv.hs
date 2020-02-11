{-# LANGUAGE GADTs, DataKinds, TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.RiscV.I where

import Prelude hiding (Word)
import Data.Fin (Fin)
import Data.Functor.Const
import Data.Peano
import Data.Word.General

import Data.RiscV.Reg

data Instruction
  = LUI Reg Word20
  | AUIPC Reg Word20
  | JAL Reg Word20
  | JALR Reg Reg Word12
  | RegImmed IMinor Reg Reg Word12
  | RegReg IOp Reg Reg Reg
  | Branch BranchCmp Reg Reg Word12
  | Load Reg LogNumBytes Signedness
  | Stor Reg LogNumBytes
  | Fence FenceSpec FenceSpec
  | IFence
  | Trap Bool
  | CsrOp CsrOp Reg Reg Csr
  | CsrOpI CsrOp Reg Word5 Csr

data BranchCmp = EQ | NE | LT | LTU | GE | GEU
  deriving (Eq, Show, Enum)

newtype LogNumBytes = LogNumBytes (Fin (Succ (Succ (Succ (Succ Zero)))))
  deriving (Eq, Ord, Show, Enum)

data IOp = ∀ op . IOp (Const (ISubminor op) op)

data IMinor = Add | Slt | Sltu | Xor | Or | And | Shl | Shr
  deriving (Eq, Show, Enum)

type family ISubminor (op :: IMinor) where
    ISubminor Add = Signedness
    ISubminor Shr = Signedness
    ISubminor _ = ()

newtype Signedness = Signedness Bool
  deriving (Eq)

data FenceSpec = FenceSpec
  { fenceI, fenceO, fenceR, fenceW :: Bool }
  deriving (Eq, Show)

newtype Csr = Csr Word12
  deriving (Eq, Ord)

data CsrOp = RW | RS | RC
  deriving (Eq, Show, Enum)

type Word5  = Word (Succ (Succ (Succ (Succ (Succ Zero)))))
type Word12 = Word (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))))))
type Word20 = Word (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))))))))))))))
