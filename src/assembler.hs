module Assembler where

import Data_info
import Data.Char (isDigit)

--part2

assembleAexp :: Aexp -> Code
assembleAexp (ANum n) = [Push n]
assembleAexp (AVar var) = [Fetch var]
assembleAexp (AAdd e1 e2) = assembleAexp e1 ++ assembleAexp e2 ++ [Add]
assembleAexp (AMult e1 e2) = assembleAexp e1 ++ assembleAexp e2 ++ [Mult]
assembleAexp (ASub e1 e2) = assembleAexp e1 ++ assembleAexp e2 ++ [Sub]

assembleBexp :: Bexp -> Code
assembleBexp BTrue = [Tru]
assembleBexp BFalse = [Fals]
assembleBexp (BEq e1 e2) = assembleAexp e1 ++ assembleAexp e2 ++ [Equ]
assembleBexp (BLe e1 e2) = assembleAexp e1 ++ assembleAexp e2 ++ [Le]
assembleBexp (BNot b) = assembleBexp b ++ [Neg]
assembleBexp (BAnd b1 b2) = assembleBexp b1 ++ assembleBexp b2 ++ [And]

assembleStm :: Stm -> Code
assembleStm (Assign var aexp) = assembleAexp aexp ++ [Store var]
assembleStm (If bexp stm1 stm2) = assembleBexp bexp ++ [Branch (assembleStm stm1) (assembleStm stm2)]
assembleStm (While bexp stm) = assembleBexp bexp ++ [Loop (assembleStm stm) [Noop]]
assembleStm (Seq stms) = concatMap assembleStm stms