{- |
 * Three address code
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.TAC where

import Playit.SymbolTable


data ThreeAddressCode = ThreeAddressCode
  { tacOperand :: Operation,
    tacLvalue  :: Maybe Operand,
    tacRvalue1 :: Maybe Operand,
    tacRvalue2 :: Maybe Operand
  }

class SymEntryCompatible where
  getSymID :: a -> String


data SymEntryCompatible a => Operand a = Variable a | Constant a


data Operation = Add | Mul -- | 
