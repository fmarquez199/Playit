{- |
 * Three address code
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
{-# LANGUAGE DatatypeContexts #-}
module Playit.TAC where

import Playit.Types


gen :: Instr -> SymTab -> ThreeAddressCode
gen i st = ThreeAddressCode (Operation) (Lval) (Rval1) (Rval2)