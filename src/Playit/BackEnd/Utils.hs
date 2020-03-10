{- |
 * TAC auxiliary functions
 *
 * Copyright : (c) 
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.BackEnd.Utils where

import Data.Maybe
import Playit.FrontEnd.Types
import Playit.BackEnd.Types
import qualified Playit.BackEnd.TACType as T


-------------------------------------------------------------------------------
-- Creates the TAC's variable operand
tacVariable :: TACInfo -> TACOP
tacVariable v = Just $ T.Variable v
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Creates the TAC's constant operand
tacConstant :: (String, Type) -> TACOP
tacConstant c = Just $ T.Constant c
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Creates the TAC's label operand
tacLabel :: String -> TACOP
tacLabel l = Just $ T.Label l
-------------------------------------------------------------------------------


binOpToTACOP :: BinOp -> T.Operation
binOpToTACOP op = case op of
  Add       -> T.Add
  DivEntera -> T.Div
  Division  -> T.Div
  Minus     -> T.Sub
  Module    -> T.Mod
  Mult      -> T.Mult
  Greater   -> T.Gt
  GreaterEq -> T.Gte
  Less      -> T.Lt
  LessEq    -> T.Lte
  Eq        -> T.Eq
  NotEq     -> T.Neq


negation :: T.Operation -> T.Operation
negation op = case op of
  T.Lt  -> T.Gte
  T.Lte -> T.Gt
  T.Gt  -> T.Lte
  T.Gte -> T.Lt
  T.Eq  -> T.Neq
  T.Neq -> T.Eq


fall :: TACOP
fall = Just $ T.Label "-1"


isFall :: TACOP -> Bool
isFall (Just (T.Label l)) = l == "-1"
isFall _                  = error "Calling isFall with no label operand"


tac :: T.Operation -> TACOP -> TACOP -> TACOP -> [TAC]
tac op lv rv1 rv2 = [T.TACC op lv rv1 rv2]


-------------------------------------------------------------------------------
-- Creates the TAC's GoTo
tacGoto :: TACOP -> [TAC]
tacGoto label = [T.TACC T.GoTo Nothing Nothing label]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Creates the TAC's GoTo
tacNewLabel :: TACOP -> [TAC]
tacNewLabel label = [T.TACC T.NewLabel label Nothing Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacAssign :: TACOP -> TACOP -> [TAC]
tacAssign lv rv = [T.TACC T.Assign lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacMinus :: TACOP -> TACOP -> [TAC]
tacMinus lv rv = [T.TACC T.Minus lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacLen :: TACOP -> TACOP -> [TAC]
tacLen lv rv = [T.TACC T.Length lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacNew :: TACOP -> TACOP -> [TAC]
tacNew lv rv = [T.TACC T.New lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacNot :: TACOP -> TACOP -> [TAC]
tacNot lv rv = [T.TACC T.Not lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacDeref :: TACOP -> TACOP -> [TAC]
tacDeref lv rv = [T.TACC T.Deref lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacAdd :: TACOP -> TACOP -> TACOP -> [TAC]
tacAdd lv rv1 rv2 = [T.TACC T.Add lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacSub :: TACOP -> TACOP -> TACOP -> [TAC]
tacSub lv rv1 rv2 = [T.TACC T.Sub lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacMult :: TACOP -> TACOP -> TACOP -> [TAC]
tacMult lv rv1 rv2 = [T.TACC T.Mult lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacDiv :: TACOP -> TACOP -> TACOP -> [TAC]
tacDiv lv rv1 rv2 = [T.TACC T.Div lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacMod :: TACOP -> TACOP -> TACOP -> [TAC]
tacMod lv rv1 rv2 = [T.TACC T.Mod lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacGt :: TACOP -> TACOP -> TACOP -> [TAC]
tacGt lv rv1 rv2 = [T.TACC T.Gt lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacGte :: TACOP -> TACOP -> TACOP -> [TAC]
tacGte lv rv1 rv2 = [T.TACC T.Gte lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacLt :: TACOP -> TACOP -> TACOP -> [TAC]
tacLt lv rv1 rv2 = [T.TACC T.Lt lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacLte :: TACOP -> TACOP -> TACOP -> [TAC]
tacLte lv rv1 rv2 = [T.TACC T.Lte lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacEq :: TACOP -> TACOP -> TACOP -> [TAC]
tacEq lv rv1 rv2 = [T.TACC T.Eq lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacNeq :: TACOP -> TACOP -> TACOP -> [TAC]
tacNeq lv rv1 rv2 = [T.TACC T.Neq lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacIf :: TACOP -> TACOP -> [TAC]
tacIf cond label = [T.TACC T.If Nothing cond label]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- tacIfFalse :: TACOP -> TACOP -> [TAC]
-- tacIfFalse cond label = [T.TACC T.IfFalse Nothing cond label]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacSet :: TACOP -> TACOP -> TACOP -> [TAC]
tacSet x i y = [T.TACC T.Set x i y]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacGet :: TACOP -> TACOP -> TACOP -> [TAC]
tacGet x y i = [T.TACC T.Get x y i]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacCall :: TACOP -> String -> Int -> [TAC]
tacCall lv s n = [T.TACC T.Call lv (tacLabel s) (tacConstant (show n,TInt))]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacParam :: TACOP -> TAC
tacParam p = T.TACC T.Param Nothing p Nothing
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Modify the symbol offset
modifyOffSet :: TACOP -> OffSet -> TACOP
modifyOffSet (Just (T.Variable (Temp n _))) newO      = tacVariable $ Temp n newO
modifyOffSet (Just (T.Variable (TACVar info _))) newO = tacVariable $ TACVar info newO
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Gets the with of the Type
getWidth :: Type -> Int
getWidth array@(TArray _ t) = 4 * getWidth t
getWidth TBool              = 1
getWidth TChar              = 1
getWidth TFloat             = 8
getWidth TInt               = 4
getWidth (TList t)          = getWidth t -- ?
getWidth TListEmpty         = 4 -- como base?
getWidth (TNew n)           = 4 -- Reg or Union
getWidth (TPointer t)       = 4 * getWidth t -- Una palabra del procesador
getWidth TNull              = 4 -- Reservemos ese espacio asi igual
-- getWidth TStr               = 1 -- Se calcula cuando se tiene el valor del string
-- getWidth TRegister          = 1 -- Suma de los witdth de cada campo. Esto esta en su offset
-- getWidth TUnion             = 1 -- witdth mayor. Esto esta en su offset
getWidth _                  = -1 -- This shouldn't happen
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
lenArray :: Expr -> Int
lenArray (ArrayList ls _) = length ls
lenArray _                = 1
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
getAST :: [ExtraInfo] -> InstrSeq
getAST []        = []
getAST (AST i:_) = i
getAST (_:rs)    = getAST rs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- getRefVar :: Var -> Var
-- getRefVar (Desref v _) = v
-- getRefVar var          = var
-------------------------------------------------------------------------------

