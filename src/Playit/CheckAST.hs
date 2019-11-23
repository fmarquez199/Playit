{- |
 * Checks the AST types
 *
 * Copyright : (c)
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.CheckAST where

import Control.Monad.Trans.RWS
import Control.Monad (when)
import qualified Data.Map as M
import Data.Maybe (isJust,fromJust)
import Playit.AuxFuncs
import Playit.Errors
import Playit.SymbolTable
import Playit.Types
import Playit.TPDummyHandler


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                        Verificar tipos del AST
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the type of the index(ed) expression / variable.
checkIndex :: Var -> Type -> Pos -> Pos -> MonadSymTab (Bool, Type)
checkIndex var tExpr pVar pExpr
  | typeVar var == TError = do
    fileCode <- ask
    error $ semmErrorMsg "Array or List" "Type Error" fileCode pVar
  | tExpr /= TInt = do
    fileCode <- ask
    error $ semmErrorMsg "Power" (show tExpr) fileCode pExpr
  | otherwise = return (True, baseTypeArrLst (typeVar var))
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks that desref is to a pointer
checkDesref :: Type -> Pos -> MonadSymTab (Bool, Type)
checkDesref tVar p
  | isPointer tVar = let (TPointer t) = tVar in return (True, t)
  | otherwise = do
    fileCode <- ask
    error $ errorMsg "This is not a pointer" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the new defined type
-- TODO: Faltaba algo, pero no recuerdo que
checkNewType :: Id -> Pos -> MonadSymTab Bool
checkNewType tName p = do
  (symTab@(SymTab st), scopes, _, _) <- get
  fileCode <- ask
  let infos = lookupInScopes [1] tName symTab

  if isJust infos then do
    let symIndex = M.findIndex tName st
        sym = head . snd $ M.elemAt symIndex st -- its already checked that's not redefined

    if getCategory sym == TypeConstructors then return True
    else
      error $ errorMsg ("This isn't a defined type\n"++tName++"\n"++show sym) fileCode p
  else
    error $ errorMsg "Type not defined" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Cheacks the assginations type in declarations
checkAssigs :: InstrSeq -> Type -> Pos -> MonadSymTab InstrSeq
checkAssigs assigs t p
  | eqAssigsTypes updatedAssigs t = return updatedAssigs
  | otherwise = do
    fileCode <- ask
    error $ errorMsg ("Assignations expressions types isn't "++show t++"\n"++show assigs) fileCode p

  where
    updatedAssigs = map (changeTDummyAssigs t) assigs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the assignation's types
checkAssig :: Type -> Expr -> Pos -> MonadSymTab Bool
checkAssig tLval expr p
  | isRead || isNull || isInitReg || (tExpr == tLval) || isLists = return True
  | tExpr == TPDummy =
    updateExprPromiseType expr tLval >> return True
  | otherwise = do
    fileCode <- ask
    error $ semmErrorMsg (show tLval) (show tExpr) fileCode p

  where
    tExpr = typeE expr
    isEmptyList = isList tExpr && baseTypeT tExpr == TDummy
    isListLval  = isList tLval && isSimpleType (baseTypeT tLval)
    isLists = isEmptyList && isListLval
    isRead = tExpr == TRead
    isNull = tExpr == TNull
    isInitReg = tExpr == TRegister
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Checks if var is an Iteration's Variable.
checkIterVar :: Var -> MonadSymTab Bool
checkIterVar var = do
  (symtab, _, scope, _) <- get
  let cc s = getCategory s == IterationVariable && getScope s == scope
      name = getName var
      cat = filter cc $ fromJust (lookupInSymTab name symtab)

  return $ not $ null cat
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Checks the binary's expression's types
checkBinary :: BinOp -> Expr -> Expr -> Pos -> MonadSymTab (Bool, Type)
checkBinary op e1 e2 p
  | op `elem` eqOps && (isNull || isLists || isRegUnions) = return (True, TBool)
  -- Just compare register with eq operators
  | isRegUnion tE1 || isRegUnion tE2 = do
    fileCode <- ask
    error $ semmErrorMsg notRegUnion regUnion fileCode p
  
  | op `elem` compOps && tE1 == tE2 && noTError = return (True, TBool)

  | op `elem` compOps && tE1 == TPDummy && tE2 /= TPDummy =
    updateExprPromiseType e1 tE2 >> return (True, TBool)

  | op `elem` compOps && tE1 /= TPDummy && tE2 == TPDummy =
    updateExprPromiseType e2 tE1 >> return (True, TBool)

  | op `elem` boolOps && tE1 == tE2 && tE1 == TBool = return (True, TBool)
  | op `elem` aritInt && tE1 == tE2 && tE1 == TInt = return (True, TInt)
  | op == Concat && tE1 == tE2 && isList tE1 && isJust tList =
    return (True, fromJust tList)
  
  -- Add this isSubType tE1 tE2 ??
  | op == Anexo && baseT1 == baseT1 && isList tE2 = return (True, tE2)
  | tE1 == tE2 && (tE1 == TInt || tE1 == TFloat) = return (True, tE1)

  | tE1 == TPDummy && tE2 /= TPDummy =
    updateExprPromiseType e1 tE2 >> return (True, tE1)

  | tE1 /= TPDummy && tE2 == TPDummy =
    updateExprPromiseType e2 tE1 >> return (True, tE2)

  | otherwise = do
    fileCode <- ask
    error $ semmErrorMsg (show tE1) (show tE2) fileCode p

  where
    tE1 = typeE e1
    tE2 = typeE e2
    tE2' = baseTypeT tE2
    baseT1 = baseTypeE e1
    baseT2 = baseTypeE e2
    tList = getTLists [t1,t2]
    eqOps = [Eq,NotEq]
    compOps = [Eq,NotEq,Greater,GreaterEq,Less,LessEq]
    aritInt = [DivEntera, Module]
    boolOps = [And, Or]
    noTError = tE1 /= TError && tE2 /= TError -- TODO : Agregar a las comparaciones cuando no salga en el primer error
    isRegUnios = isRegUnion tE1 || isRegUnion tE2
    isLists = isList tE1 && isList tE2 && isJust (getTLists [tE1,tE2])
    isNull = ((isPointer tE1 && tE2 == TNull) || (tE1 == TNull && isPointer tE2 )) || (tE1 == TNull && tE2 == TNull)
    notRegUnion = "Neither Register nor Union"
    regUnion = "Register or Union"
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the unary's expression type is the spected
checkUnary :: Type -> Type -> Pos -> MonadSymTab (Bool, Type)
checkUnary tExpr tSpected p
  | tExpr == TDummy = return (True, TDummy)
  | tExpr == tSpected = return (True, tExpr)
  | otherwise = do
    fileCode <- ask
    error $ semmErrorMsg (show tSpected) (show tExpr) fileCode p
-------------------------------------------------------------------------------
{-
  case op of
    Length ->
      if isArray tE || isList tE then
        return $ Unary op expr TInt
      else do
        (error $ semmErrorMsg "Array or Kit" (show tE) fileCode p)
        return $ Unary op expr TError
    Negative ->
      if tE `elem` [TInt, TFloat] then
        return $ Unary op expr tE
      else do
        (error $ semmErrorMsg "Power or Skill" (show tE) fileCode p)
        return $ Unary op expr TError
    Not ->
      if tE == TBool then
        return $ Unary op expr tE
      else do
        (error $ semmErrorMsg "Battle" (show tE) fileCode p)
        return $ Unary op expr TError
    UpperCase ->
      if tE == TChar then
        return $ Unary op expr tE
      else do
        (error $ semmErrorMsg "Rune" (show tE) fileCode p)
        return $ Unary op expr TError
    LowerCase -> 
      if tE == TChar then
        return $ Unary op expr tE
      else do
        (error $ semmErrorMsg "Rune" (show tE) fileCode p)
        return $ Unary op expr TError
  where
    tE = typeE expr
-}

{-
crearOpAnexo ::  Expr -> Expr -> Posicion-> MonadSymTab Expr
crearOpAnexo e1 e2 p
    | isJust typeLR =
        return $ OpBinario Anexo e1 e2 (fromJust typeLR)

    | not $ isList typee2  = do
        file <- ask
        error $ "\n\nError: " ++ file ++ ": " ++ show p ++ "\n\t" ++
            "El segundo operando de : '" ++ show Anexo ++ "'," ++
            "'" ++ show e2 ++ "' debe ser una lista."
    | typee1 /= typeArrLst typee2  = do
        file <- ask
        error $ "\n\nError: " ++ file ++ ": " ++ show p ++ "\n\t" ++
            "El emento a anexar '" ++ show e1 ++ "'," ++ "' debe ser de tipo '"
            ++ show (typeArrLst typee2) ++ "'."
    where
        typee1 = typeE e1
        typee2 = typeE e2
        typeLR = getTListAnexo typee1 typee2
-}

-------------------------------------------------------------------------------
checkIfSimple :: Type -> Type -> Type -> Pos -> MonadSymTab (Bool, Type)
checkIfSimple tCond tTrue tFalse p
    | tCond == TBool && tFalse == tTrue = return (True, tTrue)
    -- | tCond == TPDummy && tFalse == tTrue = return (True, tTrue)
    | otherwise = do
      fileCode <- ask
      
      if tCond /= TBool then
        error $ semmErrorMsg "Battle" (show tCond) fileCode p
      else
        error $ semmErrorMsg (show tTrue) (show tFalse) fileCode p
-------------------------------------------------------------------------------
{-
  if tC == TBool && tT == tF then
    return $ IfSimple cond true false tT
  else do
    if tC /= TBool then
      error $ semmErrorMsg "Battle" (show tC) fileCode p
    else
      error $ semmErrorMsg (show tT) (show tF) fileCode p
    return $ IfSimple cond true false TError
  where
    tC = typeE cond
    tT = typeE true
    tF = typeE false
--------------------
  fileCode <- ask
  if tC == TBool && tT == tF then
      return $ IfSimple cond true false tT
  else do
      if tC /= TBool then
          error $ semmErrorMsg "Battle" (show tC) fileCode pc
      else
          error $ semmErrorMsg (show tT) (show tF) fileCode p2
      return $ IfSimple cond true false TError
  where
      tC = typeE cond
      tT = typeE true
      tF = typeE false
-}

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--       Cambiar el tipo 'TDummy' cuando se lee el tipo de la declacion
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


changeTDummyList :: Type -> Type-> Type
changeTDummyList (TList TDummy) newT = TList newT
changeTDummyList (TList t) newT      = TList (changeTDummyList t newT)
changeTDummyList t newT               = t

changeTPDummyFunction :: Expr -> Type -> Expr
changeTPDummyFunction (FuncCall c TPDummy) =  FuncCall c
-- Error: otherwise

-------------------------------------------------------------------------------
-- Cambia el TDummy de una variable en las declaraciones
changeTDummyLvalAsigs :: Var -> Type -> Var
changeTDummyLvalAsigs (Var n TDummy) t    = Var n t
changeTDummyLvalAsigs (Index var e t') t  =
    let newVar = changeTDummyLvalAsigs var t
    in Index newVar e t'
changeTDummyLvalAsigs (Desref var t') t   =
    let newVar = changeTDummyLvalAsigs var t
    in Desref newVar t'
changeTDummyLvalAsigs (Field var id t') t =
    let newVar = changeTDummyLvalAsigs var t
    in Field newVar id t'
changeTDummyLvalAsigs var _               = var
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Cambia el TDummy de las variables en las declaraciones
changeTDummyAssigs :: Type -> Instr -> Instr
changeTDummyAssigs t (Assig lval e _) =
    Assig (changeTDummyLvalAsigs lval t) (changeTRead e t) TVoid
-------------------------------------------------------------------------------

-- Cableado para que el input corra
changeTRead :: Expr -> Type -> Expr
changeTRead (Read e _) t = Read e t
changeTRead e _ = e


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--          Cambiar el tipo 'TDummy' en las instrucciones del 'for'
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


{--------------------------------------------------------------------------------
-- Cambia el TDummy de la variable de iteracion en el 'for'
changeTDummyLval :: Var -> Type -> Var
changeTDummyLval (Var n TDummy) t       = Var n t
changeTDummyLval var@(Var _ _) _       = var
changeTDummyLval (Index var e t') t  =
    let newE = changeTDummyExpr t e
    in Index var newE t'
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Cambia el TDummy de las expresiones en el 'for'
changeTDummyExpr :: Type -> Expr -> Expr
changeTDummyExpr t (Literal lit TDummy) = Literal lit t
--------------------------------------------------------------------------
changeTDummyExpr _ lit@(Literal _ _) = lit
--------------------------------------------------------------------------
changeTDummyExpr t (Variable var TDummy) =
    let newVar = changeTDummyLval var t
    in Variable newVar t
--------------------------------------------------------------------------
changeTDummyExpr _ vars@(Variable _ _) = vars
--------------------------------------------------------------------------
changeTDummyExpr t (ArrayList exprs TDummy) =
    let newExprs = map (changeTDummyExpr t) exprs
    in ArrayList newExprs t
--------------------------------------------------------------------------
changeTDummyExpr _ lst@(ArrayList _ _) = lst
--------------------------------------------------------------------------
changeTDummyExpr t (Unary op e TDummy) =
    let newE = changeTDummyExpr t e
    in Unary op newE t
--------------------------------------------------------------------------
changeTDummyExpr _ opUn@Unary{} = opUn
--------------------------------------------------------------------------
changeTDummyExpr t (Binary op e1 e2 TDummy) =
    let newE1 = changeTDummyExpr t e1
        newE2 = changeTDummyExpr t e2
    in Binary op newE1 newE2 t
--------------------------------------------------------------------------
changeTDummyExpr _ opBin@Binary{} = opBin
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Cambia el TDummy de la secuencia de instrucciones del 'for'
changeTDummyFor :: Type -> SymTab -> Scope -> Instr -> Instr
changeTDummyFor t symTab scope (Assig lval e _)
    | isVarIter lval symTab scope =
        error ("\n\nError semantico, la variable de iteracion: '" ++
               show lval ++ "', no se puede modificar.\n")
    | otherwise =
        let newLval = changeTDummyLval lval t
            newE = changeTDummyExpr t e
        in Assig newLval newE TVoid
--------------------------------------------------------------------------
-- changeTDummyFor t symTab scope (Program seqI) =
--     let newSeqI = map (changeTDummyFor t symTab scope) seqI
--     in Program newSeqI
--------------------------------------------------------------------------
changeTDummyFor t symTab scope (For name e1 e2 seqI _) =
    let newE1 = changeTDummyExpr t e1
        newE2 = changeTDummyExpr t e2
        newSeqI = map (changeTDummyFor t symTab scope) seqI
    in For name newE1 newE2 newSeqI TVoid
--------------------------------------------------------------------------
--changeTDummyFor t symTab scope (ForEach name e1 e2 e3 --seqI st) =
    --let newE1 = changeTDummyExpr t e1
--        newE2 = changeTDummyExpr t e2
--        newE3 = changeTDummyExpr t e3
--        newSeqI = map (changeTDummyFor t symTab scope) seqI
--    in ForEach name newE1 newE2 newE3 newSeqI st
--------------------------------------------------------------------------
changeTDummyFor t symTab scope (While e seqI _) =
    let newE = changeTDummyExpr t e
        newSeqI = map (changeTDummyFor t symTab scope) seqI
    in While newE newSeqI TVoid
--------------------------------------------------------------------------
{-changeTDummyFor t symTab scope (If e seqI) =
    let newE = changeTDummyExpr t e
        newSeqI = map (changeTDummyFor t symTab scope) seqI
    in If newE newSeqI
--------------------------------------------------------------------------
changeTDummyFor t symTab scope (IfElse e seqI1 seqI2) =
    let newE = changeTDummyExpr t e
        newSeqI1 = map (changeTDummyFor t symTab scope) seqI1
        newSeqI2 = map (changeTDummyFor t symTab scope) seqI2
    in IfElse newE newSeqI1 newSeqI2 -}
--------------------------------------------------------------------------
changeTDummyFor t symTab scope (Print [e] _) =
    let newE = changeTDummyExpr t e
    in Print [newE] TVoid
-------------------------------------------------------------------------------
-}