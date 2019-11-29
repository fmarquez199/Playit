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
import Playit.PromisesHandler


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

  if isJust infos then

    let symIndex = M.findIndex tName st
        sym = head . snd $ M.elemAt symIndex st -- its already checked that's not redefined
    in
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
  | tExpr == TPDummy = updateExpr expr tLval >> return True
  | otherwise = do
    fileCode <- ask
    error $ semmErrorMsg (show tLval) (show tExpr) fileCode p

  where
    tExpr = typeE expr
    isEmptyList = isList tExpr && baseTypeT tExpr == TDummy
    isListLval  = isList tLval && isSimpleType (baseTypeT tLval)
    isLists = isEmptyList && isListLval && isJust (getTLists [tLval,tExpr])
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
checkBinary op e1 e2 p = do
  fileCode <- ask

  if te1 == te2 then
    case op of
      x | x `elem` compOps ->
        if op `elem` eqOps then 

          if isTypeComparableEq te1 then return (True, Tbool)
          else 
            if te1 == TPDummy then
              let allidsp = getRelatedPromises comp
              in addLateCheck comp comp [p1,p2] allidsp >> return (True, Tbool)
            else
              return (False,TError) >> error (semmErrorMsg "Tipo comparable" (show te1) fileCode p1)

        else 
          if te1 == TInt || te1 == TFloat then return (True, Tbool)
          else
            if te1 == TPDummy then
              let allidsp = getRelatedPromises comp
              in addLateCheck comp comp [p1,p2] allidsp >> return (True, Tbool)
            else
              return (False,TError) >> error (semmErrorMsg "Power or Skill" (show te1) fileCode p1)

      x | x `elem` aritOps ->
          if te1 == TInt || te2 == TFloat then return arit
          else 
            if te1 == TPDummy then
              let allidsp = getRelatedPromises arit
              in addLateCheck arit arit [p1,p2] allidsp >> return arit
            else 
              return (False,TError) >> error (semmErrorMsg powerSkill (show te1) fileCode p1)

      x | x `elem` aritInt ->
          if te1 == TInt then return arit
          else 
            if te1 == TPDummy then do
              ne1 <- updateExpr e1 TInt
              ne2 <- updateExpr e2 TInt
              return (Binary op ne1 ne2 TInt)
            else
              return (False,TError) >> error (semmErrorMsg "Power" (show te1) fileCode p1)

      x | x `elem` boolOps ->
          if te1 == TBool then return (True, Tbool)
          else
            if te1 == TPDummy then do
              ne1 <- updateExpr e1 TBool
              ne2 <- updateExpr e2 TBool
              return (Binary op ne1 ne2 TBool)
            else
              return (False,TError) >> error (semmErrorMsg "Battle" (show te1) fileCode p1)

  else  -- TODO : Falta más manejo de TPDummy
    if (op `elem` eqOps ) then
      if isTypeComparableEq te1 &&  (te2 == TPDummy) then do
        ne2 <- updateExpr e2 te1
        return (Binary op e1 ne2 TBool)

      else 
        if te1 == TPDummy &&  isTypeComparableEq te2 then do
          ne1 <- updateExpr e1 te2
          return (Binary op ne1 e2 TBool)
        
        else 
          if ((isTypeComparableEq te1) &&  (not $ isTypeComparableEq te2)) then
            error $ semmErrorMsg (show te1) (show te2) fileCode p2
          
          else
            if ((not $ isTypeComparableEq te1) &&  (isTypeComparableEq te2)) then
              error $ semmErrorMsg (show te2) (show te1) fileCode p1
            
            else
              if (isList te1) && (isList te2)  && (isJust (getTLists [te1,te2])) then
                --ne1 <- updateExpr e1 te2
                -- TODO: Falta TDUmmy adentro de las listas
                return (Binary op e1 e2 TBool)
            
              else 
                if (isPointer te1 && te2 == TNull || te1 == TNull && isPointer te2) || (te1 == TNull && te2 == TNull) then
                  return (Binary op e1 e2 TBool)
                else 
                  if te1 == TPDummy && te2 == TNull then do
                    ne1 <- updateExpr e1 (TPointer TPDummy)
                    -- TODO: Falta manejar apuntador a TDUmmy
                    return (Binary op ne1 e2 TBool)
                  else 
                    if te1 == TNull && te2 == TPDummy then do
                      ne2 <- updateExpr e1 (TPointer TPDummy)
                      -- TODO: Falta manejar apuntador a TDUmmy
                      return (Binary op e1 ne2 TBool)
                    else  
                      if isTypeComparableEq te1 && isTypeComparableEq te2 then error $ semmErrorMsg (show te1) (show te2) fileCode p2
                      else -- TODO :Faltan arrays compatibles
                          error $ semmErrorMsg "Tipo comparable" (show te1) fileCode p1

    else 
      if (op `elem` compOps ) || (op `elem` aritOps )then
        if ((isTypeNumber te1) &&  (te2 == TPDummy)) then do
          ne2 <- updateExpr e2 te1
          if (op `elem` compOps) then return (Binary op e1 ne2 TBool)
          else return (Binary op e1 ne2 te1)

        else 
          if ((te1 == TPDummy) &&  (isTypeNumber te2)) then do
            ne1 <- updateExpr e1 te2
            if (op `elem` compOps) then
                return (Binary op ne1 e2 TBool)
            else
                return (Binary op ne1 e2 te1)
          else 
            if ((isTypeNumber te1) &&  (not $ isTypeNumber te2)) then error $ semmErrorMsg (show te1) (show te2) fileCode p2
            else 
              if ((not $ isTypeNumber te1) &&  (isTypeNumber te2)) then error $ semmErrorMsg (show te2) (show te1) fileCode p1
              else  
                if (isTypeNumber te1)  && (isTypeNumber te2) then error $ semmErrorMsg (show te1) (show te2) fileCode p2
                else 
                    error $ semmErrorMsg powerSkill (show te1) fileCode p1

      else 
        if (op `elem` aritInt ) then
          if (te1 == TInt) &&  (te2 == TPDummy) then do
            ne2 <- updateExpr e2 te1
            return (Binary op e1 ne2 te1)
          else 
            if ((te1 == TPDummy) &&  (te2 == TInt)) then do
              ne1 <- updateExpr e1 te2
              return (Binary op ne1 e2 te1)
            else 
              if te1 == TInt then error $ semmErrorMsg (show te1) (show te2) fileCode p2
              else 
                if te2 == TInt then error $ semmErrorMsg (show te2) (show te1) fileCode p1
                else error $ semmErrorMsg "Power" (show te1) fileCode p1
        
        else 
          if (op `elem` boolOps ) then
            if (te1 == TBool) &&  (te2 == TPDummy) then do
              ne2 <- updateExpr e2 te1
              return (Binary op e1 ne2 TBool)
            else 
              if ((te1 == TPDummy) &&  (te2 == TBool)) then do
                ne1 <- updateExpr e1 te2
                return (Binary op ne1 e2 TBool)
              else 
                if (te1 == TBool) then error $ semmErrorMsg (show te1) (show te2) fileCode p2
                else 
                  if (te2 == TBool) then error $ semmErrorMsg (show te2) (show te1) fileCode p1
                  else error $ semmErrorMsg "Battle" (show te1) fileCode p1
          else 
            if op == Anexo then
              -- TODO: Sin hacer anexo ni concatenación
              error $ "Not implemented anexos "
            else error $ "Internal error: Op not recognized "

  {-
    Anexo ->
      if isSubtype te1 te2 && (not $ isArray te2) then return anex
      else 
        return (False,TError) >> (error $ semmErrorMsg (show ste2) (show te1) fileCode p1)
  -}

  where
    tE1         = typeE e1
    tE2         = typeE e2
    tE2'        = baseTypeT tE2
    baseT1      = baseTypeE e1
    baseT2      = baseTypeE e2
    tList       = getTLists [tE1,tE2]
    eqOps       = [Eq,NotEq]
    compOps     = [Eq, NotEq, GreaterEq, LessEq, Greater, Less]
    aritOps     = [Add, Minus, Mult, Division]
    aritInt     = [DivEntera, Module]
    boolOps     = [And, Or]
    anexo       = Binary op e1 e2 tE2
    arit        = Binary op e1 e2 tE1
    comp        = Binary op e1 e2 TBool
    err         = Binary op e1 e2 TError
    noTError    = tE1 /= TError && tE2 /= TError -- TODO : Agregar a las comparaciones cuando no salga en el primer error
    isRegUnions = isRegUnion tE1 || isRegUnion tE2
    isLists = isList tE1 && isList tE2 && isJust (getTLists [tE1,tE2])
    isNull  = ((isPointer tE1 && tE2 == TNull) || (tE1 == TNull && isPointer tE2 )) || (tE1 == TNull && tE2 == TNull)
    notRegUnion = "Neither Register nor Union"
    regUnion    = "Register or Union"
    powerSkill  = "Power or Skill"
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks 
checkAnexo :: (Expr,Pos) -> (Expr,Pos) -> MonadSymTab (Bool, Type)
checkAnexo (e1,p1) (e2,p2)
    | typeR /= TError = return (True, typeR)
    | otherwise = do
        fileCode <- ask
        error $ semmErrorMsg (show (TList te1)) (show te2) fileCode p2

    where
        te1 = typeE e1
        te2 = typeE e2
        typeR = case getTLists [TList te1,te2] of
                Just t -> t
                Nothing -> TError
        
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the unary's expression type is the spected
checkUnary :: UnOp -> Type -> Type -> Pos -> MonadSymTab (Bool, Type)
checkUnary op tExpr tSpected p = do
  fileCode <- ask
  case op of
    Length ->
      if isArray tExpr || isList tExpr then return (True, TInt)
      else
        return (False, TError) >> error (semmErrorMsg "Array or Kit" (show tExpr) fileCode p)

    Negative ->
      if tExpr `elem` [TInt, TFloat] then return (True, tExpr)
      else
        return (False, TError) >> error (semmErrorMsg "Power or Skill" (show tExpr) fileCode p)

    _ | tExpr == TDummy -> return (True, TDummy)
      | tExpr == TPDummy -> return (True, TPDummy)
      | tExpr == tSpected -> return (True, tExpr)
      | otherwise ->
        return (False, TError) >> error (semmErrorMsg (show tSpected) (show tExpr) fileCode p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
checkIfSimple :: (Expr,Pos) -> (Expr,Pos) -> (Expr,Pos) -> FileCodeReader
              -> MonadSymTab (Bool,Type)
checkIfSimple (tCond,pCond) (tTrue,pTrue) (tFalse,pFalse)

  | tCond `elem` [TBool,TPDummy] && isJust tResult = return (True, fromJust tResult)
  | tCond /= TBool = 
    error $ semmErrorMsg "Battle" (show tCond) fileCode pCond

  | isRealType tTrue && not (isRealType tFalse) =
    error $ semmErrorMsg (show tTrue) (show tFalse) fileCode pFalse

  | not (isRealType tTrue) && isRealType tFalse =
    error $ semmErrorMsg (show tFalse) (show tTrue) fileCode pTrue

  | otherwise = error $ semmErrorMsg (show tTrue) (show tFalse) fileCode pFalse

  where tResult = getTLists [tTrue, tFalse]
-------------------------------------------------------------------------------


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