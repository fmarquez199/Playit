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
import Control.Monad (when,unless)
import qualified Data.Map as M
import Data.Maybe (isJust,fromJust,fromMaybe)
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
checkIndex :: Var -> Type -> Pos -> Pos -> FileCodeReader -> (Type, String)
checkIndex var tExpr pVar pExpr fileCode
  | typeVar var == TError =
    (TError, semmErrorMsg "Array or List" "Type Error" fileCode pVar)

  | tExpr /= TInt =
    (TError, semmErrorMsg "Power" (show tExpr) fileCode pExpr)

  | otherwise = (baseTypeArrLst (typeVar var), "")
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks that desref is to a pointer
checkDesref :: Type -> Pos -> FileCodeReader -> (Type, String)
checkDesref tVar p fileCode
  | isPointer tVar = let (TPointer t) = tVar in (t, "")
  | otherwise = (TError, errorMsg "This is not a pointer" fileCode p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the new defined type
-- TODO: Faltaba algo, pero no recuerdo que
checkNewType :: Id -> Pos -> SymTab -> FileCodeReader -> MonadSymTab String
checkNewType tName p symTab@(SymTab st) fileCode = do
  (symTab, activeScopes, scopes, promises) <- get
  let
    infos = lookupInScopes [1] tName symTab
  
  if isJust infos then

    let symIndex = M.findIndex tName st
        sym = head . snd $ M.elemAt symIndex st -- its already checked that's not redefined
    in
      if getCategory sym == TypeConstructors then return ""
      else
        return (errorMsg ("This isn't a defined type\n"++tName++"\n"++show sym) fileCode p)
  else do
    let
      promise = getPromise tName promises

    unless (isJust promise) $ do
      -- Create the promise
      let 
        newProm     = PromiseUserDefinedType tName  p
        newPromises = promises ++ [newProm]

      put (symTab, activeScopes, scopes, newPromises)

    return ""
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Cheacks the assginations type in declarations
checkAssigs :: [(Instr,Pos)] -> Type -> FileCodeReader -> MonadSymTab (InstrSeq, String)
checkAssigs assigs t fileCode
  | eqAssigsTypes updatedAssigs t = do
    -- Actualiza los TPDummys
    nexprs <- mapM (\(Assig _ e _) -> updateExpr e t) onlyassigs    
    return ([Assig v ne ta | (Assig v e ta,ne) <- zip updatedAssigs nexprs ],"")

  | otherwise = do
    let
      le  = map (\(Assig _ e _,p) -> (e,p)) assigs
      got = head $ dropWhile (\(e,p) ->  isJust (getTLists [typeE e,t])) le
      msg = semmErrorMsg (show t) (show $ typeE $ fst got) fileCode (snd got)

    return (updatedAssigs, msg)

  where
    onlyassigs  = map fst assigs
    updatedAssigs = map (changeTDummyAssigs t) onlyassigs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the assignation's types
checkAssig :: Type -> Expr -> Pos -> FileCodeReader -> String
checkAssig tLval expr p fileCode
  | isRead {-|| isNull-} || isInitReg || (tExpr == tLval) || isLists = ""
  | isArray tLval && isArray tExpr =
    -- Si son arrays y arrays del mismo tipo 
    --- TODO:  Faltaría verificar que tienen el mismo tamaño para arrays con expresiones no literales
    --          Ejemplo Power|)2(|  ==  Power|)1+1(|
    if isJust tarrays && e1 == e2 then "" else msg
  
  | otherwise = msg

  where
    tExpr       = typeE expr
    -- isEmptyList = isList tExpr && baseTypeT tExpr == TDummy
    -- isListLval  = isList tLval && isSimpleType (baseTypeT tLval)
    isLists     = {-isEmptyList && isListLval &&-} isJust (getTLists [tLval,tExpr])
    isRead      = tExpr == TRead
    isNull      = tExpr == TNull
    isInitReg   = tExpr == TRegister
    msg         = semmErrorMsg (show tLval) (show tExpr) fileCode p
    (TArray e1 _) = tLval
    (TArray e2 _) = tExpr
    tarrays        = getTLists [baseTypeArrLst tLval,baseTypeArrLst tExpr]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- types ok, cantidad == #campos
checkRegUnion :: Id -> [Expr] -> SymTab -> FileCodeReader -> String
checkRegUnion name exprs symTab fileCode
  | noErr && isJust reg && (isRegister || isUnion) = ""
  | noErr && isJust reg && not isUnion             = msgNoUnion
  | noErr && isJust reg && not isRegister          = msNroFields
  | noErr && isJust reg                            = msgBadTypes
  | noErr                                          = msgUndefined
  | otherwise                                      = msgTError

  where
    reg          = lookupInSymTab name symTab
    noErr        = TError `notElem` map typeE exprs
    (Params p)   = getExtraInfo (head $ fromJust reg) !! 1
    typesE       = map typeE exprs
    typesP       = map fst p
    typesOk      = null typesE || typesE == typesP
    nroFieldsOK  = null exprs || length exprs == length p
    isRegister   = typesOk && nroFieldsOK
    isUnion      = any isRegUnion typesE && length exprs == 1
    msgNoUnion   = name ++ " is an union" 
    msNroFields = "Mismatched ammount of fields initialized for " ++ name
    msgBadTypes  = "Mismatched types initializating " ++ name
    msgUndefined = "Undefined register or union " ++ name
    msgTError    = "Type error in initialization expressions " ++ show exprs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks if var is an Iteration's Variable.
checkIterVar :: Var -> MonadSymTab Bool
checkIterVar var = do
  (symtab, _, scope, _) <- get
  let cc s = getCategory s == IterationVariable && getScope s == scope
      name = getName var
      cat  = filter cc $ fromJust (lookupInSymTab name symtab)

  return $ not $ null cat
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the binary's expression's types
checkBinary :: BinOp -> (Expr,Pos) -> (Expr,Pos) -> MonadSymTab (Expr,String)
checkBinary op (e1,p1) (e2,p2) = do
  fileCode <- ask
  let
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
    isLists     = isList tE1 && isList tE2 && isJust (getTLists [tE1,tE2])
    nullE1      = tE1 == TNull && isPointer tE2
    nullE2      = isPointer tE1 && tE2 == TNull
    isNull      = (nullE1 || nullE2) || (tE1 == TNull && tE2 == TNull)
    notRegUnion = "Neither Register nor Union"
    regUnion    = "Register or Union"
    powerSkill  = "Power or Skill"
    msgTE12P2   = semmErrorMsg (show tE1) (show tE2) fileCode p2
    msgTE21P1   = semmErrorMsg (show tE2) (show tE1) fileCode p1
    msgComp     = semmErrorMsg "Tipo comparable" (show tE1) fileCode p1

  if tE1 == tE2 then
    case op of
      x | x `elem` compOps ->
        if op `elem` eqOps then 
          if isTypeComparableEq tE1 then return (comp, "")
          else 
            if tE1 == TPDummy then
              let related = getRelatedPromises comp
              in addLateCheck comp comp [p1,p2] related >> return (comp, "")
            else
              if tE1 == TNull then return (comp, "")
              else
                return (err, msgComp)

        else 
          if tE1 == TInt || tE1 == TFloat then return (comp, "")
          else
            if tE1 == TPDummy then
              let related = getRelatedPromises comp
              in addLateCheck comp comp [p1,p2] related >> return (comp, "")
            else
              return (err, semmErrorMsg "Power or Skill" (show tE1) fileCode p1)

      x | x `elem` aritOps ->
          if tE1 == TInt || tE2 == TFloat then return (arit, "")
          else 
            if tE1 == TPDummy then
              let related = getRelatedPromises arit
              in addLateCheck arit arit [p1,p2] related >> return (arit, "")
            else 
              return (err, semmErrorMsg powerSkill (show tE1) fileCode p1)

      x | x `elem` aritInt ->
          if tE1 == TInt then return (arit, "")
          else 
            if tE1 == TPDummy then do
              ne1 <- updateExpr e1 TInt
              ne2 <- updateExpr e2 TInt
              return (Binary op ne1 ne2 TInt, "")
            else
              return (err, semmErrorMsg "Power" (show tE1) fileCode p1)

      x | x `elem` boolOps ->
          if tE1 == TBool then return (comp, "")
          else
            if tE1 == TPDummy then do
              ne1 <- updateExpr e1 TBool
              ne2 <- updateExpr e2 TBool
              return (Binary op ne1 ne2 TBool, "")
            else
              return (err, semmErrorMsg "Battle" (show tE1) fileCode p1)

  else -- Tipos distintos
    if op `elem` eqOps then
      if isTypeComparableEq tE1 && tE2 == TPDummy then do
        ne2 <- updateExpr e2 tE1
        let
          expr = Binary op e1 ne2 TBool
          allidsp = getRelatedPromises expr

        unless (null allidsp ) $
          addLateCheck expr expr [p1,p2] allidsp
        return (expr,"") 

      else 
        if tE1 == TPDummy && isTypeComparableEq tE2 then do
          ne1 <- updateExpr e1 tE2
          let
            expr = Binary op ne1 e2 TBool
            allidsp = getRelatedPromises expr

          unless (null allidsp ) $
            addLateCheck expr expr [p1,p2] allidsp
          return (expr,"")
        
        else 
          if isNull then
            -- TODO: Falta TDUmmy adentro de  punteros
            return (comp, "")
          else 
              if tE1 == TPDummy && tE2 == TNull then do
                ne1 <- updateExpr e1 (TPointer TPDummy)
                -- TODO: Falta manejar apuntador a TDUmmy
                return (Binary op ne1 e2 TBool, "")
              else
                if tE1 == TNull && tE2 == TPDummy then do
                  ne2 <- updateExpr e2 (TPointer TPDummy)
                  -- TODO: Falta manejar apuntador a TDUmmy
                  return (Binary op e1 ne2 TBool, "")
                else
                    if isTypeComparableEq tE1 && not (isTypeComparableEq tE2) then
                      return (err,msgTE12P2)
                    
                    else
                      if not (isTypeComparableEq tE1) && isTypeComparableEq tE2 then
                        return (err,msgTE21P1)
                      
                      else
                        if isLists then
                          let
                            typel = fromJust $ getTLists [tE1,tE2] 
                            allidsp = getRelatedPromises comp
                          in
                          if not (null allidsp) then do
                            ne1 <- updateExpr e1 typel
                            ne2 <- updateExpr e2 typel
                            let 
                              newExpr = (Binary op ne1 ne2 TBool)
                              newRelated = getRelatedPromises newExpr
                            
                            when (not $ null  newRelated) $
                              addLateCheck newExpr newExpr [p1,p2] newRelated
                            return (newExpr, "")
                          else 
                            return (comp, "")
                
                        else
                          if isPointer tE1 && isPointer tE2 && isJust (getTLists [tE1,tE2]) then
                            let
                              typep = fromJust $ getTLists [tE1,tE2] 
                              allidsp = getRelatedPromises comp
                            in
                              if not (null allidsp) then do
                                ne1 <- updateExpr e1 typep
                                ne2 <- updateExpr e2 typep
                                let 
                                  newExpr = (Binary op ne1 ne2 TBool)
                                  newRelated = getRelatedPromises newExpr
                                
                                when (not $ null  newRelated) $
                                  addLateCheck newExpr newExpr [p1,p2] newRelated
                                return (newExpr, "")
                              else
                                return (comp, "")
                          else
                            if isTypeComparableEq tE1 && isTypeComparableEq tE2 then
                              return (err,msgTE12P2)
                            else -- TODO :Faltan arrays compatibles
                              return (err,msgComp)

    else 
      if op `elem` compOps || op `elem` aritOps then
        if isTypeNumber tE1 &&  tE2 == TPDummy then do
          ne2 <- updateExpr e2 tE1
          if op `elem` compOps then return (Binary op e1 ne2 TBool, "")
          else return (Binary op e1 ne2 tE1, "")

        else 
          if tE1 == TPDummy &&  isTypeNumber tE2 then do
            ne1 <- updateExpr e1 tE2
            if op `elem` compOps then
              return (Binary op ne1 e2 TBool, "")
            else
              return (Binary op ne1 e2 tE2, "")
          else 
            if isTypeNumber tE1 && not (isTypeNumber tE2) then return (err,msgTE12P2)
            else 
              if not (isTypeNumber tE1) &&  isTypeNumber tE2 then return (err,msgTE21P1)
              else  
                if isTypeNumber tE1  && isTypeNumber tE2 then return (err,msgTE12P2)
                else 
                  return (err,semmErrorMsg powerSkill (show tE1) fileCode p1)

      else 
        if op `elem` aritInt then
          if tE1 == TInt && tE2 == TPDummy then do
            ne2 <- updateExpr e2 tE1
            return (Binary op e1 ne2 tE1, "")
          else 
            if tE1 == TPDummy &&  tE2 == TInt then do
              ne1 <- updateExpr e1 tE2
              return (Binary op ne1 e2 tE2, "")
            else 
              if tE1 == TInt then return (err,msgTE12P2)
              else 
                if tE2 == TInt then return (err,msgTE21P1)
                else return (err,semmErrorMsg "Power" (show tE1) fileCode p1)
        
        else 
          if op `elem` boolOps then
            if tE1 == TBool && tE2 == TPDummy then do
              ne2 <- updateExpr e2 tE1
              return (Binary op e1 ne2 TBool, "")
            else 
              if tE1 == TPDummy && tE2 == TBool then do
                ne1 <- updateExpr e1 tE2
                return (Binary op ne1 e2 TBool, "")
              else 
                if tE1 == TBool then return (err,msgTE12P2)
                else 
                  if tE2 == TBool then return (err,msgTE21P1)
                  else return (err,semmErrorMsg "Battle" (show tE1) fileCode p1)
          else 
            error $ "Internal error checkbinary: "  ++ show op
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the unary's expression type is the spected
checkUnary :: UnOp -> Type -> Type -> Pos -> FileCodeReader -> (Type, String)
checkUnary op tExpr tExpected p fileCode =
  case op of
    Length ->
      if isArray tExpr || isList tExpr || tExpr == TPDummy then (TInt, "")
      else
        (TError, semmErrorMsg "Array or Kit" (show tExpr) fileCode p)

    Negative ->
      if tExpr `elem` [TInt, TFloat, TPDummy] then (tExpr, "")
      else
        (TError, semmErrorMsg "Power or Skill" (show tExpr) fileCode p)

    _ | tExpr == TDummy   -> (TDummy, "")
      | tExpr == TPDummy  -> (TPDummy, "")
      | tExpr == tExpected -> (tExpr, "")
      | otherwise -> (TError, semmErrorMsg (show tExpected) (show tExpr) fileCode p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks 
checkAnexo :: (Expr,Pos) -> (Expr,Pos) -> FileCodeReader -> (Type, String)
checkAnexo (e1,p1) (e2,p2) fileCode
    | typeR /= TError = (typeR, "")
    | otherwise       = (TError, msg)

    where
        tE1   = typeE e1
        tE2   = typeE e2
        typeR = fromMaybe TError (getTLists [TList tE1,tE2])
        msg   = semmErrorMsg (show (TList tE1)) (show tE2) fileCode p2
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
checkIfSimple :: (Type,Pos) -> (Type,Pos) -> (Type,Pos) -> FileCodeReader -> (Type,String)
checkIfSimple (tCond,pCond) (tTrue,pTrue) (tFalse,pFalse) fileCode

  | tCond `elem` [TBool,TPDummy] && isJust tResult = (fromJust tResult, "")
  | tCond /= TBool = 
    (TError, semmErrorMsg "Battle" (show tCond) fileCode pCond)

  | isRealType tTrue && not (isRealType tFalse) =
    (TError, semmErrorMsg (show tTrue) (show tFalse) fileCode pFalse)

  | not (isRealType tTrue) && isRealType tFalse =
    (TError, semmErrorMsg (show tFalse) (show tTrue) fileCode pTrue)

  | otherwise = (TError, semmErrorMsg (show tTrue) (show tFalse) fileCode pFalse)

  where
    tResult = getTLists [tTrue, tFalse]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--       Cambiar el tipo 'TDummy' cuando se lee el tipo de la declacion
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


changeTDummyList :: Type -> Type-> Type
changeTDummyList (TList TDummy) newT = TList newT
changeTDummyList (TList t) newT      = TList (changeTDummyList t newT)
changeTDummyList t newT              = t

changeTPDummyFunction :: Expr -> Type -> Expr
changeTPDummyFunction (FuncCall c TPDummy) =  FuncCall c
-- Error: otherwise

-------------------------------------------------------------------------------
-- Cambia el TDummy de una variable en las declaraciones
changeTDummyLvalAsigs :: Var -> Type -> Var
changeTDummyLvalAsigs (Var n TDummy) t    = Var n t
changeTDummyLvalAsigs (Index var e t') t  = Index (changeTDummyLvalAsigs var t) e t'
changeTDummyLvalAsigs (Desref var t') t   = Desref (changeTDummyLvalAsigs var t) t'
changeTDummyLvalAsigs (Field var id t') t = Field (changeTDummyLvalAsigs var t) id t'
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