{- |
 * Maneja el tipo TPDummy que representa el tipo de retorno de una función que todavía
 no ha sido declarado,
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.PromisesHandler where


import Control.Monad.Trans.RWS
import Control.Monad (void,forM,forM_)
--import qualified Data.Map as M
import Data.Maybe (isJust,fromJust)
import Playit.AuxFuncs
import Playit.Errors
import Playit.SymbolTable
import Playit.Types



-------------------------------------------------------------------------------
updatePromiseTypeFunction :: Expr -> Type -> MonadSymTab ()
updatePromiseTypeFunction exprF t = do
  (symTab, activeScopes, scope,promises) <- get
  let name = case exprF of 
              (FuncCall (Call name _) _) -> name
              _ -> error "Internal error : FunctionCall doesn't have a name"
      promise = getPromiseSubroutine name promises
  
  if isJust promise then do
    let modifyTypePromise prom@(Promise id p _ pos) = 
            if id == name then Promise id p t pos else prom

    put(symTab, activeScopes, scope , map modifyTypePromise promises)
    updateType name 1 t
  else
    error $ "Internal error : Promise for '"  ++ name ++ "' not defined!"
-------------------------------------------------------------------------------

{-
    Dada una expresion cuyo el tipo  de sus llamadas a funciones no se ha decidido. 
    Ejemplo:
        // Donde funcion1 y funcion2 no se han declarado y no se sabe si es suma de enteros
        // o flotantes
        funcion1() + funcion2()
        
    Le asigna a las promesas de funcion1 y funcion2  el tipo de retorno t
-}
updateExprPromiseType :: Expr -> Type -> MonadSymTab Expr
updateExprPromiseType (Binary op e1 e2 TPDummy) t   = do
  ne1 <- updateExprPromiseType e1 t
  ne2 <- updateExprPromiseType e2 t
  return (Binary op e1 e2 t)
updateExprPromiseType (FuncCall (Call name args) TPDummy ) t   = do
  updateSubroutinePromiseType name t
  return (FuncCall (Call name args) t)
--updateExprPromiseType _ _   = return ()
    

{-
    Le asigna a una promesa de funcion el tipo de retorno pasado como argumento.
-}
updateSubroutinePromiseType :: Id -> Type -> MonadSymTab ()
updateSubroutinePromiseType name t = do
  (symTab, activeScopes, scope,promises) <- get
      
  let promise = getPromiseSubroutine name promises
  if isJust promise then do

    let promise' = fromJust promise
    let typePromise = getTypePromise promise'

    if typePromise == TPDummy then do
      let 
          modifyTypePromise prom@(Promise id p _ pos ch) = 
              if id == name then Promise id p t pos ch else prom

      put(symTab, activeScopes, scope , map modifyTypePromise promises)

      updateType name 1 t
      checkExpresionesPromise promise' t
      return ()

    else when typePromise /= t $ do
      -- error $ semmErrorMsg (show t) (show typePromise) fileCode p
      error "Internal error updateSubroutinePromiseType: IS THIS GONNA GET EXECUTED? LOL"
      return ()
  else do
    error $ "Internal error updateSubroutinePromiseType: Promise for '"  ++ name ++ "' is not defined!"
    return ()

  return ()

{-
Dada una expresion en donde aparecen llamadas a funciones con tipo de retorno indeterminados

se buscan todas esas llamadas y se les agrega una expresion que se debe chequear cuando 
se actualizen los datos de las llamadas a la funcion
-}
addCheckTailPromise :: Expr -> Expr -> [Pos] -> [Id] -> MonadSymTab ()
addCheckTailPromise (Binary op e1 e2 _) e lpos lids   = do
  addCheckTailPromise e1 e lpos lids
  addCheckTailPromise e2 e lpos lids

addCheckTailPromise (ArrayList exprs _ ) e lpos lids = 
  mapM_ (\e1 -> addCheckTailPromise e1 e lpos lids) exprs

addCheckTailPromise (FuncCall (Call name _) TPDummy ) e lpos lids =
  addSubroutinePromiseLateChecks name e lpos lids

addCheckTailPromise (FuncCall _ _ ) _ _ _   = return ()
addCheckTailPromise (Literal _ _) _ _ _ = return ()
addCheckTailPromise (Variable _ _) _ _ _ = return ()
addCheckTailPromise Null _ _ _ = return ()

{-
Dada una expresion regresa todos los identificadores de promesas a funciones asociadas
-}
getAllPromiseIdsFromExpr :: Expr -> [Id]
getAllPromiseIdsFromExpr (Binary _ e1 e2 _) = l3
  where
    le1 = getAllPromiseIdsFromExpr e1
    le2 = getAllPromiseIdsFromExpr e2
    l3  = le1 ++ le2
getAllPromiseIdsFromExpr (IfSimple e1 e2 e3 _)  = l4
  where
    le1 = getAllPromiseIdsFromExpr e1
    le2 = getAllPromiseIdsFromExpr e2
    le3 = getAllPromiseIdsFromExpr e3
    l4  = le1 ++ le2 ++ le3
getAllPromiseIdsFromExpr (Unary _ e _)  = getAllPromiseIdsFromExpr e
getAllPromiseIdsFromExpr (Read e _)   = getAllPromiseIdsFromExpr e
getAllPromiseIdsFromExpr (ArrayList expr _)  = concatMap getAllPromiseIdsFromExpr expr
getAllPromiseIdsFromExpr (FuncCall (Call name _) TPDummy )  = [name]
getAllPromiseIdsFromExpr (FuncCall _ _ )  = []
getAllPromiseIdsFromExpr (Literal _ _) = []
getAllPromiseIdsFromExpr (Variable _ _) = []
getAllPromiseIdsFromExpr Null = []


{-Actualiza una promesa agregandole un check que se realizara cuando se actualize su tipo 
de retorno-}
addSubroutinePromiseLateChecks :: Id -> Expr->[Pos] -> [Id] ->MonadSymTab ()
addSubroutinePromiseLateChecks name expr lpos lids = do
  (symTab, activeScopes, scope,promises) <- get
      
  let promise = getPromiseSubroutine name promises
  if isJust promise then do

    let promise' = fromJust promise
    
    let checks = getLateChecksPromise promise'
    let typePromise = getTypePromise promise'
    let nlids = filter (/= name) lids

    if (typePromise == TPDummy) && not (any (\c -> getLCPromiseExpr c == expr) checks) then do

      let 
          nlc = LateCheckPromiseSubroutine expr lpos nlids
          modifyTypePromise prom@(PromiseSubroutine id p t pos ch) = 
              if id == name then PromiseSubroutine id p t pos (ch ++ [nlc]) else prom

      put(symTab, activeScopes, scope , map modifyTypePromise promises)
      return ()

    else when typePromise /= TPDummy $ do
      error "Internal error AddSubroutinePromiseLateChecks : IS THIS GONNA GET EXECUTED? LOL I HOPE NOT... it shouldnt...right?"
      return ()
  else do
    error $ "Internal error AddSubroutinePromiseLateChecks : Promise for '"  ++ name ++ "' is not defined!"
    return ()

  return ()

{-
Chequea las expresiones de una promesa dado que se le actualizó su tipo
Se deben eliminar todos los checks debido a que solo se actualiza el tipo 
una sola vez
-}
checkExpresionesPromise :: Promise -> Type-> MonadSymTab ()
checkExpresionesPromise promise tr = do

  let 
      name    = getIdPromise promise
      params  = getParamsPromise promise
      checks  = getLateChecksPromise promise

  forM_ checks $ \(LateCheckPromiseSubroutine e1 lpos1 lids1) -> do

    (symTab, activeScopes, scope,promises) <- get
    let newcheck = LateCheckPromiseSubroutine (changeTPDummyFunctionInExpre name e1 tr) lpos1 lids1
    
    let ne1 = getLCPromiseExpr newcheck
    checkTypesLC ne1 lpos1
    
    lnp <- forM lids1 $ \idpl -> do
      let promise = getPromiseSubroutine idpl promises
      
      if isJust promise then do
        let
          (PromiseSubroutine id2 p2 t2 pos2 ch2) = fromJust promise
          nch2 = map (\lcp@(LateCheckPromiseSubroutine e2 lpos2 lids2) -> if e2 == e1 then LateCheckPromiseSubroutine ne1 lpos2 lids2 else  lcp) ch2
          np = PromiseSubroutine id2 p2 t2 pos2 nch2

        return [np]
      else return []

    let 
        lnp2 = concat lnp
        lnpids = map (\p -> (getIdPromise p , p)) lnp2
        isToUpdate p = any (\(id,_) -> id == getIdPromise p) lnpids
        promTpUpdate p = snd $ head $ filter (\(id,_) -> id == getIdPromise p) lnpids
        modifyTypePromise prom = if isToUpdate prom then promTpUpdate prom else prom

    put(symTab, activeScopes, scope , map modifyTypePromise promises)
  
  (symTab, activeScopes, scope,promises) <- get

  let 
    modifyTypePromise prom@(PromiseSubroutine id p t pos ch) = 
      if id == name then PromiseSubroutine id p t pos [] else prom

  put(symTab, activeScopes, scope , map modifyTypePromise promises)
  return ()


{-
    Cambia el tipo de retorno de la función en los contextos en los cuales aparece
-}
changeTPDummyFunctionInExpre :: Id  -> Expr -> Type -> Expr
changeTPDummyFunctionInExpre name f@(FuncCall (Call id args) TPDummy ) t  = if id == name then FuncCall (Call name args) t else f
changeTPDummyFunctionInExpre _ (FuncCall (Call id args) trf) _  = FuncCall (Call id args) trf
-- Solo sucede con Anexo o + , - , * , /
changeTPDummyFunctionInExpre name (Binary op e1 e2 TPDummy) t   = Binary op ne1 ne2 tb 
  where 
    ne1 = changeTPDummyFunctionInExpre name e1 t
    ne2 = changeTPDummyFunctionInExpre name e2 t
    te1 = typeE ne1
    te2 = typeE ne2
    tb  = case op of
            Anexo  -> error "Not implemented yet Anexo"
            _ | (te1 == TPDummy) || (te2 == TPDummy ) -> TPDummy 
              -- TODO: Falta caso anexo, concatenación
              | (te1 == TInt || te1 == TFloat) && te2 == te1 -> te1
              | otherwise -> TError

changeTPDummyFunctionInExpre name (Binary op e1 e2 tb) t   = Binary op ne1 ne2 tb
  where
    ne1 = changeTPDummyFunctionInExpre name e1 t
    ne2 = changeTPDummyFunctionInExpre name e2 t
changeTPDummyFunctionInExpre name (IfSimple e1 e2 e3 ti) t   = IfSimple ne1 ne2 ne3 ti
  where
    ne1 = changeTPDummyFunctionInExpre name e1 t
    ne2 = changeTPDummyFunctionInExpre name e2 t
    ne3 = changeTPDummyFunctionInExpre name e3 t
changeTPDummyFunctionInExpre name (Unary op e1 tu) t   = Unary op e1 tu
  where
    ne1 = changeTPDummyFunctionInExpre name e1 t
changeTPDummyFunctionInExpre name (Read e1 tr) t   = Read ne1 tr
  where
    ne1 = changeTPDummyFunctionInExpre name e1 t
changeTPDummyFunctionInExpre name (ArrayList expr ta) t   = ArrayList nexpr nta
  where
    nexpr = map (\e ->changeTPDummyFunctionInExpre name e t) expr
    mapTypes = map typeE nexpr
    -- TODO: Falta manejar TError en getTLists para multiples errore
    nta = case getTLists mapTypes of
            Just t ->  TList t
            Nothing -> TError
changeTPDummyFunctionInExpre name l@(Literal _ _) _   = l
changeTPDummyFunctionInExpre name v@(Variable _ _) _   = v
        


checkBinaryExpr :: BinOp -> Expr -> Pos-> Expr -> Pos -> MonadSymTab Bool
checkBinaryExpr op e1 p1 e2 p2 = do
    fileCode <- ask
    let te1 = typeE e1
    let te2 = typeE e2

    if te1 /= te2 then -- Si son distintos los tipos de las funciones
        if op `elem` comp_eqs then
          
            if isTypeComparableEq te1 &&  not (isTypeComparableEq te2) then
                error $ semmErrorMsg (show te1) (show te2) fileCode p2
            else if not (isTypeComparableEq te1) &&  isTypeComparableEq te2 then
                error $ semmErrorMsg (show te2) (show te1) fileCode p1
            else  if isTypeComparableEq te1 && isTypeComparableEq te2 then
                error $ semmErrorMsg (show te1) (show te2) fileCode p2
            else 
                error $ semmErrorMsg "Tipo comparable" (show te1) fileCode p1

        else if (op `elem` comparators ) || (op `elem` aritmetic )then
            if isTypeNumber te1 &&  not (isTypeNumber te2) then
                error $ semmErrorMsg (show te1) (show te2) fileCode p2
            else if not (isTypeNumber te1) &&  isTypeNumber te2 then
                error $ semmErrorMsg (show te2) (show te1) fileCode p1
            else  if isTypeNumber te1  && isTypeNumber te2 then
                error $ semmErrorMsg (show te1) (show te2) fileCode p2
            else 
                error $ semmErrorMsg "Power or Skill" (show te1) fileCode p1

        else if (op `elem` aritmetic_int then
            if te1 == TInt then
                error $ semmErrorMsg (show te1) (show te2) fileCode p2
            else 
              if (te2 == TInt) then
                error $ semmErrorMsg (show te2) (show te1) fileCode p1
              else
                  error $ semmErrorMsg "Power" (show te1) fileCode p1
        else if (op `elem` boolean ) then
            if te1 == TBool then
                error $ semmErrorMsg (show te1) (show te2) fileCode p2
            else if (te1 /= TBool) && (te2 == TBool) then
                error $ semmErrorMsg (show te2) (show te1) fileCode p1
            else
                error $ semmErrorMsg "Battle" (show te1) fileCode p1
        else if op == Anexo then
            -- TODO: Sin hacer
            error $ "Not implemented anexo with promises"
        else return ()
    else --- Si son iguales los tipos de las expresiones 
        if (op `elem` comparators ) || (op `elem` aritmetic ) then
            if ((te1 /= TInt) && (te1 /= TFloat)) then
                error $ semmErrorMsg "Power or Skill" (show te1) fileCode p1
            else return ()
        else if (op `elem` aritmetic_int ) then
            if te1 /= TInt then
                error $ semmErrorMsg "Power" (show te1) fileCode p1
            else return ()
        else if (op `elem` boolean ) then
            if te1 /= TBool then
                error $ semmErrorMsg "Battle" (show te1) fileCode p1
            else return ()
        else if op == Anexo then
            -- TODO: Sin hacer
            error $ "Not implemented anexo with promises"
        else return ()

    return True
    where
        comparators = [Eq, NotEq, GreaterEq, LessEq, Greater, Less]
        comp_eqs = [Eq, NotEq]
        aritmetic = [Add, Minus, Mult, Division]
        aritmetic_int = [DivEntera, Module]
        boolean = [And, Or]

{-Checkea que la expresion sea correcta-}
checkTypesLC :: Expr -> [Pos] -> MonadSymTab ()
checkTypesLC (Binary op e1 e2 _) pos   = do

    if (typeE e1) /= TPDummy && (typeE e2) /= TPDummy then do  -- En caso que alguno todavia no se haya leido/inferido no se checkea
        checkBinaryExpr op e1 (pos !! 0) e2 (pos !! 1)
        return ()
    else return ()
    return ()
checkTypesLC (ArrayList exprs _) pos   = do

    let
        mapTypes = map typeE exprs
        nta = getTLists mapTypes

    if not $ isJust nta then do
        fileCode <- ask
        let 
            exprsP = zip exprs pos
            l = filter (\(e,p) -> typeE e /= TDummy && typeE e /= TPDummy) exprsP
            (e,p) = head l
            expected = typeE e
            got = head $ dropWhile (\(e,p) ->  typeE e == expected) l
        error $ semmErrorMsg (show expected) (show $ typeE $ fst got) fileCode (snd got)
    else return ()