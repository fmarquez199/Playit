{- |
 * Maneja el tipo TPDummy que representa el tipo de retorno de una función que todavía
 no ha sido declarado,
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.FrontEnd.Promises.Handler where


import Control.Monad           (when,unless,forM)
import Control.Monad.Trans.RWS (get,put,tell,ask)
import Data.Maybe              (isJust,isNothing,fromJust,fromMaybe)
import Data.Set           as S (fromList,toList)
import Playit.FrontEnd.Utils
import Playit.FrontEnd.Errors
import Playit.FrontEnd.SymbolTable
import Playit.FrontEnd.Types



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                               Updates
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
{- |
  Dada una expresion cuyo tipo de sus llamadas a funciones no se ha decidido. 
  Les asigna un tipo a las llamadas a funciones en las expresiones
  Ejemplo:
    // Ejemplo funcion1 y funcion2 no se han declarado y no se sabe si es suma de
    // enteros o flotantes

    funcion1()(-> Undefined) + funcion2()(-> Undefined)

    updateExpr expr Int

    funcion1()(-> Int) + funcion2()(-> Int)
-}
updateExpr :: Expr -> Type -> MonadSymTab Expr
updateExpr (Unary op e TPDummy) t = do
  ne <- updateExpr e t
  return (Unary op ne (typeE ne))

updateExpr (Binary op e1 e2 TPDummy) t = do
  ne1 <- updateExpr e1 t
  ne2 <- updateExpr e2 t
  return (Binary op ne1 ne2 t)

updateExpr (Binary Anexo e1 e2 _) tl@(TList t) = do    
  ne1 <- updateExpr e1 t
  ne2 <- updateExpr e2 tl

  let ntr = fromMaybe TError (getTLists [TList (typeE ne1),typeE ne2])
  return (Binary Anexo ne1 ne2 tl)

updateExpr (Binary Concat e1 e2 _) tl@(TList t) = do    
  ne1 <- updateExpr e1 tl
  ne2 <- updateExpr e2 tl

  let ntr = fromMaybe TError (getTLists [typeE ne1,typeE ne2])
  return (Binary Concat ne1 ne2 ntr)
  
-- updateExpr (Binary op e1 e2 TBool) tl@(TList t) = do
--   ne1 <- updateExpr e1 t
--   ne2 <- updateExpr e2 t

--   return (Binary op ne1 ne2 TBool)

updateExpr (FuncCall (Call name args) tf) t = do
  SymTabState{symTab = st} <- get
  let
    nt  = if t == TNull then TPointer TDummy else t
    info = fromJust $ lookupInSymTab name st

  ntf <- updatePromise name nt tf
  return (FuncCall (Call name args) ntf)

updateExpr (ArrayList exprs _ ) tl@(TList t)  = do
  nexprs <- mapM (`updateExpr` t) exprs
  let
    mapTypes = map typeE nexprs
    ntr      = case getTLists mapTypes of
      Just nt -> TList nt
      Nothing -> TError

  return (ArrayList nexprs ntr)

updateExpr (ArrayList exprs _ ) tl@(TArray e t)  = do
  nexprs <- mapM (`updateExpr` t) exprs
  let
    mapTypes = map typeE nexprs
    ntr      = case getTLists mapTypes of
      Just nt -> TArray e nt
      Nothing -> TError

  return (ArrayList nexprs ntr)

updateExpr e _   = return e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Cambia el tipo de retorno de la función en los contextos en los cuales aparece
updatePromiseInExpr :: Id  -> Expr -> Type -> Expr
updatePromiseInExpr name f@(FuncCall (Call id args) _) t =
  if id == name then
    FuncCall (Call name args) t
  else f

-- Solo sucede con + , - , * , /
updatePromiseInExpr name (Binary op e1 e2 tb) t = Binary op ne1 ne2 ntb 
  where 
    ne1        = updatePromiseInExpr name e1 t
    ne2        = updatePromiseInExpr name e2 t
    tE1        = typeE ne1
    tE2        = typeE ne2
    isAritOp x = x `elem` [Add, Minus, Mult, Division]
    ntb        = case op of -- TODO: Falta caso  concatenación
      Anexo  -> fromMaybe TError (getTLists [TList tE1,tE2])
      Concat -> 
        if tE1 == TPDummy || tE2 == TPDummy then
          TPDummy
        else
          fromMaybe TError (getTLists [tE1,tE2])
      x | isAritOp x ->
        if tE1 == TPDummy || tE2 == TPDummy then TPDummy
        -- TODO : Faltan inferencias aquí para e2 o e1 si el otro es concreto
        else
          if (tE1 == TInt || tE2 == TFloat) && tE2 == tE1 then tE1 
          else TError
      _   -> tb 

updatePromiseInExpr name (IfSimple e1 e2 e3 ti) t = IfSimple ne1 ne2 ne3 ti
  where
    ne1 = updatePromiseInExpr name e1 t
    ne2 = updatePromiseInExpr name e2 t
    ne3 = updatePromiseInExpr name e3 t

updatePromiseInExpr name (Unary op e tr) t = Unary op ne ntr
  where
    ne  = updatePromiseInExpr name e t
    ntr = if tr == TPDummy then typeE ne else tr

updatePromiseInExpr name (Read e1 tr) t = Read (updatePromiseInExpr name e1 t) tr
updatePromiseInExpr name (ArrayList expr (TArray lexpr _)) t = ArrayList nexpr nta
  where
    nexpr    = map (\e ->updatePromiseInExpr name e t) expr
    mapTypes = map typeE nexpr
    nta      = case getTLists mapTypes of
      Just t ->  TArray lexpr t 
      Nothing -> TError

updatePromiseInExpr name (ArrayList expr (TList _)) t = ArrayList nexpr nta
  where
    nexpr    = map (\e ->updatePromiseInExpr name e t) expr
    mapTypes = map typeE nexpr
    nta      = case getTLists mapTypes of
      Just t ->  TList t 
      Nothing -> TError

updatePromiseInExpr _ l@(Literal _ _) _  = l
updatePromiseInExpr _ v@(Variable _ _) _ = v
updatePromiseInExpr _ Null _             = Null
updatePromiseInExpr _ e _                = error $ "e : " ++ show e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Le asigna a una funcion promesa el tipo de retorno pasado como argumento.
updatePromise :: Id -> Type -> Type -> MonadSymTab Type
updatePromise name t tf = do
  state@SymTabState{proms = promises} <- get
  let promise = getPromise name promises

  if isJust promise then do
    let
      promise'    = fromJust promise
      typePromise = promiseType promise'

    if not (isRealType typePromise) && isJust (getTLists [typePromise,t]) then do
      let modifyTypePromise prom = 
            if promiseId prom == name then 
              let (PromiseS id p _ cat pos ch ch2 ch3) = prom 
              in PromiseS id p t cat pos ch ch2 ch3
            else prom

      put state{proms = map modifyTypePromise promises}
      updateType name 1 t >> checkExpr promise' t
      checkExprCalls promise' t -- TODO: No se chequean cuando se infiere?
      checkExprForEach promise' t
      return t
    else return typePromise
  else return tf
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
updateInfoSubroutine:: Id -> Category -> [(Type,Id)] -> Type -> MonadSymTab ()
updateInfoSubroutine name cat p t = do
  state@SymTabState{proms = promises} <- get
  fileCode <- ask
  let paramsF = reverse p
      promise = getPromise name promises

  when (isJust promise) $ do
    let 
      promise' = fromJust promise
      paramsP  = promiseParams promise'
      typeP    = promiseType promise'
      c        = promiseCat promise'
      l        = zip paramsP paramsF
      posp     = promisePos promise'

    if any (/=True) [isJust (getTLists [t1,t2]) | ((t1,_),(t2,_)) <- l ] then do
      let               
        ((tgot,pgot),(texpected,_)) = head $ dropWhile (\((t1,_),(t2,_)) -> isJust (getTLists [t1,t2])) l
      tell [semmErrorMsg texpected tgot fileCode pgot]

    else

      if length paramsP /= length paramsF then

        let msj = "Amount of arguments: " ++ show (length paramsP) ++
                " not equal to expected:" ++ show (length paramsF)
        in tell [errorMsg msj fileCode posp]

      else
        if isNothing (getTLists [typeP,t]) then
          when (typeP `notElem` [TError,TPDummy] && t `notElem` [TError,TPDummy]) $
            tell [semmErrorMsg typeP t fileCode posp]
        else do
          checkExpr promise' t
          checkExprCalls promise' t -- TODO: No se chequean cuando se infiere?
          checkExprForEach promise' t
          put state{proms = filter (\p -> promiseId p /= name) promises}

  updateExtraInfoProm name cat [Params paramsF]
  updateType name 1 t >> updateCategory name 1 cat
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Dada una expresion en donde aparecen llamadas a funciones con tipo de retorno
-- indeterminados, se buscan todas esas llamadas y se les agrega una expresion
-- que se debe chequear cuando se actualizen los datos de las llamadas a la funcion
addLateCheck :: Expr -> Expr -> [Pos] -> [Id] -> MonadSymTab ()
addLateCheck (Binary _ e1 e2 _) e lpos lids =
  addLateCheck e1 e lpos lids >> addLateCheck e2 e lpos lids

addLateCheck (Unary _ e1 _) e lpos lids = addLateCheck e1 e lpos lids

addLateCheck (IfSimple e1 e2 e3 _) e lpos lids =
  addLateCheck e1 e lpos lids >> addLateCheck e2 e lpos lids >> addLateCheck e3 e lpos lids

addLateCheck (ArrayList exprs _ ) e lpos lids =
  mapM_ (\e1 -> addLateCheck e1 e lpos lids) exprs

addLateCheck (FuncCall (Call name _) tf ) e lpos lids =
  unless ( isRealType tf) $ 
    addLateChecks name e lpos lids

addLateCheck _ _ _ _  = return ()
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Dada una expresion regresa todos los identificadores de promesas a
-- funciones asociadas
getRelatedPromises :: Expr -> [Id]
getRelatedPromises (Binary _ e1 e2 _)    = l3
  where
    le1 = getRelatedPromises e1
    le2 = getRelatedPromises e2
    l3  = le1 ++ le2
getRelatedPromises (IfSimple e1 e2 e3 _) = l4
  where
    le1 = getRelatedPromises e1
    le2 = getRelatedPromises e2
    le3 = getRelatedPromises e3
    l4  = le1 ++ le2 ++ le3
getRelatedPromises (Unary _ e _)      = getRelatedPromises e
getRelatedPromises (Read e _)         = getRelatedPromises e
getRelatedPromises (ArrayList expr _) = concatMap getRelatedPromises expr
getRelatedPromises (FuncCall (Call name _) t) = [name | not $ isRealType t]
getRelatedPromises _                          = []
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
addLateCheckForEach :: Id -> Id -> Expr -> Pos -> [Id] -> MonadSymTab ()
addLateCheckForEach idp idvar expr p ids = do
  state@SymTabState{symTab = st, currS = s, proms = promises} <- get
  let
    infos   = lookupInScopes [s] idvar st
    promise = getPromise idp promises
    tlvar   = if isJust infos then
                let 
                  isVar symInfo = category symInfo == IterationVariable
                  v             = filter isVar (fromJust infos)
                in
                  symType (head v)
              else 
                TPDummy

  when (isJust promise ) $ do
    let
      nlids = filter (/= idp) ids
      nlc   = LateCheckPromForE expr idvar tlvar p nlids
      modifyTypePromise prom = 
        if promiseId prom == idp then 
          let (PromiseS id p t cat pos ch ch2 ch3) = prom 
          in PromiseS id p t cat pos ch ch2 (ch3 ++ [nlc]) 
        else prom

    put state{proms = map modifyTypePromise promises}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Actualiza una promesa agregandole un check que se realizara cuando se
-- actualize su tipo de retorno
addLateChecks :: Id -> Expr -> [Pos] -> [Id] -> MonadSymTab ()
addLateChecks name e lpos lids = do
  state@SymTabState{proms = promises} <- get
  let
    promise = getPromise name promises

  when (isJust promise) $ do
    let
      promise'    = fromJust promise
      checks      = promiseLateCheck promise'
      typePromise = promiseType promise'
      nlids       = filter (/= name) lids

    unless (isRealType typePromise || any (\c -> expr c == e) checks) $
      let 
        nlc = LateCheckPromS e lpos nlids
        modifyPromT prom@PromiseS{promiseId = pId, promiseLateCheck = ch} = 
          if pId == name then prom{promiseLateCheck = ch ++ [nlc]}
          else prom
      in
      put state{proms = map modifyPromT promises}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Cambia el tipo de retorno de la función en los contextos en los cuales aparece
-- updatePromiseInExpr :: Id  -> Expr -> Type -> Expr
-- updatePromiseInExpr name f@(FuncCall (Call id args) _) t =
--   if id == name then
--     FuncCall (Call name args) t
--   else f
-- -- Solo sucede con Anexo o + , - , * , /
-- updatePromiseInExpr name (Binary op e1 e2 tb) t = Binary op ne1 ne2 ntb 
--   where 
--     ne1        = updatePromiseInExpr name e1 t
--     ne2        = updatePromiseInExpr name e2 t
--     tE1        = typeE ne1
--     tE2        = typeE ne2
--     isAritOp x = x `elem` [Add, Minus, Mult, Division]
--     ntb        = case op of -- TODO: Falta caso  concatenación
--       Anexo  -> fromMaybe TError (getTLists [TList tE1,tE2])
--       Concat -> fromMaybe TError (getTLists [tE1,tE2])      
--       x | isAritOp x && (tE1 == TPDummy || tE2 == TPDummy) ->
--           TDummy
--           -- TODO : Faltan inferencias aquí para e2 o e1 si el otro es concreto
--         | isAritOp x && (tE1 == TInt || tE1 == TFloat) && tE2 == tE1 -> tE1
--         | otherwise -> TError
--       _   -> tb

-- updatePromiseInExpr name (IfSimple e1 e2 e3 ti) t = IfSimple ne1 ne2 ne3 ti
--   where
--     ne1 = updatePromiseInExpr name e1 t
--     ne2 = updatePromiseInExpr name e2 t
--     ne3 = updatePromiseInExpr name e3 t

-- updatePromiseInExpr name (Unary op e tr) t = Unary op ne ntr
--   where
--     ne  = updatePromiseInExpr name e t
--     ntr = if tr == TPDummy then typeE ne else tr

-- updatePromiseInExpr name (Read e1 tr) t        = Read (updatePromiseInExpr name e1 t) tr
-- updatePromiseInExpr name (ArrayList expr ta) t = ArrayList nexpr nta
--   where
--     nexpr = map (\e ->updatePromiseInExpr name e t) expr
--     mapTypes = map typeE nexpr
--     -- TODO: Falta manejar TError en getTLists para multiples errore
--     nta = case getTLists mapTypes of
--       Just t ->  TList t  -- TODO: Falta condicion para TArray
--       Nothing -> TError

-- updatePromiseInExpr _ l@(Literal _ _) _  = l
-- updatePromiseInExpr _ v@(Variable _ _) _ = v
-- updatePromiseInExpr _ Null _             = Null
-- updatePromiseInExpr _ e _                = error $ "e : " ++ show e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
getParamsSubroutineFromSymtab :: Id -> MonadSymTab [(Type, Id)]
getParamsSubroutineFromSymtab name = do
  SymTabState{symTab = st} <- get
  let
    symInfos = lookupInScopes [1,0] name st

  if isJust symInfos then
    let
      isSubroutine si = category si `elem` [Procedures, Functions]
      subroutine'     = filter isSubroutine (fromJust symInfos)
    in
      if not $  null subroutine' then
        return $ fromJust $ getParams $ extraInfo $ head subroutine'
      else
        error $ "internal error getParamsSubroutineFromSymtab:  " ++ name ++ " not a subroutine"
  else
    error $ "internal error getParamsSubroutineFromSymtab:  " ++ name ++ " doesn't exist"
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                               Checks
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
checkExprCalls :: Promise -> Type -> MonadSymTab ()
checkExprCalls promise tr = do 
  fileCode <- ask
  let 
    name    = promiseId promise
    checks  = otherCallsLateCheck promise

    -- params :: [(Expr,Pos)]
  newcheck' <- forM checks $ \lcp@(LateCheckPromCall (Call idPromiseCall paramsCall) relatedIds) -> do

    SymTabState{proms = promises} <- get
    -- Obtenemos los parametros que se tienen definidos para la función  
    defParamsCall <- getParamsSubroutineFromSymtab idPromiseCall

    -- Creamos la lista de nuevas expresiones , actualizadas con el nuevo tipo para la promesa
    let 
      listParams = [(t,updatePromiseInExpr name exprArg tr,pc) | ((exprArg,pc),(t,_)) <- zip paramsCall defParamsCall ]
      
    -- Vemos para cada nueva actualización si se cumple que la llamada a la función es correcta
    -- Dado la nueva expresión cno el tipo de la promesa cambiado
    newParamsCall <- forM listParams $ \(t,ne,p) -> do
      when (isNothing (getTLists [typeE ne,t])) $
        when (t `notElem` [TError,TPDummy] && typeE ne `notElem` [TError,TPDummy]) $
          tell [semmErrorMsg t (typeE ne) fileCode p]
      return (ne,p) 

    let 
      newcheck = LateCheckPromCall (Call idPromiseCall newParamsCall) relatedIds
    
    -- Actualizamos las llamadas en las promesas relacionadas
    lnp <- forM relatedIds $ \idRelated -> do
      let
        promR = getPromise idRelated promises

      if isJust promR then do
        let
          promR' = fromJust promR
          (PromiseS idpromR paramspromR typepromR catpromR pospromR checkspromR checksCallspromR checks3) = promR'
          nchecksCallspromR = map (\c -> if c == lcp then newcheck else  c) checksCallspromR
          np = PromiseS idpromR paramspromR typepromR catpromR pospromR checkspromR nchecksCallspromR checks3
    
        return [np]
      else return []
    
    let
      nparams = [(fromMaybe TError (getTLists [typeE ne,t]),p)|(t,ne,p) <- listParams]
    
    when (all (\(t,_) -> t/=TError) nparams && any (\(t,_) -> not $ isRealType t) defParamsCall) $
      updatePromiseArgTypes (fromJust $ getPromise idPromiseCall promises) nparams
      
    state@SymTabState{proms = promises} <- get
    let 
      lnp2           = concat lnp
      lnpids         = map (\p -> (promiseId p, p)) lnp2
      isToUpdate p   = any (\(id,_) -> id == promiseId p) lnpids
      promTpUpdate p = snd $ head $ filter (\(id,_) -> id == promiseId p) lnpids
      modifyTypePromise prom = if isToUpdate prom then promTpUpdate prom else prom

    put state{proms = map modifyTypePromise promises}
    return newcheck

  -- TODO :Eliminar solo si se actualiza a un tipo concreto
  state'@SymTabState{proms = promises} <- get
  let 
    modifyTypePromise prom = 
      if promiseId prom == name then 
        let (PromiseS id p t cat pos ch ch2 ch3) = prom 
        in PromiseS id p t cat pos ch (if isRealType tr then [] else newcheck') ch3
      else prom

  put state'{proms = map modifyTypePromise promises}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
checkExprForEach :: Promise -> Type -> MonadSymTab ()
checkExprForEach promise tr = do 
  fileCode <- ask
  let 
    name    = promiseId promise
    checks  = forEachLateCheck promise

  newcheck' <- forM checks $ \lcp@(LateCheckPromForE e1 idvar tvar pexpr lids1) -> do
    state@SymTabState{symTab = st, currS = s} <- get
    let
      newcheck = LateCheckPromForE (updatePromiseInExpr name e1 tr) idvar tvar pexpr lids1
      ne1      = expr newcheck
      tne1     = typeE ne1
    
    when (tne1 /= TPDummy) $
      if not (isArray tne1) && not (isList tne1) then
        when (tne1 /= TError) $
          tell [forEachErrorMsg tne1 fileCode pexpr]
      else
        if tvar /= TPDummy && isNothing (getTLists [TList tvar,tne1]) then
          when (tne1 /= TError && tvar /= TError) $
            tell [semmErrorMsg (TList tvar) tne1 fileCode pexpr]
        else
          when (isRealType tr && tvar == TPDummy) $
            let
              varInfo = [SymbolInfo name (baseTypeArrLst tr) s IterationVariable []]
              newSymTab = insertSymbols [idvar] varInfo st
            in
              put state{symTab = newSymTab}

    state'@SymTabState{proms = promises} <- get

    lnp <- forM lids1 $ \idpl -> do
      let
        promise' = getPromise idpl promises
      
      if isJust promise' then do
        let
          (PromiseS id2 p2 t2 cat2 pos2 ch2 chcalls ch3) = fromJust promise'
          nch3 = map (\lcp@(LateCheckPromForE e2 idvar2 tvar2 pexpr2 lids2) ->
            if e2 == e1 then LateCheckPromForE ne1 idvar2 tvar2 pexpr2 lids2 else lcp) ch3
          np = PromiseS id2 p2 t2 cat2 pos2 ch2 chcalls nch3
    
        return [np]
      else return []

    let 
      lnp2           = concat lnp
      lnpids         = map (\p -> (promiseId p, p)) lnp2
      isToUpdate p   = any (\(id,_) -> id == promiseId p) lnpids
      promTpUpdate p = snd $ head $ filter (\(id,_) -> id == promiseId p) lnpids
      modifyTypePromise prom = if isToUpdate prom then promTpUpdate prom else prom

    put state'{proms = map modifyTypePromise promises}
    return newcheck

  -- TODO :Eliminar solo si se actualiza a un tipo concreto
  state''@SymTabState{proms = promises} <- get
  let 
    modifyTypePromise prom = 
      if promiseId prom == name then 
        let (PromiseS id p t cat pos ch ch2 ch3) = prom 
        in PromiseS id p t cat pos ch ch2 (if isRealType tr then [] else newcheck')
      else prom

  put state''{proms = map modifyTypePromise promises}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Chequea las expresiones de una promesa dado que se le actualizó su tipo
-- Se actualiza el tipo de la llamada a la funcion de la promesa en sus expresiones latechecks
-- 
-- si se actualizó la promesa a un tipo concreto se eliminan sus latechecks
checkExpr :: Promise -> Type-> MonadSymTab ()
checkExpr promise tr = do
  fileCode <- ask
  let 
    name    = promiseId promise
    -- params  = promiseParams promise
    checks  = promiseLateCheck promise

  newcheck' <- forM checks $ \(LateCheckPromS e1 lpos1 lids1) -> do

    state@SymTabState{proms = promises} <- get
    let
      newcheck = LateCheckPromS (updatePromiseInExpr name e1 tr) lpos1 lids1
      ne1 = expr newcheck
      tne1 = typeE ne1
    
    checkLateCheck ne1 lpos1
    
    lnp <- forM lids1 $ \idpl -> do
      let
        promise' = getPromise idpl promises
      
      if isJust promise' then do
        let
          (PromiseS id2 p2 t2 cat2 pos2 ch2 chcalls ch3) = fromJust promise'
          nch2 = map (\lcp@(LateCheckPromS e2 lpos2 lids2) ->
            if e2 == e1 then LateCheckPromS ne1 lpos2 lids2 else lcp) ch2
          np = PromiseS id2 p2 t2 cat2 pos2 nch2 chcalls ch3

        return [np]
      else return []

    let 
      lnp2           = concat lnp
      lnpids         = map (\p -> (promiseId p, p)) lnp2
      isToUpdate p   = any (\(id,_) -> id == promiseId p) lnpids
      promTpUpdate p = snd $ head $ filter (\(id,_) -> id == promiseId p) lnpids
      modifyTypePromise prom = if isToUpdate prom then promTpUpdate prom else prom

    put state{proms = map modifyTypePromise promises}
    return newcheck

  -- TODO :Eliminar solo si se actualiza a un tipo concreto
  state'@SymTabState{proms = promises} <- get
  let 
    modifyTypePromise prom = 
      if promiseId prom == name then 
        let (PromiseS id p t cat pos ch ch2 ch3) = prom 
        in PromiseS id p t cat pos (if isRealType tr then [] else newcheck') ch2 ch3
      else prom

  put state'{proms = map modifyTypePromise promises}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
checkUnaryExpr :: UnOp -> Expr -> Type -> Pos -> MonadSymTab ()
checkUnaryExpr op e t p = do
  fileCode <- ask
  let 
    te = typeE e

  if op == Length then
    unless (isArray te || isList te) $
      when (te `notElem` [TError,TPDummy]) $
        tell [arrLstErrorMsg te fileCode p]
  else 
    when (op == Negative) $
      unless (te == TInt || te == TFloat) $
        when (te `notElem` [TError,TPDummy]) $
          tell [aritErrorMsg te fileCode p]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
checkBinaryExpr :: BinOp -> Expr -> Pos-> Expr -> Pos -> MonadSymTab ()
checkBinaryExpr op e1 p1 e2 p2 = do
  fileCode <- ask
  let
    tE1     = typeE e1
    tE2     = typeE e2
    eqOps   = [Eq, NotEq]
    compOps = [Eq, NotEq, GreaterEq, LessEq, Greater, Less]
    aritOps = [Add, Minus, Mult, Division]
    aritInt = [DivEntera, Module]
    boolOps = [And, Or]

  if tE1 /= tE2 then -- Si son distintos los tipos de las funciones

    if op `elem` eqOps then
      
      if isTypeComparableEq tE1 &&  not (isTypeComparableEq tE2) then
        when (tE1 `notElem` [TError,TPDummy] && tE2 `notElem` [TError,TPDummy]) $
          tell [semmErrorMsg tE1 tE2 fileCode p2]
      else 
        if not (isTypeComparableEq tE1) &&  isTypeComparableEq tE2 then
          when (tE1 `notElem` [TError,TPDummy] && tE2 `notElem` [TError,TPDummy]) $
            tell [semmErrorMsg tE2 tE1 fileCode p1]
        else
          if isTypeComparableEq tE1 && isTypeComparableEq tE2 then
            when (isNothing (getTLists [tE1,tE2])) $
              when (tE1 `notElem` [TError,TPDummy] && tE2 `notElem` [TError,TPDummy]) $
                tell [semmErrorMsg tE1 tE2 fileCode p2]
          else 
            when (tE1 `notElem` [TError,TPDummy]) $
              tell [compErrorMsg tE1 fileCode p1]

    else 

      if (op `elem` compOps ) || (op `elem` aritOps )then

        if isTypeNumber tE1 &&  not (isTypeNumber tE2) then
          when (tE1 `notElem` [TError,TPDummy] && tE2 `notElem` [TError,TPDummy]) $
            tell [semmErrorMsg tE1 tE2 fileCode p2]
        else 
          if not (isTypeNumber tE1) &&  isTypeNumber tE2 then
            when (tE1 `notElem` [TError,TPDummy] && tE2 `notElem` [TError,TPDummy]) $
              tell [semmErrorMsg tE2 tE1 fileCode p1]
          else 
            if isTypeNumber tE1  && isTypeNumber tE2 then
              when (tE1 `notElem` [TError,TPDummy] && tE2 `notElem` [TError,TPDummy]) $
                tell [semmErrorMsg tE1 tE2 fileCode p2]
            else 
              when (tE1 `notElem` [TError,TPDummy]) $
                tell [aritErrorMsg tE1 fileCode p1]

      else 
        if op `elem` aritInt then

          if tE1 == TInt then
            when (tE1 `notElem` [TError,TPDummy] && tE2 `notElem` [TError,TPDummy]) $
              tell [semmErrorMsg tE1 tE2 fileCode p2]
          else 
            if tE2 == TInt && tE1 `notElem` [TError,TPDummy] then
              tell [semmErrorMsg tE2 tE1 fileCode p1]
            else
              when (tE1 `notElem` [TError,TPDummy]) $
                tell [semmErrorMsg TInt tE1 fileCode p1]

        else 
          if op `elem` boolOps then

            if tE1 == TBool && tE2 `notElem` [TError,TPDummy] then
              tell [semmErrorMsg tE1 tE2 fileCode p2]
            else 
              if tE1 /= TBool && tE2 == TBool && tE1 `notElem` [TError,TPDummy]
                && tE2 `notElem` [TError,TPDummy] then
                  tell [semmErrorMsg tE2 tE1 fileCode p1]
              else
                when (tE1 `notElem` [TError,TPDummy]) $
                  tell [semmErrorMsg TBool tE1 fileCode p1]

          else do
            when (op == Anexo) $
              let typeR = fromMaybe TError (getTLists [TList tE1,tE2])
              in
                when (typeR == TError && tE1 `notElem` [TError,TPDummy] &&
                  baseTypeT tE2 `notElem` [TError,TPDummy]) $
                    tell [semmErrorMsg (TList tE1) tE2 fileCode p2]

            when (op == Concat) $ 
              if isList tE1 && not (isList tE2) then
                when (tE2 `notElem` [TError,TPDummy]) $
                  tell [semmErrorMsg tE1 tE2 fileCode p2]
              else
                if not (isList tE1) && not (isList tE2) then
                  when (baseTypeT tE1 `notElem` [TError,TPDummy] &&
                    baseTypeT tE2 `notElem` [TError,TPDummy]) $
                      tell [concatErrorMsg tE1 tE2 fileCode p2]
                else 
                  when (isList tE1 && isList tE2 && isNothing (getTLists [tE1,tE2])
                    && baseTypeT tE1 `notElem` [TError,TPDummy] &&
                      baseTypeT tE2 `notElem` [TError,TPDummy]) $
                        tell [semmErrorMsg tE1 tE2 fileCode p2]

  else --- Si son iguales los tipos de las expresiones 
    if op `elem` eqOps then
      unless (isTypeComparableEq tE1) $           
        when (tE1 `notElem` [TError,TPDummy]) $
          tell [compErrorMsg tE1 fileCode p1]
      
    else
      if op `elem` compOps || op `elem` aritOps then
        when (tE1 /= TInt && tE1 /= TFloat && tE1 `notElem` [TError,TPDummy]) $
          tell [aritErrorMsg tE1 fileCode p1]
      else
        if op `elem` aritInt then
          when (tE1 /= TInt && tE1 `notElem` [TError,TPDummy]) $
            tell [semmErrorMsg TInt tE1 fileCode p1]
        else
          if op `elem` boolOps then
            when (tE1 /= TBool && tE1 `notElem` [TError,TPDummy]) $
              tell [semmErrorMsg TBool tE1 fileCode p1]
          else do
            when (op == Anexo) $
              let typeR = fromMaybe TError (getTLists [TList tE1,tE2])
              in
                when (typeR == TError && tE1 `notElem` [TError,TPDummy] &&
                  baseTypeT tE2 `notElem` [TError,TPDummy]) $
                    tell [semmErrorMsg (TList tE1) tE2 fileCode p2]

            when (op == Concat && not (isList tE1) && not (isList tE2) &&
              baseTypeT tE1 `notElem` [TError,TPDummy] &&
                baseTypeT tE1 `notElem` [TError,TPDummy]) $
                  tell [concatErrorMsg tE1 tE2 fileCode p1]

  return ()
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checkea que la expresion sea correcta
checkLateCheck :: Expr -> [Pos] -> MonadSymTab ()
checkLateCheck (Binary op e1 e2 _) pos =
  let 
    tE1 = typeE e1
    tE2 = typeE e2
  in
    when ((tE1 /= TPDummy && tE1 /= TDummy) && (tE2 /= TPDummy && tE2 /= TDummy)) $ 
      checkBinaryExpr op e1 (head pos) e2 (pos !! 1)

checkLateCheck (Unary op e texpr) pos =
  let
    tE = typeE e
  in
    when (tE /= TPDummy && tE /= TDummy) $ checkUnaryExpr op e texpr (head pos)

checkLateCheck (IfSimple e1 e2 e3 _) lpos = do
  let 
    tE1 = typeE e1
    tE2 = typeE e2
    tE3 = typeE e3

  when (tE1 /= TPDummy && tE2 /= TPDummy && tE3 /= TPDummy) $ do
    fileCode <- ask

    if tE1 == TBool then
      let mbTypeR = getTLists [tE2,tE3] -- Simula crear una lista que contiene esos dos tipos para ahorrar calculos
      in
      when (isNothing mbTypeR) $
        if isRealType tE2 && not (isRealType tE3) then
          when (tE2 /= TError && tE3 /= TError) $
            tell [semmErrorMsg tE2 tE3 fileCode (lpos !! 2)]
        else
          if not (isRealType tE2) && isRealType tE3 then
            when (tE2 /= TError && tE3 /= TError) $
              tell [semmErrorMsg tE3 tE2 fileCode (lpos !! 1)]
          else
            when (tE2 /= TError && tE3 /= TError) $
              tell [semmErrorMsg tE2 tE3 fileCode (lpos !! 2)]
    else
      when (tE1 /= TError) $
        tell [semmErrorMsg TBool tE1 fileCode (head lpos)]

checkLateCheck (ArrayList exprs _) pos = do
  let
    mapTypes = map typeE exprs
    nta = getTLists mapTypes

  unless (isJust nta) $ do
    fileCode <- ask
    let 
      exprsP = zip exprs pos
      (tExpected, (tGot,ptGot)) = getTExpectedTGot (map (\(e,pe) -> (typeE e,pe)) exprsP)
      msg = semmErrorMsg tExpected tGot fileCode ptGot

    when (baseTypeT tExpected `notElem` [TError,TPDummy] &&
      baseTypeT tGot `notElem` [TError,TPDummy]) $
        tell [msg]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
{-
Actualiza el tipo de los argumentos de una promesa a una versión más especifica.

Solo se debería llamar cuando no se tiene un tipo concreto para los argumentos
y se acaba de inferir un tipo más especifico. Ejemplo, puntero a algo -> puntero a entero
-}
updatePromiseArgTypes :: Promise -> [(Type,Pos)] -> MonadSymTab ()
updatePromiseArgTypes promise nparams  = do
  let  
    name      = promiseId promise
    extraInfo = Params [(t,show i)| ((t,_),i) <- zip nparams [1..]]

  state@SymTabState{proms = promises} <- get
  let 
    modifyTypePromise prom = 
      if promiseId prom == name then 
        let (PromiseS id params t cat pos ch ch2 ch3) = prom 
        in PromiseS id nparams t cat pos ch ch2 ch3
      else prom
  
  -- Actualizamos los tipos de los argumentos en la promesa
  put state{proms = map modifyTypePromise promises}

  -- Actualizamos los tipos de los argumentos en la tabla de simbolos
  updateExtraInfoProm name (promiseCat promise) [extraInfo]


addLateCheckCall :: Id -> Subroutine -> [Id] -> MonadSymTab ()
addLateCheckCall id call relatedIds = do
  state@SymTabState{proms = promises} <- get
  let  
    promise = fromJust $ getPromise id promises

  let 
    newCheckCall = LateCheckPromCall call (filter (/=id) relatedIds)
    modifyTypePromise prom = 
      if promiseId prom == id then 
        let (PromiseS id params t cat pos ch ch2 ch3) = prom 
        in PromiseS id params t cat pos ch (ch2 ++ [newCheckCall]) ch3
      else prom
  
  -- Actualizamos los tipos de los argumentos en la promesa
  put state{proms = map modifyTypePromise promises}

updatePromiseLateChecksCalls :: Subroutine -> [(Type,Pos)] ->MonadSymTab ()
updatePromiseLateChecksCalls callf@(Call namef params) nparams = do
  SymTabState{proms = promises} <- get
  let
    exprs       = map fst params
    relatedIds  = map getRelatedPromises exprs
    relatedIds' = (S.toList . S.fromList) (concat relatedIds)

  unless (null relatedIds) $
    mapM_ (\id -> addLateCheckCall id callf relatedIds') relatedIds'

  let 
    promise = getPromise namef promises
  
  when (isJust promise) $ do
    let 
      promise' = fromJust promise
      paramsPromise = promiseParams promise'
    when (any (\(t,_) -> not (isRealType t)) paramsPromise)  $
      -- Si tiene una promesa y hay al menos un tipo sin inferir
      updatePromiseArgTypes (fromJust promise) nparams
-------------------------------------------------------------------------------
