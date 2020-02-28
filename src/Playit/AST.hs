{- |
 * Creates de abstract syntax tree with type checks
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.AST where

import Control.Monad           (when,unless,forM,forM_)
import Control.Monad.Trans.RWS
import Data.Maybe              (fromJust,isJust,isNothing,fromMaybe)
import Playit.AuxFuncs
import Playit.CheckAST
import Playit.Errors
import Playit.PromisesHandler
import Playit.SymbolTable
import Playit.Types


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Create AST nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates whole statements block
program :: InstrSeq -> MonadSymTab Instr
program i =
  if all isVoid i then
    return $ Program i TVoid
  else
    return $ Program i TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates variables ids node
var :: Id -> Pos -> MonadSymTab (Var,Pos)
var id p = do
  SymTabState{symTab = st, actS = activeScopes} <- get
  fileCode <- ask
  let
    infos = lookupInScopes activeScopes id st
    vErr  = (Var id TError, p)

  if isJust infos then
    let 
      vars    = [Variables,IterationVariable,Parameters Value,Parameters Reference]
      isVar s = category s `elem` vars
      v       = filter isVar (fromJust infos)
    in
      if null v then
        tell [errorMsg "This is not a variable" fileCode p] >> return vErr
      else
        return (Var id (symType $ head v), p)
  else
    tell [errorMsg "Variable not declared in active scopes" fileCode p] >> return vErr
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates indexed variables node
indexArray :: (Var,Pos) -> (Expr,Pos) -> MonadSymTab (Var,Pos)
indexArray (var,pVar) (expr,pExpr)
  | not (isArray tVar) && tVar /= TStr = do
    fileCode <- ask
    when (tVar /= TError) $
      tell [arrayErrorMsg tVar fileCode pVar]
    return (Index var expr TError, pVar) -- Maybe luego sera necesario distinguir en el nodo array/lista/string

  -- | tVar == TStr && 
  | tExpr == TPDummy || tExpr == TInt = do
    nexpr <- updateExpr expr TInt
    let tindex = if tVar == TStr then TChar else baseTypeArrLst (typeVar var)
    return (Index var nexpr tindex, pVar)

  | tExpr /= TInt = do
    fileCode <- ask
    when (tExpr /= TError) $
      tell [indexErrorMsg tExpr fileCode pExpr]
    return (Index var expr TError, pVar) 

  where
    tExpr = typeE expr
    tVar = typeVar var


indexList :: (Var,Pos) -> (Expr,Pos) -> MonadSymTab (Var,Pos)
indexList (var,pVar) (expr,pExpr)
  | not (isList tVar) = do
    fileCode <- ask
    when (tVar /= TError) $
      tell [listErrorMsg tVar fileCode pVar]
    return (Index var expr TError, pVar) -- Maybe luego sera necesario distinguir en el nodo array/lista/string

  | tExpr == TPDummy || tExpr == TInt = do
    nexpr <- updateExpr expr TInt
    return (Index var nexpr (baseTypeArrLst (typeVar var)), pVar)

  | tExpr /= TInt = do
    fileCode <- ask
    when (tExpr /= TError) $
      tell [indexErrorMsg tExpr fileCode pExpr]
    return (Index var expr TError, pVar) -- Maybe luego sera necesario distinguir en el nodo array/lista/string

  where
    tExpr = typeE expr
    tVar  = typeVar var
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the registers / unions fields
field :: (Var,Pos) -> Id -> Pos -> MonadSymTab (Var,Pos)
field (var,pVar) field pField = do
  SymTabState{symTab = st, proms = promises} <- get
  fileCode                 <- ask
  
  -- Verify type 'var' is register / union
  -- La existencia de "name" fue verificada ya en un parse anterior
  -- Nota: Puede no existir en la tabla pero sí en una promesa
  let
    tVar = typeVar var
    err  = Field var field TError
    tErr = fieldErrorMsg tVar fileCode pVar
    reg  = case baseTypeVar var of 
            (TNew name) -> name
            _           -> ""

  if reg == "" then -- type error
    when (tVar /= TError) (tell [tErr]) >> return (err, pField)
  else
    --chequearTipo reg p
    
    let info = lookupInSymTab field st
    in
      if isJust info then
        let
          isInRegUnion (SymbolInfo _ _ _ c e) = c == Fields && getReg e == reg
          symbols = filter isInRegUnion (fromJust info)
          noField = errorMsg ("Field not in '" ++ reg ++ "'") fileCode pField
        in
          if null symbols then tell [noField] >> return (err, pField)
          else
            return (Field var field (symType $ head symbols), pField)
      else
        let
          promise = getPromise reg promises
          fNoDecl = [errorMsg "Field not declared" fileCode pField]
          noDecl  = [errorMsg ("'"++ reg ++"' was not declared yet. Incorrect use.") fileCode pField]
        in
          if isNothing promise then
            tell fNoDecl >> return (err, pField)
          else
            tell noDecl >> return (err, pField)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the desreferentiation variable node
desref :: (Var,Pos) -> MonadSymTab (Var,Pos)
desref (var,p) = do
  fileCode <- ask
  let
    (tVar, msg) = checkDesref (typeVar var) p fileCode
    var' = (Desref var tVar, p)
  
  if null msg then return var'
  else tell [msg] >> return var'
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the TNew type
newType :: Id -> Pos -> MonadSymTab Type
newType tName p = do
  SymTabState{symTab = st} <- get
  fileCode          <- ask
  msg               <- checkNewType tName p st fileCode
  
  if null msg then return $ TNew tName
  else tell [msg] >> return TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Create assignations instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates an assignation node
assig :: (Var,Pos) -> (Expr,Pos) -> MonadSymTab Instr
assig (lval,pLval) (e,pE) = do
  fileCode <- ask
  iter     <- checkIterVar lval
  e'       <- updateExpr e (typeVar lval)
  let
    msg = checkAssig (typeVar lval) e' pE fileCode

  if not iter && null msg then return (Assig lval e' TVoid)
  else do
    when iter $
      tell [errorMsg "You can't modify an iteration variable" fileCode pLval]
    
    when (typeVar lval /= TError && typeE e' /= TError) $ tell [msg]
    return (Assig lval e' TError)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
increDecreVar :: BinOp -> (Var,Pos) -> MonadSymTab Instr
increDecreVar op (var,pos) = do
  fileCode <- ask
  let
    tVar = typeVar var
    expr = Binary op (Variable var TInt) (Literal (Integer 1) TInt) TInt

  unless (tVar == TInt) $
    when (typeVar var /= TError) $
      tell [semmErrorMsg TInt tVar fileCode pos]

  assig (var,pos) (expr, pos)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- |
regUnion :: (Id,Pos) -> [(Expr,Pos)] -> MonadSymTab (Expr,Pos)
regUnion (name,p) e = do
  SymTabState{symTab = st} <- get
  fileCode                 <- ask
  let
    exprs = map fst e
    msg   = checkRegUnion name e st fileCode p

  if null msg then return (Literal (Register exprs) (TNew name), p)
  else
    tell [msg] >> return (Literal (Register exprs) TError, p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Create operators instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the binary operator node
binary :: BinOp -> (Expr,Pos) -> (Expr,Pos) -> Pos -> MonadSymTab (Expr,Pos)
binary op ep1@(e1,_) ep2@(e2,_) p = do
  (bin, msg) <- checkBinary op ep1 ep2
  let
    e = (bin, p)
  
  if null msg then return e
  else
    if typeE e1 /= TError && typeE e2 /= TError then tell [msg] >> return e
    else return e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the unary operator node
unary :: UnOp -> (Expr,Pos) -> Type -> MonadSymTab (Expr,Pos)
unary op (expr,p) tExpected = do
  fileCode <- ask
  let
    (tOp, msg) = checkUnary op (typeE expr) tExpected p fileCode
    e          = Unary op expr tOp
    related    = getRelatedPromises expr
  
  if null msg then
    if not (null related) then
      if tExpected /= TVoid then do
        newExpr <- updateExpr expr tExpected
        return (Unary op newExpr tOp, p)
      else
        addLateCheck e e [p] related >> return (e, p)
    else
      return (e, p)
  else
    if typeE expr /= TError then tell [msg] >> return (e, p)
    else return (e, p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Create arrays / lists instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserta en una lista un nuevo elemento en indice 0
anexo :: (Expr,Pos) -> (Expr,Pos) -> Pos -> MonadSymTab (Expr, Pos)
anexo (e1,p1) (e2,p2) p = do
  fileCode <- ask
  let
    (tOp, msg) = checkAnexo (e1,p1) (e2,p2) fileCode
    e          = Binary Anexo e1 e2 tOp
  
  if null msg then do
    newE <- updateExpr e (fromJust $ getTLists [TList (typeE e1),typeE e2])
    let
      related = getRelatedPromises newE
    
    if not $ null related then
      addLateCheck newE newE [p1,p2] related >> return (newE,p)
    else
      return (e, p)

  else do
    when (typeE e1 /= TError && baseTypeE e2 /= TError) $ tell [msg]
    return (e, p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Create concat 2 lists operator node
concatLists :: (Expr,Pos) -> (Expr,Pos) -> Pos -> MonadSymTab (Expr,Pos)
concatLists (e1,p1) (e2,p2) p 
  | (isList t1 || t1 == TPDummy) && (isList t2 || t2 == TPDummy) && isJust tL = do -- <<2>>:: <<>>
    let
      typel   = fromJust tL
      exprR   = Binary Concat e1 e2 typel

    newE <- updateExpr exprR typel
    let
      related = getRelatedPromises newE
    
    unless (null related) $ addLateCheck newE newE [p1,p2,p] related
    return (newE, p)

  | otherwise = do
    fileCode <- ask

    if isList t1 && not (isList t2) then do
      when (t1 /= TError && t2 /= TError) $
        tell [semmErrorMsg t1 t2 fileCode p2]
      return (err, p2)
    else do
      if (not (isList t1) && isList t2) {-|| (isList t1 && isList t2)-} then
        when (baseTypeT t1 /= TError && baseTypeT t2 /= TError) $
          tell [semmErrorMsg t2 t1 fileCode p1]
      else
        when (baseTypeT t1 /= TError && baseTypeT t2 /= TError) $
          tell [concatErrorMsg t1 t2 fileCode p]

      return (err, p1)

    return (err, p)

  where
    t1  = typeE e1
    t2  = typeE e2
    tL  = getTLists [t1,t2]
    err = Binary Concat e1 e2 TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the same type array node
array :: [(Expr,Pos)] -> Pos -> MonadSymTab (Expr,Pos)
array expr p
  | t /= TError = do
    let
      ids = getRelatedPromises arr

    unless (null ids) $ do
      newArray <- updateExpr arr t
      let 
        nids = getRelatedPromises newArray
      
      unless (null nids) $ addLateCheck newArray newArray (map snd expr) nids
      
    return (arr, p)
  
  | otherwise = do
    fileCode <- ask
    let 
      (tExpected, (tGot,pTGot)) = getTExpectedTGot (map (\(e,p) -> (typeE e,p)) expr)
  
    when (tExpected /= TError && tGot /= TError) $
      tell [semmErrorMsg tExpected tGot fileCode pTGot]
    return (arr, p)

  where
    e          = map fst expr
    arrayTypes = map typeE e
    t          = fromMaybe TError (getTLists arrayTypes)
    arr        = ArrayList e (TArray (Literal (Integer $ length e) TInt) t)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the same type list node
list :: [(Expr,Pos)] -> Pos -> MonadSymTab (Expr,Pos)
list [] p = return (ArrayList [] (TList TDummy), p) -- TODO : Recordar TDummy -> TListEmpty.
list expr p
  | isJust t = do
    let
      typel = TList (fromJust t)
      list  = ArrayList exprs typel
      ids   = getRelatedPromises list

    unless (null ids) $ do
      newList <- updateExpr list typel
      let 
        newIds  = getRelatedPromises newList
      
      unless (null newIds) $ addLateCheck newList newList (map snd expr) newIds

    return (list, p)

  | otherwise = do
    fileCode <- ask
    let
      (tExpected, (tGot,pTGot)) = getTExpectedTGot (map (\(e,pe) -> (typeE e,pe)) expr)

    when (tExpected /= TError && tGot /= TError) $
      tell [semmErrorMsg tExpected tGot fileCode pTGot]
    return (ArrayList exprs TError, pTGot)

  where
    exprs = map fst expr
    t     = getTLists $ map typeE exprs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Creates the selection instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the selection instruction node
if' :: [(Expr, InstrSeq)] -> Pos -> Instr
if' cases p = if allSeqsVoid then IF cases TVoid else IF cases TError
  where
    seqs        = map snd cases
    allSeqsVoid = all (all isVoid) seqs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the guards of the selection instruction node
guard :: (Expr,Pos) -> InstrSeq -> MonadSymTab (Expr, InstrSeq)
guard (cond,p) i = do
  fileCode <- ask
  let
    tCond = typeE cond
    cond' = (cond, i)

  if tCond == TBool then return cond'
  else
    if tCond == TPDummy then do
      ncond <- updateExpr cond TBool
      return (ncond, i)
    else do
    when (tCond /= TError) $ tell [semmErrorMsg TBool tCond fileCode p]
    return cond'
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the simple if instruction node
ifSimple :: (Expr,Pos) -> (Expr,Pos) -> (Expr,Pos) -> MonadSymTab (Expr,Pos)
ifSimple (cond,pC) (true,pT) (false,pF) = do
  fileCode <- ask
  let
    tCond   = typeE cond
    tTrue   = typeE true
    tFalse  = typeE false
    (t,msg) = checkIfSimple (tCond,pC) (tTrue,pT) (tFalse,pF) fileCode
    e       = (IfSimple cond true false t, pC)

  if null msg then do
    ncond <- updateExpr cond TBool
    ntrue <- updateExpr true t
    nfalse <- updateExpr false t

    let (_,msg2) = checkIfSimple (typeE ncond,pC) (typeE ntrue,pT) (typeE nfalse,pF) fileCode
    
    if null msg2 then do
      let
        exprR = IfSimple ncond ntrue nfalse t
        ids  = getRelatedPromises exprR

      unless (null ids) $ addLateCheck exprR exprR [pC,pT,pF] ids
      return (exprR, pC)
    else do
      when (tCond /= TError && tTrue /= TError && tFalse /= TError) $ tell [msg2]
      return (IfSimple ncond ntrue nfalse TError, pC)      
  else do
    when (tCond /= TError && tTrue /= TError && tFalse /= TError) $ tell [msg]
    return (IfSimple cond true false TError, pC)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                 Creates the iterations instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the determined iteration instruction node
for :: Id -> (Expr,Pos) -> (Expr,Pos) -> InstrSeq  -> MonadSymTab Instr
for var (e1,pE1) (e2,pE2) i = do
  fileCode <- ask
  let 
    tE1 = typeE e1
    tE2 = typeE e2

  if tE1 /= TInt && tE1 /= TPDummy then do
    when (tE1 /= TError) $ tell [semmErrorMsg TInt tE1 fileCode pE1]
    return $ For var e1 e2 i TError
  else
    if tE2 /= TInt && tE2 /= TPDummy then do
      when (tE2 /= TError) $ tell [semmErrorMsg TInt tE2 fileCode pE2]
      ne1 <- updateExpr e1 TInt
      return $ For var ne1 e2 i TError
    else do
      ne1 <- updateExpr e1 TInt
      ne2 <- updateExpr e2 TInt
      return $ For var ne1 ne2 i TVoid
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the determined conditional iteration instruction node
forWhile :: Id -> (Expr,Pos) -> (Expr,Pos) -> (Expr,Pos) -> InstrSeq -> MonadSymTab Instr
forWhile var (e1,pE1) (e2,pE2) (e3,pE3) i = do
  fileCode <- ask
  ne1 <- updateExpr e1 TInt
  ne2 <- updateExpr e2 TInt
  ne3 <- updateExpr e3 TBool
  let 
    tE1 = typeE ne1
    tE2 = typeE ne2
    tE3 = typeE ne3
    
  if tE1 /= TInt then do
    when (tE1 /= TError) $ tell [semmErrorMsg TInt tE1 fileCode pE1]
    return $ ForWhile var ne1 ne2 ne3 i TError
  else
    if tE2 /= TInt then do
      when (tE2 /= TError) $ tell [semmErrorMsg TInt tE2 fileCode pE2]
      ne1 <- updateExpr e1 TInt
      return $ ForWhile var ne1 ne2 ne3 i TError
    else
      if tE3 /= TBool then do
        when (tE3 /= TError) $ tell [semmErrorMsg TBool tE3 fileCode pE3]
        return $ ForWhile var ne1 ne2 ne3 i TError
      else 
        return $ ForWhile var ne1 ne2 ne3 i TVoid
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the determined iteration instruction node for arrays / list
forEach :: Id -> (Expr,Pos) -> InstrSeq -> MonadSymTab Instr
forEach var (e,p) i = do
  fileCode <- ask
  let 
    tE               = typeE e
    forOk            = ForEach var e i TVoid
    forErr           = ForEach var e i TError
    returnCheckedFor = if all isVoid i then return forOk else return forErr
  
  if isArray tE || isList tE then returnCheckedFor
  else
    if tE == TPDummy then do
      let
        related    = getRelatedPromises e

      forM_ related (\id -> addLateCheckForEach id var e p related)
      returnCheckedFor
    else do
      when (tE /= TError) $ tell [forEachErrorMsg tE fileCode p]
      return forErr
-------------------------------------------------------------------------------
    

-------------------------------------------------------------------------------
-- | Creates the indetermined iteration instruction node
while :: (Expr,Pos) -> InstrSeq -> MonadSymTab Instr
while (cond,p) i = do
  fileCode <- ask
  let 
    tCond = typeE cond

  if tCond == TBool || tCond == TPDummy then do
    newCond <- updateExpr cond TBool
    if all isVoid i then
      return $ While newCond i TVoid
    else
      return $ While newCond i TError
  else do
    when (tCond /= TError) $ tell [semmErrorMsg TBool tCond fileCode p]
    return $ While cond i TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                   Procedures / Functions calls nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates subroutine call instruction node
-- Considerar quitar esta función
call :: Id -> Params -> Pos -> MonadSymTab (Subroutine,Pos)
call subroutine args p = do

  SymTabState{symTab = st} <- get
  fileCode <- ask
  let
    symInfos = lookupInScopes [1,0] subroutine st
    sub      = (Call subroutine args, p)
  
  if isJust symInfos then
    let
      isSubroutine si = category si `elem` [Procedures, Functions]
      subroutine'     = filter isSubroutine (fromJust symInfos)
    in
      if null subroutine' then
        tell [errorMsg "This is not a subroutine" fileCode p] >> return sub
      else do
        let 
          extraInfoF = extraInfo $ head subroutine'
          params     = fromJust $ getParams extraInfoF
          nParams    = length params
          nArgs      = length args          
          l = [(getTLists [typeE e, tp],((e,pe),(tp,id))) | ((e,pe),(tp,id)) <-  zip args params]

        if nArgs == nParams then 
          if  any (isNothing.fst) l then do
            let               
              ( _ , ((e,pe),(t,_))) = head $ dropWhile (isJust.fst) l

            when (t /= TError && typeE e /= TError) $
              tell [semmErrorMsg t (typeE e) fileCode pe]
            return sub
          else do                

            -- Hacemos inferencia para las expresiones en los argumentos
            newargs <- forM l $ \(Just nt,((e,p),(_,_))) -> do
              -- Inferencia para una llamada a función como argumento
              ne <- updateExpr e nt 
              return (ne,p)
            
            -- Registramos como late checks a la llamada a la función 
            --si un argumento no es un tipo definido
            let 
              callf = Call subroutine newargs
              nparams = map (\(Just nt,((_,p),(_,_))) -> (nt, p)) l

            updatePromiseLateChecksCalls callf nparams
            return (callf,p)
        else
          let msj = "Amount of arguments: " ++ show nArgs ++ 
                " not equal to expected:" ++ show nParams
          in tell [errorMsg msj fileCode p] >> return sub
  else
    -- Add a promise to create subroutine
    -- Si no existe construimos la llamada igual para que procCall o funcCall creen la promesa
    -- put(symTab, activeScopes, scopes, promises ++ [Promise subroutine (map typeE args) TPDummy p] )
    return sub
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
procCall:: (Subroutine,Pos) -> MonadSymTab Instr
procCall (proc@(Call name args), p) = do
  state@SymTabState{symTab = st, proms = promises} <- get
  fileCode <- ask
  let symInfos = lookupInScopes [1,0] name st
  
-- Si esta definido, verificar que es procedimiento
  if isJust symInfos then
    let
      isProc s = category s == Procedures
      proc'    = filter isProc (fromJust symInfos)
      msg      = errorMsg ("'" ++ name ++ "' is not a procedure") fileCode p
    in
      if null proc' then
        tell [msg] >> return (ProcCall proc TError)
      else
        return $ ProcCall proc TVoid
-- Sino, es una promesa.
  else do
    let
      nparams     = map (\(e,p) -> (typeE e,p)) args 
      extraInfo   = Params [(typeE e,show i)| ((e,p),i) <- zip args [1..]]
      newProc     = [SymbolInfo name TVoid 1 Procedures [extraInfo]]
      newProm     = PromiseS name nparams TVoid Procedures p [] [] []
      newPromises = promises ++ [newProm]
      newSymTab   = insertSymbols [name] newProc st

    put state{symTab = newSymTab, proms = newPromises}
    updatePromiseLateChecksCalls proc nparams
    return $ ProcCall proc TVoid
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates function call expresion node
-- NOTE: Its already verified that subroutine's defined with 'call', because
--      its excuted first
funcCall :: (Subroutine,Pos) -> MonadSymTab (Expr,Pos)
funcCall (func@(Call name args), p) = do
  state@SymTabState{symTab = st, proms = promises} <- get
  fileCode <- ask
  let symInfos = lookupInScopes [1,0] name st

-- Si esta definido, verificar que es funcion
  if isJust symInfos then
    let 
      isFunc s = category s == Functions
      func'    = filter isFunc (fromJust symInfos)
      msg      = errorMsg ("'" ++ name  ++ "' is not a function") fileCode p
    in
      if null func' then
        tell [msg] >> return (FuncCall func TError, p)
      else
        return (FuncCall func (symType $ head func'), p)
-- Sino, es una promesa.
  else do
    let
      nparams     = map (\(e,p) -> (typeE e,p)) args
      extraInfo   = Params [(typeE e,show i)| ((e,p),i) <- zip args [1..]]
      newFunc     = [SymbolInfo name TPDummy 1 Functions [extraInfo]]
      newProm     = PromiseS name nparams TPDummy Functions p [] [] []
      newPromises = promises ++ [newProm]
      newSymTab   = insertSymbols [name] newFunc st

    put state{symTab = newSymTab, proms = newPromises}
    updatePromiseLateChecksCalls func nparams
    return (FuncCall func TPDummy, p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                          I/O instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the print instruction node
print' :: [(Expr,Pos)] -> Pos -> MonadSymTab Instr
print' expr p
  | TError `notElem` exprsTypes = return $ Print exprs TVoid -- Si hay un tpdummy aqui no hay forma de actualizar este nodo luego
  | otherwise = do
    fileCode <- ask
    let 
      (errExpr,pExpr) = head $ dropWhile (\(e,p) -> typeE e /= TError) expr
      msg             = semmErrorMsg TStr (typeE errExpr) fileCode pExpr

    when (typeE errExpr /= TError) $ tell [msg]
    return (Print exprs TError)

  where
    exprs      = map fst expr
    exprsTypes = map typeE exprs
    -- tE         = fromMaybe TError (getTLists exprsTypes)  
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the read instruction node
read' :: (Expr,Pos) -> Pos -> MonadSymTab (Expr,Pos)
read' (e, pe) p = do
  fileCode <- ask
  let
    tE = typeE e

  if tE == TStr then return (Read e TStr, p)
  else
    if tE == TPDummy then do
      ne <- updateExpr e TStr
      return (Read ne TStr, p) -- TRead para casts implicitos?
    else do
      when (typeE e /= TError) $
        tell [semmErrorMsg TStr tE fileCode p]
      return (Read e TError, p)
-------------------------------------------------------------------------------

{- Esta funcion se encarga de verificar que el tipo de la expresión del tamaño
  de un array sea entero--}
tArray :: (Expr, Pos) -> Type -> MonadSymTab Type
tArray (e,p) t
  | te == TInt  = return $ TArray e t
  | te == TPDummy  = do
    ne <- updateExpr e TInt
    return (TArray ne t)

  | otherwise = do
    fileCode <- ask
    when (typeE e /= TError) $
      tell [semmErrorMsg TInt te fileCode p]
    
    return (TArray e t)

  where
    te = typeE e

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                        Pointers instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the free memory instruction node
free :: Id -> Pos -> MonadSymTab Instr
free var p = do
  SymTabState{symTab = st, actS = activeScopes} <- get
  fileCode <- ask
  let
    infos = lookupInScopes activeScopes var st
    msg   = errorMsg "Variable not declared in active scopes" fileCode p
  
  if isJust infos then return $ Free var TVoid
  else
    tell [msg] >> return (Free var TError)
-------------------------------------------------------------------------------
