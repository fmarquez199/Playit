{- |
 * Creates de abstract syntax tree with type checks
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.AST where

import Control.Monad (void,when,unless,forM,forM_)
import Control.Monad.Trans.RWS
import qualified Data.Map as M
import Data.Maybe (fromJust,isJust,isNothing,fromMaybe)
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
  (symTab, activeScopes, _, _) <- get
  fileCode <- ask
  let
    infos = lookupInScopes activeScopes id symTab
    vErr  = (Var id TError, p)

  if isJust infos then
    let 
      vars          = [Variables, Parameters Value, Parameters Reference]
      isVar symInfo = getCategory symInfo `elem` vars
      v             = filter isVar (fromJust infos)
    in
      if null v then
        tell [errorMsg "This is not a variable" fileCode p] >> return vErr
      else
        return (Var id (getType $ head v), p)
  else
    tell [errorMsg "Variable not declared in active scopes" fileCode p] >> return vErr
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates indexed variables node
index :: (Var,Pos) -> (Expr,Pos) -> MonadSymTab (Var,Pos)
index (var,pVar) (expr,pExpr) = do
  fileCode <- ask
  let
    (tVar, msg) = checkIndex var (typeE expr) pVar pExpr fileCode
    var' = (Index var expr tVar, pVar)
  
  if null msg then return var'
  else tell [msg] >> return var'
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the registers / unions fields
field :: (Var,Pos) -> Id -> Pos -> MonadSymTab (Var,Pos)
field (var,pVar) field pField = do
  (symTab, _, _, _) <- get
  fileCode@(file,code) <- ask
  
  -- Verify type 'var' is register / union
  let
    reg = case baseTypeVar var of 
            (TNew name) -> name
            _           -> ""
    err = Field var field TError
    msg = errorMsg "Type of field isn't a register or union" fileCode pVar

  if reg == "" then tell [msg] >> return (err, pField)    -- type error
  else
    --chequearTipo reg p
    
    let info = lookupInSymTab field symTab
    in
      if isJust info then
        let
          isInRegUnion (SymbolInfo _ _ c e) = c == Fields && getReg e == reg
          symbols = filter isInRegUnion (fromJust info )
          msg'    = errorMsg ("Field not in '"++reg++"'") fileCode pField
        in
          if null symbols then tell [msg'] >> return (err, pField)
          else return (Field var field (getType $ head symbols), pField)
      else
        tell [errorMsg "Field not declared" fileCode pField] >> return (err, pField)
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
  (symTab, _, _, _) <- get
  fileCode          <- ask
  msg               <- checkNewType tName p symTab fileCode
  
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
assig (lval,pLval) (expr,pE) = do
  fileCode <- ask
  iter     <- checkIterVar lval
  expr'    <- updateExpr expr (typeVar lval)
  let
    msg = checkAssig (typeVar lval) expr' pE fileCode

  if not iter && null msg then return (Assig lval expr' TVoid)
  else tell [msg] >> return (Assig lval expr' TError)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- |
regUnion :: (Id,Pos) -> [(Expr,Pos)] -> MonadSymTab (Expr,Pos)
regUnion (name,p) e = do
  (symTab, _, _, _) <- get
  fileCode          <- ask
  let
    exprs     = map fst e
    -- p         = snd $ head e
    msg       = checkRegUnion name exprs symTab fileCode

  if null msg then return (Literal (Register exprs) (TNew name), p)
  else
    tell [errorMsg msg fileCode p] >> return (Literal (Register exprs) TError, p)
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
  (bin, msg) <- checkBinary op ep1 ep2 p
  let
    e = (bin, p)
  
  if null msg then return e
  else tell [msg] >> return e
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
  
  if null msg && not (null related) then
    if tExpected /= TVoid then do
      newExpr <- updateExpr expr tExpected
      return (Unary op newExpr tOp, p)
    else
      addLateCheck e e [p] related >> return (e, p)
  else
    tell [msg] >> return (e, p)
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
    
    if not $ null related then addLateCheck newE newE [p1,p2] related >> return (newE,p)
    else return (e, p)

  else tell [msg] >> return (e, p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Create concat 2 lists operator node
concatLists :: (Expr,Pos) -> (Expr,Pos) -> Pos -> MonadSymTab (Expr,Pos)
concatLists (e1,p1) (e2,p2) p 
  | isList t1 && isList t2 && isJust tL = do -- <<2>>:: <<>>
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

    if isList t1 && not (isList t2) then
      tell [semmErrorMsg (show t1) (show t2) fileCode p2] >> return (err, p2)
    else do
      if (not (isList t1) && isList t2) || (isList t1 && isList t2) then
        tell [semmErrorMsg (show t2) (show t1) fileCode p1]
      else
        tell [semmErrorMsg "List" (show t1) fileCode p1]
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
      l        = filter (\(e,p) -> typeE e `notElem` [TDummy,TPDummy,TNull]) expr
      expected = typeE $ fst $ head l
      got      = head $ dropWhile (\(e,_) -> typeE e == expected) l
  
    tell [semmErrorMsg (show expected) (show $ typeE $ fst got) fileCode (snd got)]
    return (arr, p)

  where
    e          = map fst expr
    -- p          = snd $ head expr -- change to pos of the expr that has the error. See example in 'list' func
    arrayTypes = map typeE e
    t          = fromMaybe TError (getTLists arrayTypes)
    arr        = ArrayList e (TArray (Literal (Integer $ length e) TInt) t)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the same type list node
list :: [(Expr,Pos)] -> Pos -> MonadSymTab (Expr,Pos)
list [] p = return (ArrayList [] (TList TDummy), p) -- TODO : Recordar quitar el TDummy
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
      l        = filter (\(e,p) -> typeE e `notElem` [TDummy,TPDummy,TNull]) expr
      expected = typeE $ fst $ head l
      got      = head $ dropWhile (\(e,p) ->  typeE e == expected) l
      msg      = semmErrorMsg (show expected) (show $ typeE $ fst got) fileCode (snd got)
    
    tell [msg] >> return (ArrayList exprs (typeE $ fst got), snd got)

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
  else tell [semmErrorMsg "Battle" (show tCond) fileCode p] >> return cond'
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the simple if instruction node
ifSimple :: (Expr,Pos) -> (Expr,Pos) -> (Expr,Pos) -> MonadSymTab (Expr,Pos)
ifSimple (cond,pC) (true,pT) (false,pF) = do
  fileCode <- ask
  let
    (t,msg) = checkIfSimple (typeE cond,pC) (typeE true,pT) (typeE false,pF) fileCode
    e       = (IfSimple cond true false t, pC)

  if null msg then return e
  else tell [msg] >> return e
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
    err = For var e1 e2 i TError

  case typeE e1 of
    TInt -> case typeE e2 of

      TInt -> return $ For var e1 e2 i TVoid
      _ -> tell [semmErrorMsg "Power" (show tE2) fileCode pE2] >> return err

    _ -> tell [semmErrorMsg "Power" (show tE1) fileCode pE1] >> return err
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the determined conditional iteration instruction node
forWhile :: Id -> (Expr,Pos) -> (Expr,Pos) -> (Expr,Pos) -> InstrSeq -> MonadSymTab Instr
forWhile var (e1,pE1) (e2,pE2) (e3,pE3) i = do
  fileCode <- ask
  let 
    tE1    = typeE e1
    tE2    = typeE e2
    tE3    = typeE e3
    forOk  = ForWhile var e1 e2 e3 i TVoid
    forErr = ForWhile var e1 e2 e3 i TError

  case tE1 of
    TInt -> case tE2 of
      TInt -> case tE3 of
        TBool -> if all isVoid i then return forOk else return forErr
        _ -> tell [semmErrorMsg "Battle" (show tE3) fileCode pE3] >> return forErr

      _ -> tell [semmErrorMsg "Power" (show tE2) fileCode pE2] >> return forErr

    _ -> tell [semmErrorMsg "Power" (show tE1) fileCode pE1] >> return forErr
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
  else tell [semmErrorMsg "Array or Kit" (show tE) fileCode p] >> return forErr
-------------------------------------------------------------------------------
    

-------------------------------------------------------------------------------
-- | Creates the indetermined iteration instruction node
while :: (Expr,Pos) -> InstrSeq -> MonadSymTab Instr
while (cond,p) i = do
  fileCode <- ask
  let 
    tc       = typeE cond
    whileOk  = While cond i TVoid
    whileErr = While cond i TError

  case tc of
    TBool -> if all isVoid i then return whileOk else return whileErr
    _ -> tell [semmErrorMsg "Battle" (show tc) fileCode p] >> return whileErr
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

  (symTab, activeScopes, scopes, promises) <- get
  fileCode <- ask
  let
    symInfos = lookupInScopes [1,0] subroutine symTab
    sub      = (Call subroutine args, p)
  
  if isJust symInfos then
    let
      isSubroutine si = getCategory si `elem` [Procedures, Functions]
      subroutine'     = filter isSubroutine (fromJust symInfos)
    in
      if null subroutine' then
        tell [errorMsg "This is not a subroutine" fileCode p] >> return sub
      else
        let 
          nParams = fromJust $ getNParams $ getExtraInfo $ head subroutine'
          nArgs   = length args
        in
          if nArgs == nParams then return (Call subroutine args,p)
          else
            let msj = "Amount of arguments: " ++ show nArgs ++ " not equal to expected:" ++ show nParams
            in tell [errorMsg msj fileCode p] >> return sub
  else
    -- Add a promise to create subroutine
    -- Si no existe construimos la llamada igual para que procCall o funcCall creen la promesa
    -- put(symTab, activeScopes, scopes, promises ++ [Promise subroutine (map typeE args) TPDummy p] )
    return sub
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
procCall:: (Subroutine,Pos) -> MonadSymTab Instr
procCall (procedure@(Call name args), p) = do

  (symTab, activeScopes, scope, promises) <- get
  fileCode <- ask
  let symInfos = lookupInScopes [1,0] name symTab
  
  if isJust symInfos then
    let
      isProcedure symInfo = getCategory symInfo == Procedures
      procedure' = filter isProcedure (fromJust symInfos)
      msg        = errorMsg ("'" ++ name ++ "' is not a procedure") fileCode p
    in
      if null procedure' then
        tell [msg] >> return (ProcCall procedure TError)
      else
        return $ ProcCall procedure TVoid

  else do
    -- If no is declared but maybe(It has to be a promise) is a promise
{-    let promise = getPromiseSubroutine name promises
    
    if isJust promise then do
      let info = [SymbolInfo TVoid 1 Procedures [Params [(typeE e,show i)| (e,i) <- zip args [1..]]]]
  
      put (insertSymbols [name] info symTab, activeScopes, scope, promises)
      return $ ProcCall procedure
    else
      error $ "Error interno:  Procedure '" ++ name ++ "' doesn't have a promise."
-}
    let 
      extraInfo   = Params [(typeE e,show i)| ((e,p),i) <- zip args [1..]]
      newProc     = [SymbolInfo TVoid 1 Procedures [extraInfo]]
      newProm     = Promise name (map (\(e,p) -> (typeE e,p)) args) TVoid p []
      newPromises = promises ++ [newProm]
      newSymTab   = insertSymbols [name] newProc symTab

    put (newSymTab, activeScopes, scope, newPromises)
    return $ ProcCall procedure TVoid
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates function call expresion node
-- NOTE: Its already verified that subroutine's defined with 'call', because
--      its excuted first
funcCall :: (Subroutine,Pos) -> MonadSymTab (Expr,Pos)
funcCall (function@(Call name args), p) = do

  (symTab, activeScopes, scope, promises) <- get
  fileCode <- ask
  let symInfos = lookupInScopes [1,0] name symTab
  
  if isJust symInfos then
    let 
      isFunction symInfo = getCategory symInfo == Functions
      function' = filter isFunction (fromJust symInfos)
      msg       = errorMsg ("'" ++ name  ++ "' is not a function") fileCode p
    in
      if null function' then
        tell [msg] >> return (FuncCall function TError, p)
      else
        return (FuncCall function (getType $ head function'), p)

  else do
    -- If no is declared but maybe(It has to be a promise) is a promise
{-    let promise = getPromiseSubroutine name promises
    
    if isJust promise then do
      let info = [SymbolInfo TDummy 1 Functions [Params [(typeE e,show i)| (e,i) <- zip args [1..]]]]
      put (insertSymbols [name] info symTab, activeScopes, scope, promises)
      return (FuncCall function TPDummy, p)
    else
      error $ "Error interno:  Function '" ++ name ++ "' doesn't have a promise,"
-}
    let
      extraInfo   = Params [(typeE e,show i)| ((e,p),i) <- zip args [1..]]
      newFunc     = [SymbolInfo TPDummy 1 Functions [extraInfo]]
      newProm     = Promise name (map (\(e,p) -> (typeE e,p)) args) TPDummy p []
      newPromises = promises ++ [newProm]
      newSymTab   = insertSymbols [name] newFunc symTab

    put (newSymTab, activeScopes, scope, newPromises)
    return (FuncCall function TPDummy, p)
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
  | TError `notElem` exprsTypes = return $ Print exprs TVoid
  | otherwise = do
    fileCode <- ask
    let 
      l        = filter (\(e,p) -> typeE e /= TDummy && typeE e /= TPDummy) expr
      (e,_)    = head l
      expected = typeE e
      got      = head $ dropWhile (\(e,p) -> typeE e == expected) l
      msg      = semmErrorMsg "Runes" (show $ typeE $ fst got) fileCode (snd got)

    tell [msg] >> return (Print exprs TError)

  where
    exprs      = map fst expr
    exprsTypes = map typeE exprs
    tE         = fromMaybe TError (getTLists exprsTypes)  
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the read instruction node
read' :: (Expr,Pos) -> MonadSymTab (Expr,Pos)
read' (e,p) = do
  fileCode <- ask
  let
    tE = typeE e

  if tE == TStr then return (Read e TStr, p)
  else
    tell [semmErrorMsg "Runes" (show tE) fileCode p] >> return (Read e TError, p)
-------------------------------------------------------------------------------

{-Esta funcion se encarga de verificar que el tipo de la expresión del tamaño de un 
array sea entero--}
tArray :: (Expr, Pos) -> Type -> MonadSymTab Type
tArray (e,p) t
  | te == TInt  = return $ TArray e t
  | te == TPDummy  = do
    ne <- updateExpr e TInt
    return (TArray ne t)

  | otherwise = do
    fileCode <- ask
    tell [semmErrorMsg "TInt" (show te) fileCode p] >> return (TArray e t)

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
  (symTab, activeScopes, _, _) <- get
  fileCode <- ask
  let
    infos = lookupInScopes activeScopes var symTab
    msg   = errorMsg "Variable not declared in active scopes" fileCode p
  
  if isJust infos then return $ Free var TVoid
  else
    tell [msg] >> return (Free var TError)
-------------------------------------------------------------------------------