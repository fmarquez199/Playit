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
  let infos = lookupInScopes activeScopes id symTab

  if isJust infos then

    let vars          = [Variables, Parameters Value, Parameters Reference]
        isVar symInfo = getCategory symInfo `elem` vars
        v             = filter isVar (fromJust infos)
    in
    if null v then
      error $ errorMsg "This is not a variable" fileCode p
    else
      return (Var id (getType $ head v), p)

  else error $ errorMsg "Variable not declared in active scopes" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates indexed variables node
index :: (Var,Pos) -> (Expr,Pos) -> MonadSymTab (Var,Pos)
index (var,pVar) (expr,pExpr) = do
  (ok,tVar) <- checkIndex var (typeE expr) pVar pExpr
  
  if ok then return (Index var expr tVar, pVar)
  else return (Index var expr TError, pVar)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the registers / unions fields
field :: (Var,Pos) -> Id -> Pos -> MonadSymTab (Var,Pos)
field (var,pVar) field pField = do
  (symTab, _, _, _) <- get
  fileCode@(file,code) <- ask
  
  -- Verify type 'var' is register / union
  let reg = case baseTypeVar var of 
              (TNew name) -> name
              _           -> ""
  
  if reg == "" then -- Type error
    error $ errorMsg "Type of field isn't a register or union" fileCode pVar

  else

    --chequearTipo reg p
    
    let info = lookupInSymTab field symTab
    in
    if isJust info then

      let isInRegUnion (SymbolInfo _ _ c e) = c == Fields && getReg e == reg
          symbols = filter isInRegUnion (fromJust info )
      in
      if null symbols then
        error $ errorMsg ("Field not in '"++reg++"'") fileCode pField
      else 
        return (Field var field (getType $ head symbols), pField)

    else
      error $ errorMsg "Field not declared" fileCode pField
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the desreferentiation variable node
desref :: (Var,Pos) -> MonadSymTab (Var,Pos)
desref (var,p) = do
  (ok,tVar) <- checkDesref (typeVar var) p
  
  if ok then return (Desref var tVar, p)
  else return (Desref var TError, p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the TNew type
newType :: Id -> Pos -> MonadSymTab Type
newType tName p = do
  ok <- checkNewType tName p
  
  if ok then return $ TNew tName
  else return TError
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
  iter <- checkIterVar lval
  asig <- checkAssig (typeVar lval) expr pE

  if not iter && asig then do
    expr' <- updateExpr expr (typeVar lval)
    return $ Assig lval expr' TVoid

  else return $ Assig lval (Literal EmptyVal TError) TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- |
-- TODO: Cambiar TRegister por el TNew correspondiente
register :: [(Expr,Pos)] -> Pos -> (Expr,Pos)
register [] p' = (Literal (Register []) TRegister, p')
register e p'
  | TError `notElem` map typeE exprs = (Literal (Register exprs) TRegister, p)
  | otherwise = (Literal (Register exprs) TError, p)

  where
    exprs = map fst e
    p     = snd $ head e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Create operators instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the binary operator node
binary :: BinOp -> (Expr,Pos) -> (Expr,Pos) -> Pos -> MonadSymTab (Expr,Pos)
binary op e1 e2 p = do
  (ok,bin) <- checkBinary op e1 e2 p
  
  return (bin, p)
  -- else return (Binary op e1 e2 TError, p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the unary operator node
unary :: UnOp -> (Expr,Pos) -> Type -> MonadSymTab (Expr,Pos)
unary op (expr,p) tSpected = do
  (ok,tOp) <- checkUnary op (typeE expr) tSpected p
  
  if ok then return (Unary op expr tOp, p)
  else return (Unary op expr TError, p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Create arrays / lists instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserta en una lista un nuevo elemento en indice 0
anexo :: BinOp -> (Expr,Pos) -> (Expr,Pos) -> MonadSymTab Expr
anexo op (e1,p1) (e2,p2) = do
  (ok,tOp) <- checkAnexo (e1,p1) (e2,p2)
  
  if ok then do

    let exprR = Binary op e1 e2 tOp

    nexpr <- updateExpr exprR (fromJust $ getTLists [TList (typeE e1),typeE e2])

    let allidsp = getRelatedPromises nexpr
    
    if not $ null allidsp then addLateCheck nexpr nexpr [p1,p2] allidsp >> return nexpr
    else return exprR 

  else return $ Binary op e1 e2 TError -- change when no exit with first error encounter
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Create concat 2 lists operator node
concatLists :: BinOp -> (Expr,Pos) -> (Expr,Pos) -> Pos -> MonadSymTab Expr
concatLists op (e1,p1) (e2,p2) p 
  | isList t1 && isList t2 && isJust tL  = do -- <<2>>:: <<>>
    let 
      exprR   = Binary Concat e1 e2 (fromJust tL)
      allidsp = getRelatedPromises exprR
    
    unless (null allidsp) $ do
      addLateCheck exprR exprR [p1,p2,p] allidsp
      --TODO: HAcer inferencias adentro de una lista

    return exprR

  | otherwise = do
    (fileName,code) <- ask
    --error $ semmErrorMsg (show baseT1) (show baseT2) fileCode p2
    error ("\n\nError: " ++ show fileName ++ ": " ++ show p ++ "\n  "
          ++ "Operation Concat needs expression '" ++ show e1 ++
          "' and expression '" ++ show e2 ++ "' to be same-type lists.")
    return $ Binary Concat e1 e2 TError

  where
    t1 = typeE e1
    t2 = typeE e2
    tL = getTLists [t1,t2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the same type array node
array :: [(Expr,Pos)] -> (Expr,Pos)
array expr = (ArrayList e (TArray (Literal (Integer $ length e) TInt) t), p)

  where
    e          = map fst expr
    p          = snd $ head expr
    arrayTypes = map typeE e
    fstType    = head arrayTypes
    t          = if all (==fstType) arrayTypes then fstType else TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the same type list node
list :: [(Expr,Pos)] -> Pos -> MonadSymTab (Expr,Pos)
list [] p = return (ArrayList [] (TList TDummy), p) -- TODO : Recordar quitar el TDummy
list expr p
  | isJust t =
    let list = ArrayList exprs (TList (fromJust t))
        ids  = getRelatedPromises list
    in addLateCheck list list (map snd expr) ids >> return (list, p)

  | otherwise = do
    fileCode <- ask
    let 
      l        = filter (\(e,p) -> typeE e `notElem` [TDummy,TPDummy,TNull]) expr
      (e,_)    = head l
      expected = typeE e
      got      = head $ dropWhile (\(e,p) ->  typeE e == expected) l

    error $ semmErrorMsg (show expected) (show $ typeE $ fst got) fileCode (snd got)

  where
    exprs     = map fst expr
    -- p         = head $ snd e
    listTypes = map typeE exprs
    t         = getTLists listTypes
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
  let tCond = typeE cond

  if tCond == TBool then return (cond, i)
  else error $ semmErrorMsg "Battle" (show tCond) fileCode p-- change when no exit with first error encountered
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the simple if instruction node
ifSimple :: (Expr,Pos) -> (Expr,Pos) -> (Expr,Pos) -> MonadSymTab (Expr,Pos)
ifSimple (cond,pC) (true,pT) (false,pF) = do
  fileCode <- ask
  (ok,t) <- checkIfSimple (typeE cond,pC) (typeE true,pT) (typeE false,pF) fileCode

  if ok then return (IfSimple cond true false t, pC)
  else return (IfSimple cond true false TError, pC)
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

  case typeE e1 of
    TInt -> case typeE e2 of

      TInt -> return $ For var e1 e2 i TVoid
      _ ->
        return $ For var e1 e2 i TError
        -- error $ semmErrorMsg "Power" (show tE2) fileCode pE2

    _ ->
      return $ For var e1 e2 i TError
      -- error $ semmErrorMsg "Power" (show tE1) fileCode pE1
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

        TBool ->
          if all isVoid i then return forOk else return forErr
        _ ->
          return forErr
          -- error $ semmErrorMsg "Battle" (show tE3) fileCode pE3

      _ ->
        return forErr
        -- error $ semmErrorMsg "Power" (show tE2) fileCode pE2

    _ ->
      return forErr
      -- error $ semmErrorMsg "Power" (show tE1) fileCode pE1
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
  else return forErr >> error (semmErrorMsg "Array or Kit" (show tE) fileCode p)
-------------------------------------------------------------------------------
    

-------------------------------------------------------------------------------
-- | Creates the indetermined iteration instruction node
while :: (Expr,Pos) -> InstrSeq -> MonadSymTab Instr
while (cond,p) i = do
  fileCode <- ask
  let 
    tc         = typeE cond
    whileOk    = While cond i TVoid
    whileError = While cond i TError

  case tc of
    TBool -> if all isVoid i then return whileOk else return whileError 
    _ -> return whileError >> error (semmErrorMsg "Battle" (show tc) fileCode p)
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
  let symInfos = lookupInScopes [1,0] subroutine symTab
  
  if isJust symInfos then
    let
      isSubroutine si = getCategory si `elem` [Procedures, Functions]
      subroutine'     = filter isSubroutine (fromJust symInfos)
    in
      if null subroutine' then
        error $ errorMsg "This is not a subroutine" fileCode p
      else
        let 
          nParams = fromJust $ getNParams $ getExtraInfo $ head subroutine'
          nArgs   = length args
        in
          if nArgs == nParams then return (Call subroutine args,p)
          else
            let msj = "Amount of arguments: " ++ show nArgs ++ " not equal to expected:" ++ show nParams
            in error $ errorMsg msj fileCode p
  else
    -- Add a promise to create subroutine
    -- Si no existe construimos la llamada igual para que procCall o funcCall creen la promesa
    -- put(symTab, activeScopes, scopes, promises ++ [Promise subroutine (map typeE args) TPDummy p] )
    return (Call subroutine args, p)
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
      procedure'          = filter isProcedure (fromJust symInfos)
    in
      if null procedure' then
        return (ProcCall procedure TError) >> error $ errorMsg ("'" ++ name  ++ "' is not a procedure") fileCode p
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
    in
      if null function' then
        return (FuncCall function TError, p) >> error $ errorMsg ("'" ++ name  ++ "' is not a function") fileCode p
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
    return $ Print exprs TError

    fileCode <- ask
    let 
      l        = filter (\(e,p) -> typeE e /= TDummy && typeE e /= TPDummy) expr
      (e,_)    = head l
      expected = typeE e
      got      = head $ dropWhile (\(e,p) -> typeE e == expected) l

    error $ semmErrorMsg "Runes" (show $ typeE $ fst got) fileCode (snd got)

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
  let tE = typeE e

  if tE == TStr then return (Read e TStr, p)
  else
    return (Read e TError, p) >> error (semmErrorMsg "Runes" (show tE) fileCode p)
-------------------------------------------------------------------------------


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
  let infos = lookupInScopes activeScopes var symTab
  
  if isJust infos then return $ Free var TVoid
  else
    return (Free var TError) >> error $ errorMsg "Variable not declared in active scopes" fileCode p
-------------------------------------------------------------------------------