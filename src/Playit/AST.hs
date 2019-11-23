{- |
 * Creates de abstract syntax tree with type checks
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.AST where

import Control.Monad.Trans.RWS
import Control.Monad (void,when,forM,forM_)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)
import Playit.CheckAST
import Playit.Errors
import Playit.SymbolTable
import Playit.AuxFuncs
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

  if isJust infos then do
    let vars = [Variables, Parameters Value, Parameters Reference]
        isVar symInfo = getCategory symInfo `elem` vars
        v = filter isVar (fromJust infos)

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
  else return (Index var expr TError, pVar) -- change when no exit with first error encounter
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
  else do

    --chequearTipo reg p
    
    let info = lookupInSymTab field symTab

    if isJust info then do
      let isInRegUnion (SymbolInfo _ _ c e) = c == Fields && getReg e == reg
          symbols = filter isInRegUnion (fromJust info )
                  
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
  else return (Desref var TError, p) -- change when no exit with first error encounter
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the TNew type
newType :: Id -> Pos -> MonadSymTab Type
newType tName p = do
  ok <- checkNewType tName p
  
  if ok then return $ TNew tName
  else return TError -- change when no exit with first error encounter
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

  if not iter && asig then return $ Assig lval expr TVoid
  else return $ Assig lval (Literal EmptyVal TError) TError -- change when no exit with first error encounter
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
register :: [(Expr,Pos)] -> (Expr,Pos)
register e
  | TError `notElem` map typeE exprs = (Literal (Register exprs) TRegister, p)
  | otherwise = (Literal (Register exprs) TError, p)

  where
    exprs = map fst e
    p = head $ snd e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Create operators instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the binary operator node
binary :: BinOp -> (Expr,Pos) -> (Expr,Pos) -> Pos -> MonadSymTab (Expr,Pos)
binary op (e1,p1) (e2,p2) p = do
    (ok,tOp) <- checkBinary op e1 e2 p
    
    if ok then return (Binary op e1 e2 tOp, p)
    else return (Binary op e1 e2 TError, p) -- Cambiar cuando no se salga del parser en checkBinary con el error
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the unary operator node
unary :: UnOp -> (Expr,Pos) -> Type -> MonadSymTab (Expr,Pos)
unary op (expr,p) tSpected = do
    (ok,tOp) <- checkUnary (typeE expr) tSpected p
    
    if ok then return (Unary op expr tOp, p)
    else return (Unary op expr TError, p) -- change when no exit with first error encounter
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Create arrays / lists instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the length operator node
len :: Expr -> Pos -> MonadSymTab Expr
len e p
    | isArray t || isList t = return $ Unary Length e TInt
    | otherwise = do     
        fileCode <- ask
        error $ semmErrorMsg "Array or List" (show t) fileCode p
        -- error $ "\n\nError: " ++ fileName ++ ": " ++ show p ++ "\n\t" ++
        --     "La operacion de longitud: '" ++ (show Length) ++ "'," ++ 
        --     " requiere que el tipo de '" ++ (show e) ++ "' sea un arreglo o lista."    
    where
        t = typeE e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the same type array node
array :: [(Expr,Pos)] -> (Expr,Pos)
array e = (ArrayList e (TArray (Literal (Integer $ length e) TInt) t), p)

  where exprs = map fst e
        arrayTypes = map typeE exprs
        fstType = head arrayTypes
        t = if all (== fstType) arrayTypes then fstType else TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the same type list node
list :: [Expr] -> Pos -> MonadSymTab Expr
list [] p = return $ ArrayList [] (TList TDummy) -- TODO : Recordar quitar el TDummy
list e  p
  | isJust t = return $ ArrayList e (TList (fromJust t))
  | otherwise = do
    fileCode <- ask
    error $ semmErrorMsg (show t) (show listTypes) fileCode p

  where
    listTypes = map typeE e
    t = getTLists listTypes
-------------------------------------------------------------------------------
{-
list exprP  = 
    case tipo of
        Just t -> return $ ArrayList exprs (TList t)
        Nothing  -> do
            fileCode <- ask
            let 
                l = filter (\(e,p) -> typeE e /= TDummy && typeE e /= TPDummy) exprP
                (e,p) = head l
                expected = typeE e
                got = head $ dropWhile (\(e,p) ->  typeE e == expected) l
            error $ semmErrorMsg (show expected) (show $ typeE $ fst got) fileCode (snd got)
    where
        exprs = map (\(e,_) -> e) exprP
        mapaTipos = map typeE exprs
        tipo = getTLists mapaTipos
-}

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Creates the selection instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the selection instruction node
if' :: [(Expr, InstrSeq)] -> Pos -> Instr
if' cases p = if and $ void seqs then IF cases TVoid else IF cases TError
  where
    seqs = map snd cases
    void = map $ all isVoid
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the guards of the selection instruction node
guard :: (Expr,Pos) -> InstrSeq -> MonadSymTab (Expr, InstrSeq)
guard (cond,p) i = do
  fileCode <- ask
  let tCond = typeE cond

  if tCond == TBool then return (cond, i)
  else error $ semmErrorMsg "Battle" (show tCond) fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the simple if instruction node
ifSimple :: (Expr,Pos) -> (Expr,Pos) -> (Expr,Pos) -> MonadSymTab (Expr,Pos)
ifSimple (cond,pC) (true,pT) (false,pF) = do

  (ok,t) <- checkIfSimple (typeE cond) (typeE true) (typeE false) p

  if ok then return (IfSimple cond true false t, p)
  else return (IfSimple cond true false TError, p)-- change when no exit with first error encountered
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
  let tE1 = typeE e1
      tE2 = typeE e2

  case typeE e1 of
    TInt -> case typeE e2 of
      TInt -> return $ For var e1 e2 i TVoid
      _ -> do
        error $ semmErrorMsg "Power" (show tE2) fileCode pE2
        return $ For var e1 e2 i TError
    _ -> do
      error $ semmErrorMsg "Power" (show tE1) fileCode pE1
      return $ For var e1 e2 i TError
{- 
  | tE1 == TInt && tE2 == TInt =
      do
          let newI = map (changeTDummyFor TInt st scope) i
          checkInfSup e1 e2 pos st
          return $ For var e1 e2 newI
  --------------------------------------------------------------------------
  | tE1 == TInt =
      error ("\n\nError semantico en segunda la expresion del 'for': '"
              ++ expr2 ++ "', de tipo: " ++ showType tE2
              ++ ". En la linea: " ++ show line ++ "\n")
  --------------------------------------------------------------------------
  | tE2 == TInt =
      error ("\n\nError semantico en la primera expresion del 'for': '"
              ++ expr1 ++ "', de tipo: " ++ showType tE1 ++ ". En la linea: "
              ++ show line ++ "\n")
  --------------------------------------------------------------------------
  | otherwise =
      error ("\n\nError semantico en la primera expresion: '" ++ expr1 ++
              "', de tipo: " ++ showType tE1 ++ ", y segunda expresion: '"
              ++ expr2 ++ "', de tipo: " ++ showType tE2 ++
              ", del 'for'. En la linea: " ++ show line ++ "\n")

  where
      expr1 = showE e1
      expr2 = showE e2
      tE1 = typeE e1
      tE2 = typeE e2
-}
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Creates the determined conditional iteration instruction node
forWhile :: Id -> (Expr,Pos) -> (Expr,Pos) -> (Expr,Pos) -> InstrSeq -> MonadSymTab Instr
forWhile var (e1,pE1) (e2,pE2) (e3,pE3) i = do
  fileCode <- ask
  let tE1 = typeE e1
      tE2 = typeE e2
      tE3 = typeE e3

  case tE1 of
    TInt -> case tE2 of
      TInt -> case tE3 of
        TBool ->
          if all isVoid i then return $ ForWhile var e1 e2 e3 i TVoid
          else return $ ForWhile var e1 e2 e3 i TError
        _ -> do
          error $ semmErrorMsg "Battle" (show tE3) fileCode pE3
          return $ ForWhile var e1 e2 e3 i TError
      _ -> do
        error $ semmErrorMsg "Power" (show tE2) fileCode pE2
        return $ ForWhile var e1 e2 e3 i TError
    _ -> do
      error $ semmErrorMsg "Power" (show tE1) fileCode pE1
      return $ ForWhile var e1 e2 e3 i TError
{-
  | tE1 == TInt && tE2 == TInt && tE3 == TBool =
      do
          let newI = map (changeTDummyFor TInt st scope) i
          checkInfSup e1 e2 pos st
          return $ For var e1 e2 newI st
  --------------------------------------------------------------------------
  | tE1 == TInt =
      error ("\n\nError semantico en segunda la expresion del 'for': '"
              ++ expr2 ++ "', de tipo: " ++ showType tE2
              ++ ". En la linea: " ++ show line ++ "\n")
  --------------------------------------------------------------------------
  | tE2 == TInt =
      error ("\n\nError semantico en la primera expresion del 'for': '"
              ++ expr1 ++ "', de tipo: " ++ showType tE1 ++ ". En la linea: "
              ++ show line ++ "\n")
  --------------------------------------------------------------------------
  | tE3 == TBool =
      error ("\n\nError semantico en la primera expresion: '" ++ expr1 ++
              "', de tipo: " ++ showType tE1 ++ ", y segunda expresion: '"
              ++ expr2 ++ "', de tipo: " ++ showType tE2 ++
              ", del 'for'. En la linea: " ++ show line ++ "\n")
  --------------------------------------------------------------------------
  | otherwise =
      error ("\n\nError semantico en la primera expresion: '" ++ expr1 ++
              "', de tipo: " ++ showType tE1 ++ ", segunda expresion: '"
              ++ expr2 ++ "', de tipo: " ++ showType tE2 ++
              ", y tercera expresion: '" ++ expr3 ++ "', de tipo: " ++ showType tE3 ++
              ", del 'for'. En la linea: " ++ show line ++ "\n")
  where
      expr1 = showE e1
      expr2 = showE e2
      expr3 = showE e3
      tE1 = typeE e1
      tE2 = typeE e2
      tE3 = typeE e3 
-}


-------------------------------------------------------------------------------
-- | Creates the determined iteration instruction node for arrays / list
forEach :: Id -> (Expr,Pos) -> InstrSeq -> MonadSymTab Instr
forEach var (e,p) i = do
  fileCode <- ask
  let te = typeE e
      check = ForEach var e i TVoid
      frerr = ForEach var e i TError
  
  case te of
    (TArray _ _) -> if all isVoid i then return check else return frerr
    (TList _) -> if all isVoid i then return check else return frerr
    _ -> error (semmErrorMsg "Array or Kit" (show te) fileCode p) >> return $ frerr
-------------------------------------------------------------------------------
    

-------------------------------------------------------------------------------
-- | Creates the indetermined iteration instruction node
while :: (Expr,Pos) -> InstrSeq -> MonadSymTab Instr
while (cond,p) i = do
  fileCode <- ask
  let tc = typeE cond
      check = While cond i TVoid
      wherr = While cond i TError

  case tc of
    TBool -> if all isVoid i then return check else return wherr 
    _ -> error (semmErrorMsg "Battle" (show tc) fileCode p) >> return wherr
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                   Procedures / Functions calls nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
checkPromises ::  MonadSymTab ()
checkPromises = do
  (symTab, activeScopes, scopes , promises) <- get
  fileCode <- ask

  forM promises $ \(Promise name args t p ) -> do
    if t /= TPDummy then
      error $ errorMsg ("Function '" ++ name ++ "' is not defined") fileCode p
    else 
      error $ errorMsg ("Procedure '" ++ name ++ "' is not defined") fileCode p
    return ()
  
  return ()
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
updateInfoSubroutine:: Id -> Category -> [(Type,Id)] -> Type -> MonadSymTab ()
updateInfoSubroutine name cat p t = do
  (symTab, activeScopes, scopes, promises) <- get
  fileCode <- ask
  let paramsF = reverse p
      promise = getPromiseSubroutine name promises

  when (isJust promise) $ do
    let promise' = fromJust promise
        paramsP = getParamsPromise promise'
        typeP = getTypePromise promise'
        errorTL = dropWhile (\((t1,_),(t2,_)) -> t1 == t2) (zip paramsP paramsF)

    if  any (/=True) [t1 == t2 | (t1,(t2,id2)) <- zip paramsP paramsF ] then
      error $ errorMsg "Wrong type of arguments" fileCode (getPosPromise promise')
    else
      if length paramsP /= length paramsF then
        let msj = "Amount of arguments: " ++ show (length paramsP) ++
                " not equal to expected:" ++ show (length paramsF)
        in error $ errorMsg msj fileCode (getPosPromise promise')
      else
        if  not $ null errorTL then do
          let ((gotType,pGotType),(expectedType,_)) = head errorTL
          error $ semmErrorMsg (show expectedType) (show gotType) fileCode pGotType
        else
          if typeP /= TPDummy && typeP /= t then
            error $ semmErrorMsg (show typeP) (show t) fileCode (getPosPromise promise')
          else do
            checkExpresionesPromise promise' t
            -- Quitamos la promesa
            -- (symTab, activeScopes, scopes , promises) <- get
            put(symTab, activeScopes, scopes ,filter (\p -> getIdPromise p /= name) promises)
            return () 
{-        if typeP /= TPDummy && typeP /= t then
          error $ semmErrorMsg (show typeP) (show t) fileCode (getPosPromise promise')
        else do
          put(symTab, activeScopes, scopes, filter (/= promise') promises)
          return () 
-}
  updateExtraInfo name cat [Params paramsF]
  updateType name 1 t
  return ()
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------

-- | Creates subroutine call instruction node
-- Considerar quitar esta función
call :: Id -> Params -> Pos -> MonadSymTab (Subroutine,Pos)
call subroutine args p = do
  (symTab, activeScopes, scopes, promises) <- get
  fileCode <- ask
  let symInfos = lookupInScopes [1,0] subroutine symTab
  
  if isJust symInfos then do
    let isSubroutine si = getCategory si `elem` [Procedures, Functions]
        subroutine' = filter isSubroutine (fromJust symInfos)

    if null subroutine' then
      error $ errorMsg "This is not a subroutine" fileCode p
    else do
      let nParams = fromJust $ getNParams $ getExtraInfo $ head subroutine'
          nArgs = length args
      
      if nArgs == nParams then
        return (Call subroutine args,p)
      else
        let msj = "Amount of arguments: " ++ show nArgs ++ " not equal to expected:" ++ show nParams
        in error $ errorMsg msj fileCode p
  else do
    -- Add a promise to create subroutine
    -- Si no existe construimos la llamada igual para que procCall o funcCall 
    -- creen la promesa
    -- put(symTab, activeScopes, scopes, promises ++ [Promise subroutine (map typeE args) TPDummy p] )
    return (Call subroutine args, p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
procCall:: (Subroutine,Pos) -> MonadSymTab Instr
procCall (procedure@(Call name args), p) = do

  (symTab, activeScopes, scope, promises) <- get
  fileCode <- ask
  let symInfos = lookupInScopes [1,0] name symTab
  
  if isJust symInfos then do
    let isProcedure symInfo = getCategory symInfo == Procedures
        procedure' = filter isProcedure (fromJust symInfos)

    if null procedure' then
      error $ errorMsg ("'" ++ name  ++ "' is not a procedure") fileCode p
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
    let extraInfo = Params [(typeE e,show i)| ((e,p),i) <- zip args [1..]]
        newProc = [SymbolInfo TVoid 1 Procedures [extraInfo]]
        newProm = Promise name (map (\(e,p) -> (typeE e, p)) args) TVoid p []
        newPromises = promises ++ [newProm]
        newSymTab = insertSymbols [name] newProc symTab

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
  
  if isJust symInfos then do
    let isFunction symInfo = getCategory symInfo == Functions
        function' = filter isFunction (fromJust symInfos)

    if null function' then
      error $ errorMsg ("'" ++ name  ++ "' is not a function") fileCode p
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
    let extraInfo = Params [(typeE e,show i)| ((e,p),i) <- zip args [1..]]
        newFunc = [SymbolInfo TPDummy 1 Functions [extraInfo]]
        newProm = Promise name (map (\(e,p) -> (typeE e, p)) args) TPDummy p []
        newPromises = promises ++ [newProm]
        newSymTab = insertSymbols [name] newFunc symTab

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
print' e p
  | TError `notElem` map typeE e = return $ Print e TVoid
  | otherwise = do
    fileCode <- ask
    error $ semmErrorMsg "Good-typed expression" "Type error" fileCode p 
-------------------------------------------------------------------------------
{-
    | all (\t -> t == TStr || t == TPDummy) mapaTipos = return $ Print exps TVoid
    | otherwise = do
        let 
            l = filter (\(e,p) -> typeE e /= TDummy && typeE e /= TPDummy) exprP
            (e,p) = head l
            expected = typeE e
            got = head $ dropWhile (\(e,p) ->  typeE e == expected) l
        fileCode <- ask
        error $ semmErrorMsg "Runes" (show $ typeE $ fst got) fileCode (snd got)
    where
        exps = map (\(e,_) -> e) exprP
        mapaTipos = map typeE exps
        typeExps = case (getTLists mapaTipos) of  
            Just t -> t
            Nothing -> TError
-}

-------------------------------------------------------------------------------
-- | Creates the read instruction node
read' :: (Expr,Pos) -> MonadSymTab (Expr,Pos)
read' (e,p) = do
  fileCode <- ask
  let tE = typeE e

  if tE == TStr then return (Read e TStr, p)
  else
    error (semmErrorMsg "Runes" (show tE) fileCode p) >> return (Read e TError, p)
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
  else error $ errorMsg "Variable not declared in active scopes" fileCode p
-------------------------------------------------------------------------------