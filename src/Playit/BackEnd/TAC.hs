{- |
 * Three address code
 *
 * Copyright : (c) 
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.BackEnd.TAC (tacInitState,gen) where

import Control.Monad.IO.Class      (liftIO)
import Control.Monad               (when,unless)
import Control.Monad.Trans.RWS     (ask,tell,get,put)
import Data.List.Split             (splitOn)
import Data.Maybe                  (fromJust,isNothing)
import Playit.BackEnd.Utils    
import Playit.BackEnd.Types
import Playit.FrontEnd.SymbolTable (lookupInSymTab)
import Playit.FrontEnd.Types
import Playit.FrontEnd.Utils       (typeVar,baseTypeT,getName,isArrLst)
import qualified Data.Map               as M
import qualified Playit.BackEnd.TACType as T


-- Colocar los temps de print, read y null al inicio?
tacInitState :: SymTab -> Operands
tacInitState = Operands M.empty temps M.empty [] brk cont 0 []
  where
    printReg = Temp "_print" (-1)
    readReg  = Temp "_read" (-1)
    nullReg  = Temp "_null" (-1)
    cont     = tacLabel "cont"
    brk      = tacLabel "brk"
    temps    = M.fromList [(printReg,False), (readReg,False), (nullReg,False)]


gen :: Instr -> TACMonad ()
gen ast = tell (tacCall Nothing "_main" 0 ++ tacNewLabel (tacLabel "_main")) >> genCode ast

-- 
genCode :: Instr -> TACMonad ()
genCode i = case i of
  (Program is _)             -> mapM_ genCode is >> genSubroutines
  (Assigs is _)              -> mapM_ genCode is
  (Assig v e _)              -> newLabel >>= genAssig v e
  (Break _)                  -> genBreak
  (Continue _)               -> genContinue
  (For n e1 e2 is _)         -> breakI   >>= genFor n e1 e2 is
  (ForEach n e is _)         -> return () -- breakI >>= genForEach n e is
  (ForWhile n e1 e2 e3 is _) -> breakI   >>= genForWhile n e1 e2 e3 is
  (IF gs _)                  -> newLabel >>= genIF gs
  (While e is _)             -> breakI   >>= genWhile e is
  (Print es _)               -> return () -- genPrint es
  (Free id _)                -> genFree id
  (ProcCall s _)             -> genProcCall s
  (Return e _)               -> genExpr e >>= genReturn


genSubroutines :: TACMonad ()
genSubroutines = do
  Operands{subs = subroutines} <- get
  mapM_ genSubroutine subroutines


genSubroutine :: (Id,InstrSeq,Bool) -> TACMonad ()
genSubroutine (s,i,isProc) =
  resetOffset >> tell (tacNewLabel $ tacLabel s) >> mapM_ genCode i >> when isProc (genReturn Nothing)
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                            TAC Instructions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- Registros/Uniones
genAssig :: Var -> Expr -> TACOP -> TACMonad ()
genAssig var e nextL = case typeVar var of
  -- Si es el operador ternario la expr puede ser bool o no
  TBool -> do
    vTemp  <- genVar var (typeVar var)
    trueL  <- newLabel
    falseL <- newLabel
    genBoolExpr e trueL falseL
    tell (tacNewLabel trueL)
    tell (tacAssign vTemp $ tacConstant ("True",TBool))
    tell (tacGoto nextL)
    tell (tacNewLabel falseL)
    tell (tacAssign vTemp $ tacConstant ("False",TBool))
    tell (tacNewLabel nextL)
-- Registros y uniones
  TNew n -> 
    return ()
--
  _ -> do
    eTemp <- genExpr e
    vTemp <- genVar var (typeVar var)
    {- 
      if isField var then

      else
        base/fp[var.offset] := e
    -}
    unless (isArrLst e) $ tell $ tacAssign vTemp eTemp


-- 
genFor :: Id -> Expr -> Expr -> InstrSeq -> TACOP -> TACMonad ()
genFor n e1 e2 is nextL = forComparison n e1 e2 nextL >>= forInstrs is nextL


{-- TODO
  controller var <- arrLst:
  InstrSeq
  .~

  |
  v

  var = arrLst
  controller dummy = 0 -> #var:
    InstrSeq[var := var[dummy]]
  .~
--}
-- 
-- genForEach :: Id -> Expr -> InstrSeq -> TACMonad ()
-- genForEach n e is = return [] -- concatTAC var for
  -- where
  --   var = genCode (Assig (Var n (typeE e)) e TVoid)


-- 
genForWhile :: Id -> Expr -> Expr -> Expr -> InstrSeq -> TACOP -> TACMonad ()
genForWhile n e1 e2 cond is nextL = do
  iteration <- forComparison n e1 e2 nextL
  genBoolExpr cond fall nextL
  forInstrs is nextL iteration


-- 
genIF :: [(Expr, InstrSeq)] -> TACOP -> TACMonad ()
genIF [] nextL               = tell (tacNewLabel nextL)
genIF ((e, is):guards) nextL = do
  let isLast = null guards
  falseL <- if isLast then return nextL else newLabel
  genBoolExpr e fall falseL
  mapM_ genCode is
  unless isLast $ tell (tacGoto nextL)
  unless isLast $ tell (tacNewLabel falseL)
  when isLast $ return ()
  genIF guards nextL


-- 
genWhile :: Expr -> InstrSeq -> TACOP -> TACMonad ()
genWhile e is nextL = do
  begin <- continue
  tell (tacNewLabel begin)
  genBoolExpr e fall nextL
  mapM_ genCode is
  tell (tacGoto begin)
  tell (tacNewLabel nextL)


-- TODO: relacionado con arrays/lists
-- genPrint :: [Expr] -> TACMonad ()
-- genPrint es = return []
  -- es'    <- foldl concatTAC (return []) $ map unWrapExprCode es
  -- let
  --   len   = length es - 1
  --   t'  x = "$print[" ++ x ++ "]"
  --   l'  x = es !! x 
  --   lvi x = tacVariable $ SymbolInfo (t' x) TDummy 1 Constants ("print",-1) []
  --   rvi x = tacConstant (show (l' x), TDummy)

  -- return $ es' ++ [ T.TACC T.Assign (lvi (show x)) (rvi x) Nothing | x <- [0..len] ]


-- 
genFree :: Id -> TACMonad ()
genFree varId = do
  Operands{vars = vs, astST = st} <- get
  let -- Si se cambia por 'free Expr' no tendria que buscar en la symtab
    varT = symType . head . fromJust $ lookupInSymTab varId st
    var  = fromJust $ M.lookup (Var varId varT) vs
  tell [tacParam var]
  tell (tacCall Nothing "free" 1)


-------------------------------------------------------------------------------
-- Hace prologo
genProcCall :: Subroutine -> TACMonad ()
genProcCall (Call s params) = do
  pushSubroutine s True
  genParams (map fst params)
  -- Prologo antes de pasar el poder al proc
  tell (tacCall Nothing s $ length params)

-- 
genParams :: [Expr] -> TACMonad ()
genParams params = do
  operands <- mapM genExpr params
  tell $ map tacParam operands
-------------------------------------------------------------------------------


-- Hace epilogo
genReturn :: TACOP -> TACMonad ()
genReturn e = tell [T.TACC T.Return Nothing e Nothing]


-- 
genContinue :: TACMonad ()
genContinue = do
  Operands{contL = continue} <- get
  tell (tacGoto continue)


-- 
genBreak :: TACMonad ()
genBreak = do
  Operands{brkL = break} <- get
  tell (tacGoto break)


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                             TAC Expressions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


genExpr :: Expr -> TACMonad TACOP
genExpr e = case e of
  Literal l t         -> genLiteral l t
  Variable v t        -> genVar v t
  Unary u e t         -> genUnOp u e t
  Binary b e1 e2 t    -> do
    e1Temp <- genExpr e1
    e2Temp <- genExpr e2
    genBinOp b t e1Temp e2Temp
  IfSimple eB eT eF t -> genTerOp eB eT eF t
  ArrayList es t      ->
    let width = getWidth (baseTypeT t)
    in pushOffset width >>= newTemp >>= genArrayList es width 0
  -- Null                -> genNull
  -- Read e _            -> genRead e
  FuncCall s _        -> genFuncCall s
  IdType t            -> genType t
  _ -> return Nothing


-- Array de bools, apuntador a bool
genBoolExpr :: Expr -> TACOP -> TACOP -> TACMonad ()
genBoolExpr e trueL falseL =  
  case e of
    Literal (Boolean True) _  -> unless (isFall trueL) $ tell (tacGoto trueL)
    Literal (Boolean False) _ -> unless (isFall falseL) $ tell (tacGoto falseL)
    Unary Not e _             -> genBoolExpr e falseL trueL
  -- Variables
  -- Comparators
    Binary op e1 e2 _ | op `elem` [Greater,GreaterEq,Less,LessEq,Eq,NotEq] -> do
      leftExpr  <- genExpr e1
      rightExpr <- genExpr e2
      genComparison leftExpr rightExpr trueL falseL op
  -- Conjunction and disjunction
    Binary op e1 e2 _ | op `elem` [And,Or] -> do
      e1TrueL <- -- for `or` we need to generate a new `true` label if the current is `fall`
        if op == Or then if isFall trueL then newLabel else return trueL
        else return fall
      e1FalseL <-
        if op == And then if isFall falseL then newLabel else return falseL
        else return fall
      
      genBoolExpr e1 e1TrueL e1FalseL
      genBoolExpr e2 trueL falseL
      if op == And then
        when (isFall falseL) $ tell (tacNewLabel e1FalseL)
      else
        when (isFall trueL) $ tell (tacNewLabel e1TrueL)
  -- Functions
  -- Ternary operator
  -- 
    e -> error $ "Unexpected boolean expression:  " ++ show e


genComparison :: TACOP -> TACOP -> TACOP -> TACOP -> BinOp -> TACMonad ()
genComparison leftExpr rightExpr trueL falseL op = do
    let
      trueNotFall  = not $ isFall trueL
      falseNotFall = not $ isFall falseL
    
    if trueNotFall && falseNotFall then
      tell (tac (binOpToTACOP op) leftExpr rightExpr trueL) >> tell (tacGoto falseL)
    else
      if trueNotFall then tell (tac (binOpToTACOP op) leftExpr rightExpr trueL)
      else
        when falseNotFall $
          tell (tac (negation $ binOpToTACOP op) leftExpr rightExpr falseL)


-- 
-- genNull :: TACMonad TACOP
-- genNull = do
  -- Operands{offS = actO:_} <- get
  -- let pointer = tacVariable $ SymbolInfo "$null" TNull (-1) Constants actO []
  -- return ([T.TACC T.Assign pointer Nothing Nothing], pointer)


-- | Generates the TAC code for literals
{- TODO:
  EmptyVal -> Usado cuando no se coloca msj en el read
  Register
  Union
  ArrLst -> Realmente se llega hasta aqui?
  String
-}
genLiteral :: Literal -> Type -> TACMonad TACOP
genLiteral l typeL = do
  -- actO <- pushOffset (getWidth typeL)
  -- lv   <- newTemp actO
  -- pushLiteral l lv
  -- let
  
  case l of
    {-
      ArrLst elems -> -- No llega aqui
        liftIO (print ("Llegue a literal ArrLst: " ++ show elems)) >> return (tacLabel "lit arrLst")
      Str s -> do
    -}
    -- EmptyVal -> return ([T.TACC T.Assign lv rv Nothing], lv)
    -- (Register es) -> return ([T.TACC T.Assign lv rv Nothing], lv)
    _ ->
      return $ tacConstant (show l, typeL)


-- 
{- TODO:
  casos bloques anidados que acceden a los ids
  Param Id Type Ref
  Field Var Id Type
  Index
  Desref
-}
genVar :: Var -> Type -> TACMonad TACOP
genVar var tVar = do
  -- actO <- pushOffset (getWidth tVar)
  -- lv   <- newTemp actO
  tacVar  <- pushVariable var tVar
  
  -- let
    -- refVS = M.insert (getRefVar var) lv vs -- var o *var?
    -- deref = ([T.TACC T.Deref lv rv Nothing], lv)
  
  case var of
    Param n t ref -> error "Un parametro no deberia poder estar en una asignacion"
    Desref _ t    -> return() -- tell (deref lv rv) >> return lv
    Index _ e _   -> return()
    Field v f t   -> return()
    _     -> return tacVar


-- 
-- New IdType
genUnOp :: UnOp -> Expr -> Type -> TACMonad TACOP
genUnOp op e tOp = do
  rv   <- genExpr e
  actO <- pushOffset (getWidth tOp)
  lv   <- newTemp actO
  
  case op of
    Length   -> tell (tacLen lv rv)   >> return lv
    Negative -> tell (tacMinus lv rv) >> return lv
    New      -> tell (tacNew lv rv)   >> return lv -- relacionado con IdType
    charOp   -> do
      l0 <- newLabel
      l1 <- newLabel
      l2 <- newLabel
      let
        c0    = tacConstant ("0", TInt)
        c25   = tacConstant ("25", TInt)
        c32   = tacConstant ("32", TInt)
        check = tacGte lv c0 l0 ++ tacGoto l2 ++ tacNewLabel l0 ++ tacSub lv rv c25
        goNew = tacGoto l2 ++ tacNewLabel l1

      if charOp == UpperCase then do
        tell (tacSub lv rv $ tacConstant ("97", TInt))
        tell check
        tell (tacGte lv c0 l0)
        tell goNew
        tell (tacSub lv rv c32)
        tell (tacNewLabel l2)
        return lv
      else do
        tell (tacSub lv rv $ tacConstant ("65", TInt))
        tell check
        tell (tacLte lv c0 l0)
        tell goNew
        tell (tacAdd lv rv c32)
        tell (tacNewLabel l2)
        return lv


-- 
genBinOp :: BinOp -> Type -> TACOP -> TACOP -> TACMonad TACOP
genBinOp op tOp rv1 rv2 = do
  actO <- pushOffset (getWidth tOp)
  lv   <- newTemp actO
  
  case op of
  -- Aritmethics
    op -> tell (tac (binOpToTACOP op) lv rv1 rv2)  >> return lv
  -- Lists
    -- Anexo  -> return (e1Code ++ e2Code ++ [T.TACC T.Anexo lvt e1Temp e2Temp], lvt)
    -- Concat -> return (e1Code ++ e2Code ++ [T.TACC T.Concat lvt e1Temp e2Temp], lvt)


-- 
genTerOp :: Expr -> Expr -> Expr -> Type -> TACMonad TACOP
genTerOp eB eT eF tOp = do
  actO   <- pushOffset (getWidth tOp)
  lv     <- newTemp actO
  next   <- newLabel
  falseL <- newLabel
  genBoolExpr eB fall falseL
  eTTemp <- genExpr eT
  tell (tacAssign lv eTTemp)
  tell (tacGoto next)
  tell (tacNewLabel falseL)
  eFTemp <- genExpr eF
  tell (tacAssign lv eFTemp)
  tell (tacNewLabel next)
  return lv


-- 
genArrayList :: [Expr] -> Int -> Int -> TACOP -> TACMonad TACOP
genArrayList [] _ _ _                         = return Nothing
genArrayList (elem:elems) width index arrTemp = do
  elemTemp <- genExpr elem
  tell (tacSet arrTemp (tacConstant (show index,TInt)) elemTemp)
  actO     <- pushOffset width
  genArrayList elems width (index + 1) (modifyOffSet arrTemp actO)


-- 
-- genRead :: Expr -> MTACExpr
-- genRead e = do
  -- msg <- genCode (Print [e] TVoid)
  -- state@Operands{offS = actO:_} <- get
  -- let
  --   lv = tacVariable $ SymbolInfo "$read" TStr (-1) TempRead actO []
  --   rv = tacConstant ("'R'", TChar)
  
  -- return (msg ++ [T.TACC T.Assign lv rv Nothing], lv)


-- Hace prologo
genFuncCall :: Subroutine -> TACMonad TACOP
genFuncCall (Call f params) = do
  pushSubroutine f False
  genParams (map fst params)
  Operands{base = actO} <- get
  lv <- newTemp actO
  -- Prologo antes de pasar el poder al proc
  tell (tacCall lv f $ length params)
  return lv


-- Cuando se hace new de un tipo, para apuntadores. Reservar espacio para ese tipo
-- devolver temporal que es un apuntador a ese tipo?
genType :: Type -> TACMonad TACOP
genType t = return Nothing
  -- state@Operands{temps = ts, offS = os@(actO:_), astST = st} <- get
  -- let
    -- tInfo = head . fromJust $ lookupInSymTab (show t) st
    -- temp  = "$t" ++ show (M.size ts)
    -- newO  = (fst actO, snd actO + getWidth tInfo t)
    -- lv    = tacVariable $ SymbolInfo temp t (-1) TempReg actO []
    -- rv    = tacConstant (show t, t)
  
  -- put state{temps = M.insert temp True ts, offS = newO:os}
  -- return ([], rv)


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--- Auxiliares que deben ir en este archivo


newTemp :: OffSet -> TACMonad TACOP
newTemp actO = do
  state@Operands{temps = ts} <- get
  let t = Temp (show $ M.size ts - 3) actO
  put state{temps = M.insert t True ts}
  return $ tacVariable t


-- Tal vez colocar labs como [String]
newLabel :: TACMonad TACOP
newLabel = do
  state@Operands{labs = ls} <- get
  let newL = length ls
  put state{labs = newL:ls}
  return $ tacLabel $ show newL


pushOffset :: Int -> TACMonad OffSet
pushOffset width = do
  state@Operands{base = actO} <- get
  let newO = actO + width
  put state{base = newO}
  return actO

resetOffset :: TACMonad ()
resetOffset = do
  state <- get
  put state{base = 0}


pushLiteral :: Literal -> TACOP -> TACMonad ()
pushLiteral l operand = do
  state@Operands{lits = ls} <- get
  put state{lits = M.insert l operand ls}


-- NOTA: si se guarda en un temporal no se accede a memoria. 
pushVariable :: Var -> Type -> TACMonad TACOP
pushVariable var tVar = do
  Operands{vars = vs} <- get
  if M.member var vs then return $ fromJust $ M.lookup var vs -- Aumentar las veces que esta siendo usada (TACOP, Int <veces usada>)
  else do
    actO <- pushOffset (getWidth tVar)
    temp <- newTemp actO
    state@Operands{vars = vs, astST = st} <- get
    put state{vars = M.insert var temp vs}
    let info = head . fromJust $ lookupInSymTab (getName var) st
    -- return $ tacVariable $ TACVar info actO
    return temp


pushSubroutine :: Id -> Bool -> TACMonad ()
pushSubroutine s isProc = do
  state@Operands{subs = subroutines, astST = st} <- get
  let ast = getAST . extraInfo . head . fromJust $ lookupInSymTab s st
  put state{subs = (s,ast,isProc):subroutines}


forComparison :: Id -> Expr -> Expr -> TACOP -> TACMonad (TACOP,TACOP,TACOP)
forComparison n e1 e2 nextL = do
  begin   <- newLabel
  cont    <- continue
  iterVar <- genExpr (Variable (Var n TInt) TInt)
  e1Temp  <- genExpr e1
  tell (tacAssign iterVar e1Temp)
  e2Temp  <- genExpr e2
  tell (tacNewLabel begin)
  genComparison iterVar e2Temp fall nextL LessEq
  return (begin,cont,iterVar)


forInstrs :: InstrSeq -> TACOP -> (TACOP,TACOP,TACOP) -> TACMonad ()
forInstrs is nextL (begin,cont,iterVar) = do
  mapM_ genCode is
  tell (tacNewLabel cont)
  iterVarIncr <- genBinOp Add TInt iterVar (tacConstant ("1",TInt))
  tell (tacAssign iterVar iterVarIncr)
  tell (tacGoto begin)
  tell (tacNewLabel nextL)


-------------------------------------------------------------------------------
continue :: TACMonad TACOP
continue = do
  cont  <- newLabel
  state <- get
  put state{contL = cont}
  return cont
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
breakI :: TACMonad TACOP
breakI = do
  brk   <- newLabel
  state <- get
  put state{brkL = brk}
  return brk
-------------------------------------------------------------------------------
