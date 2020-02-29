{- |
 * Three address code
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.TAC where

import Control.Monad.IO.Class  (liftIO)
import Control.Monad           (when,unless)
import Control.Monad.Trans.RWS (ask,tell,get,put)
import Data.List.Split         (splitOn)
import Data.Maybe              (fromJust,isNothing)
import Playit.AuxFuncs
import Playit.SymbolTable
import Playit.Types
import qualified Data.Map as M
import qualified Playit.TACType as T


--
tacInitState :: SymTab -> Operands
tacInitState = Operands M.empty temps M.empty [] brk cont [0]
  where
    printReg = Temp "_print" (-1)
    readReg  = Temp "_read" (-1)
    nullReg  = Temp "_null" (-1)
    cont     = tacLabel "cont"
    brk      = tacLabel "brk"
    temps    = M.fromList [(printReg,False), (readReg,False), (nullReg,False)]


-- 
gen :: Instr -> TACMonad ()
gen i = case i of
  (Assig v e _)              -> newLabel >>= genAssig v e
  (Assigs is _)              -> mapM_ gen is
  (Break _)                  -> genBreak
  (Continue _)               -> genContinue
  (For n e1 e2 is _)         -> breakI >>= genFor n e1 e2 is
  (ForEach n e is _)         -> return () -- newLabel >>= genForEach n e is
  (ForWhile n e1 e2 e3 is _) -> return () -- newLabel >>= genForWhile n e1 e2 e3 is
  (IF gs _)                  -> return () -- genIF gs >>= backpatch
  (Program is _)             -> mapM_ gen is
  (While e is _)             -> breakI >>= genWhile e is
  (Print es _)               -> return () -- genPrint es
  (Free id _)                -> return () -- genFree id
  (ProcCall s _)             -> return () -- genProcCall s
  (Return e _)               -> return () -- genReturn e


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                            TAC Instructions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- 
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
  TNew n -> return ()
--
  _ -> do
    eTemp <- genExpr e
    vTemp <- genVar var (typeVar var)
    {- 
      if var.scope == 0 then lo que hice
      else
        base/fp[var.offset] := e
    -}
    tell $ tacAssign vTemp eTemp


-- 
genFor :: Id -> Expr -> Expr -> InstrSeq -> TACOP -> TACMonad ()
genFor n e1 e2 is nextL = do
  begin   <- newLabel
  cont    <- continue
  iterVar <- genExpr (Variable (Var n TInt) TInt)
  e1Temp  <- genExpr e1
  tell (tacAssign iterVar e1Temp)
  e2Temp  <- genExpr e2
  tell (tacNewLabel begin)
  genComparison iterVar e2Temp nextL fall GreaterEq
  mapM_ gen is
  tell (tacNewLabel cont)
  iterVarIncr <- genBinOp Add TInt iterVar (tacConstant ("1",TInt))
  tell (tacAssign iterVar iterVarIncr)
  tell (tacGoto begin)
  tell (tacNewLabel nextL)


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
  --   var = gen (Assig (Var n (typeE e)) e TVoid)


-- 
{- TODO: si e3Code usa la var de iteracion para calcular la condicion, hay 2 opciones:
  1 : Cambiar antes esa var por el registro donde se guardo y el que se actualiza
  2 : Cambiar la variable en si
-}
-- genForWhile :: Id -> Expr -> Expr -> Expr -> InstrSeq -> TACMonad ()
-- genForWhile n e1 e2 e3 is = do
  -- (e1Code,e1Temp) <- genExpr e1
  -- (e2Code,e2Temp) <- genExpr e2
  -- (e3Code,e3Temp) <- genExpr e3
  -- contI           <- continue
  -- brkI            <- breakI
  -- state@Operands{temps = ts, labs = ls, contL = cont, brkL = brk, astST = st} <- get
  -- let
  --   ctr  = tacLabel cont
  --   out  = tacLabel brk
  --   t    = "$t" ++ show (M.size ts)
  --   v    = head $ fromJust $ lookupInSymTab n st
  --   min  = tacVariable $ v (offSet v)
  --   iter = tacVariable $ Temp t (offSet v)

  -- put state{temps = ts, labs = brk:ls}
  -- stmts <- foldl concatTAC (return []) (map gen is)

  -- return $ e1Code ++ T.TACC T.Assign min e1Temp Nothing:e2Code ++
  --     T.TACC T.Assign iter min Nothing: contI ++ [
  --     T.TACC T.Sub iter iter e2Temp,
  --     T.TACC T.Gte iter (Just (T.Constant ("0", TInt))) out
  --   ] ++ e3Code ++ T.TACC T.If Nothing e3Temp out:stmts ++ [
  --     T.TACC T.Add iter iter (Just (T.Constant ("1", TInt))),
  --     tacGoTo ctr
  --   ] ++ brkI


-- 
-- genIF :: [(Expr, InstrSeq)] -> TACMonad ()
-- genIF [] = return []
-- genIF [(e, i)] = do
  -- (eCode,eTemp) <- genExpr e
  -- iCode         <- foldl concatTAC (return []) (map gen i)
  -- state@Operands{labs = ls, astST = st} <- get
  -- let
  --   lsn  = length ls
  --   lT = tacLabel lsn
  --   lnext = tacLabel $ lsn + 1
  
  -- case e of
  --   -- if (true) {s} || else {s} ==>> s
  --   (Literal (Boolean True) TBool) -> do
  --     put state{labs = lsn:ls}
  --     return $ iCode ++ [tacGoTo Nothing]
  --   -- if (false) {Don't care}
  --   (Literal (Boolean False) TBool) -> return []
  --   _ -> do
  --     put state{labs = lsn + 1:lsn:ls}
      
  --     return $ eCode ++ 
  --       [
  --         T.TACC T.If Nothing eTemp lT,
  --         tacGoTo lnext,
  --         T.TACC T.NewLabel lT Nothing Nothing
  --       ] ++ iCode ++
  --       [
  --         tacGoTo Nothing,
  --         T.TACC T.NewLabel lnext Nothing Nothing
  --       ]
-- genIF ((e, i):gs) = concatTAC (concatTAC (genIF [(e, i)]) (genIF gs)) $ return [tacGoTo Nothing]


-- 
genWhile :: Expr -> InstrSeq -> TACOP -> TACMonad ()
genWhile e is nextL = do
  begin <- continue
  tell (tacNewLabel begin)
  genBoolExpr e fall nextL
  mapM_ gen is
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
-- genFree :: Id -> TACMonad ()
-- genFree varId = do
  -- Operands{vars = vs, astST = st} <- get
  -- let
  --   varT = symType . head . fromJust $ lookupInSymTab varId st
  --   var  = Var varId varT
  --   addr = fromJust $ M.lookup var vs
  -- return [T.TACC T.Free Nothing addr Nothing]


-------------------------------------------------------------------------------
-- 
-- genProcCall :: Subroutine -> TACMonad ()
-- genProcCall (Call s params) = do
  -- state@Operands{astST = st, labs = ls} <- get
  -- paramsCode <- genParams params 
  -- let
  --   pInfo  = head . fromJust $ lookupInSymTab s st
  --   instrs = getAST $ extraInfo pInfo
  --   proc   = tacVariable pInfo
  --   args   = tacConstant (show $ length paramsCode, TInt)
  --   ret    = [T.TACC T.Return Nothing Nothing Nothing]
  --   begin  = [T.TACC T.NewLabel (tacLabel $ length ls) Nothing Nothing]
  -- liftIO $ print params
  -- stmts <- foldl concatTAC (return []) (map gen instrs)
  -- put state{labs = length ls : ls}

  -- return $ paramsCode ++ [T.TACC T.Call Nothing proc args] ++ begin ++ stmts ++ ret

-- 
-- genParams :: Params -> TACMonad ()
-- genParams []             = return []
-- genParams ((e,_):params) = do
  -- (eCode,eTemp) <- genExpr e
  -- paramsCode    <- genParams params
  -- return $ eCode ++
  --   if isNothing eTemp then paramsCode
  --   else T.TACC T.Param Nothing eTemp Nothing : paramsCode
-------------------------------------------------------------------------------


-- 
-- genReturn :: Expr -> TACMonad ()
-- genReturn e = do
  -- eTemp <- genExpr e
  -- tell [T.TACC T.Return Nothing eTemp Nothing] -- ++ goto begin(continue) ++ label end(break)


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
  -- ArrayList es t      -> genArrayList es t
  -- Null                -> genNull
  -- Read e _            -> genRead e
  -- FuncCall s _        -> genFuncCall s
  -- IdType t            -> genType t
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
-}
genLiteral :: Literal -> Type -> TACMonad TACOP
genLiteral l typeL = do
  -- actO <- pushOffset (getWidth typeL)
  -- lv   <- newTemp actO
  -- pushLiteral l lv
  -- let
  
  case l of
    {-
      ArrLst elems -> do
        let
          len   = length elems - 1
          tArr  = typeArrLst typeL
          tArrW = getWidth tInfo tArr
          tInfo = head . fromJust $ lookupInSymTab (show tArr) st
          osi   = zip (replicate (len + 1) (fst actO)) [(snd actO)..(len * tArrW)]
          newOS = (fst actO, (len + 1) * tArrW) : os
          lvi o = tacVariable $ SymbolInfo t tArr (-1) TempReg o []
          lit x = elems !! x
          rvi x = tacConstant (show (lit x), tArr)
          idx i = tacConstant (show i, tArr)
          arrL  = [T.TACC T.Set (lvi o) (idx x) (rvi x) | (x,o) <- zip [0..len] osi]
        
        put state{temps = newTS, lits = newLS, offS = newOS}
        return (arrL, lv)
      Str s -> do
        let
          len   = length s - 1
          strW  = getWidth tInfo TChar
          tInfo = head . fromJust $ lookupInSymTab (show TChar) st
          osi   = zip (replicate (len + 1) (fst actO)) [(snd actO)..len]
          newOS = (fst actO, len + 1) : os
          lvi o = tacVariable $ SymbolInfo t TChar (-1) TempReg o []
          ch  x = s !! x
          rvi x = tacConstant (show (ch x), TChar)
          idx i = tacConstant (show i, TChar)
          str   = [T.TACC T.Set (lvi o) (idx x) (rvi x) | (x,o) <- zip [0..len] osi]
        
        put state{temps = newTS, lits = newLS, offS = newOS}
        return (str, lv)
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
-}
genVar :: Var -> Type -> TACMonad TACOP
genVar var tVar = do
  -- actO <- pushOffset (getWidth tVar)
  -- lv   <- newTemp actO
  tacVar  <- pushVariable var tVar
  
  -- let
    -- refVS = M.insert (getRefVar var) lv vs -- var o *var?
    -- deref = ([T.TACC T.Deref lv rv Nothing], lv)
  
  {- 
    if var.scope == 0 then lo que hice
    else
      adrr := base/fp[var.offset]
    
    acceso a campos
    if var.scope == 0 then adrr := var[field.offset]
    else
      adrr := base/fp[var.offset + field.offset]
  -}
  case var of
    -- Desref _ t  -> tell (deref lv rv) >> return lv
    -- Index _ e _ -> do
    --   (eCode,eTemp) <- genExpr e
    --   put state{vars = newVS, offS = newOS}
    --   return (eCode ++ [T.TACC T.Get lv rv eTemp], lv)
    -- Param n t ref -> -- no llega aaqui. T.T ==>> Asociar la Var que se le pasa como Parametro
    --   if ref == Value then return ([T.TACC T.Param Nothing rv Nothing], Nothing)
    --   else
    --     put state{vars = newVS} >> return ([T.TACC T.Ref lv rv Nothing], lv)
    -- -- Field v f t   -> 
    _     -> return tacVar


-- 
-- ISSUE: fromJust Nothing con New para Registros
genUnOp :: UnOp -> Expr -> Type -> TACMonad TACOP
genUnOp op e tOp = do
  rv   <- genExpr e
  actO <- pushOffset (getWidth tOp)
  lv   <- newTemp actO
  
  case op of
    Length   -> tell (tacLen lv rv)   >> return lv
    Negative -> tell (tacMinus lv rv) >> return lv
    New      -> tell (tacNew lv rv)   >> return lv
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


-- Tratarlo como un if que asigna cosas
{- 
  if cond then assig trueVal
  else assig falseVal

  Revisar bien labels y offsets
-}
genTerOp :: Expr -> Expr -> Expr -> Type -> TACMonad TACOP
genTerOp eB eT eF tOp = do
  actO   <- pushOffset (getWidth tOp)
  actO'  <- pushOffset 1
  lv     <- newTemp actO
  eBTemp <- newTemp actO'
  next   <- newLabel -- next
  trueL  <- newLabel -- fall
  falseL <- newLabel -- next
  genBoolExpr eB trueL falseL
  tell (tacNewLabel trueL)
  tell (tacAssign eBTemp $ tacConstant ("True",TBool))
  tell (tacGoto next)
  tell (tacNewLabel falseL)
  tell (tacAssign eBTemp $ tacConstant ("False",TBool))
  tell (tacNewLabel next)
  eTTemp <- genExpr eT
  eFTemp <- genExpr eF
  tell (tacIf eBTemp trueL)
  tell (tacAssign lv eFTemp)
  tell (tacGoto falseL)
  tell (tacNewLabel trueL)
  tell (tacAssign lv eTTemp)
  tell (tacNewLabel falseL)
  return lv

  -- if tOp == TBool then do
  --   actO <- pushOffset (getWidth tOp)
  --   lv   <- newTemp actO

  -- (eBCode,eBTemp) <- genExpr eB
  -- (eTCode,eTTemp) <- genExpr eT
  -- (eFCode,eFTemp) <- genExpr eF
  -- state@Operands{temps = ts, labs = ls, offS = os@(actO:_), astST = st} <- get
  -- let
  --   lT    = tacLabel lsn
  --   lF    = tacLabel $ lsn + 1
  --   lvt   = tacVariable $ SymbolInfo t tOp (-1) TempReg actO []
  --   [
  --     T.TACC T.If Nothing eBTemp lT,
  --     T.TACC T.Assign lvt eFTemp Nothing,
  --     tacGoTo lF,
  --     T.TACC T.NewLabel lT Nothing Nothing,
  --     T.TACC T.Assign lvt eTTemp Nothing,
  --     T.TACC T.NewLabel lF Nothing Nothing
  --   ]


-- TODO: Offset y que se genere bien
-- genArrayList :: [Expr] -> Type -> MTACExpr
-- genArrayList elems tE = do
  -- -- elemsCode <- foldl concatTAC (return []) $ map unWrapExprCode elems
  -- state@Operands{temps = ts, offS = os@(actO:prevO:_), astST = st} <- get
  -- let
  --   t   x = "$t" ++ show x
  --   lvt   = tacVariable $ SymbolInfo (t (M.size ts)) tArr (-1) TempReg actO []
  --   len   = length elems - 1
  --   tArr  = typeArrLst tE
  --   tArrW = getWidth tInfo tArr
  --   tInfo = head . fromJust $ lookupInSymTab (show tArr) st
  --   osi   = zip (replicate (len + 1) (fst actO)) (map (* tArrW) [(snd prevO)..])
  --   newOS = (fst actO, (len + 1) * tArrW) : os
  --   lvi x o = tacVariable $ SymbolInfo (t x) tArr (-1) TempReg o []
  --   lit x = elems !! x
  --   rvi x = tacConstant (show (lit x), tArr)
  --   arrL  = [T.TACC T.Assign (lvi x o) (rvi x) Nothing | (x,o) <- zip [(M.size ts)..] osi]
  --   newTS x = M.insert (t x) True ts
  
  -- put state{temps = newTS (M.size ts), offS = newOS}

  -- if null elems then return ([], lvt)
  -- else return (arrL, lvt)


-- 
-- genRead :: Expr -> MTACExpr
-- genRead e = do
  -- msg <- gen (Print [e] TVoid)
  -- state@Operands{offS = actO:_} <- get
  -- let
  --   lv = tacVariable $ SymbolInfo "$read" TStr (-1) TempRead actO []
  --   rv = tacConstant ("'R'", TChar)
  
  -- return (msg ++ [T.TACC T.Assign lv rv Nothing], lv)


-- 
-- genFuncCall :: Subroutine -> MTACExpr
-- genFuncCall (Call f params) = do
  -- Operands{astST = st} <- get
  -- let
  --   fInfo  = head . fromJust $ lookupInSymTab f st
  --   instrs = getAST $ extraInfo fInfo

  -- paramsCode <- genParams params
  -- stmts      <- foldl concatTAC (return []) (map gen instrs)
  -- state@Operands{temps = ts, labs = ls, offS = os@(actO:_)} <- get
  -- let
  --   func   = tacVariable fInfo
  --   args   = tacConstant (show $ length paramsCode, TInt)
  --   temp   = "$t" ++ show (M.size ts)
  --   typeF  = symType fInfo
  --   newO   = (fst actO, snd actO + getWidth fInfo typeF)
  --   lv     = tacVariable $ SymbolInfo temp typeF (-1) TempReg actO []
  --   begin  = [T.TACC T.NewLabel (tacLabel $ length ls) Nothing Nothing]

  -- put state{temps = M.insert temp True ts, labs = length ls : ls, offS = newO:os}
  -- return (paramsCode ++ [T.TACC T.Call lv func args] ++ begin ++ stmts, lv)


-- TODO
-- genType :: Type -> MTACExpr
-- genType t = do
  -- -- state@Operands{temps = ts, offS = os@(actO:_), astST = st} <- get
  -- let
  --   -- tInfo = head . fromJust $ lookupInSymTab (show t) st
  --   -- temp  = "$t" ++ show (M.size ts)
  --   -- newO  = (fst actO, snd actO + getWidth tInfo t)
  --   -- lv    = tacVariable $ SymbolInfo temp t (-1) TempReg actO []
  --   rv    = tacConstant (show t, t)
  
  -- -- put state{temps = M.insert temp True ts, offS = newO:os}
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
  state@Operands{offS = os@(actO:_)} <- get
  let newO = actO + width
  put state{offS = newO:os}
  return actO


pushLiteral :: Literal -> TACOP -> TACMonad ()
pushLiteral l operand = do
  state@Operands{lits = ls} <- get
  put state{lits = M.insert l operand ls}


-- NOTA: si se guarda en un temporal no se accede a memoria. 
pushVariable :: Var -> Type -> TACMonad TACOP
pushVariable var tVar = do
  Operands{vars = vs} <- get
  if M.member var vs then return $ fromJust $ M.lookup var vs
  else do
    actO <- pushOffset (getWidth tVar)
    temp <- newTemp actO
    state@Operands{vars = vs, astST = st} <- get
    put state{vars = M.insert var temp vs}
    let info = head . fromJust $ lookupInSymTab (getName var) st
    -- return $ tacVariable $ TACVar info actO
    return temp


-------------------------------------------------------------------------------
-- Backpatch for goto's in guards
-- backpatch :: [TAC] -> TACMonad ()
-- backpatch tac = do
  -- state@Operands{labs = ls, brkL = brk, contL = cont} <- get
  -- let
  --   lsn    = length ls
  --   lout   = getLout $ last tac
  --   l      = getLabel lout
  --   lout'  = if isNothing lout then tacLabel lsn else lout
  --   replac = tacGoTo lout'
  --   target = tacGoTo Nothing

  -- put state{labs = lsn : ls}
  -- return [(\x -> if x == target then replac else x) x | x <- tac]
-------------------------------------------------------------------------------


-- iterTemps :: TACMonad (TACOP,TACOP)
-- iterTemps = do
--   begin <- newLabel
--   return cont


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


-------------------------------------------------------------------------------
-- unWrapExprCode :: Expr -> TACMonad ()
-- unWrapExprCode e = do
  -- (eCode,_) <- genExpr e
  -- return eCode
-------------------------------------------------------------------------------
