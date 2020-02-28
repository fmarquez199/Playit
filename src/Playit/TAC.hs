{- |
 * Three address code
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.TAC where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.RWS
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing)
import Playit.AuxFuncs
import Playit.SymbolTable
import Playit.Types
import qualified Data.Map as M
import qualified Playit.TACType as T


--
tacInitState :: SymTab -> Operands
tacInitState = Operands M.empty temps M.empty [] (-1) (-1) [0]
  where
    printReg = Temp "_print" (-1)
    readReg  = Temp "_read" (-1)
    nullReg  = Temp "_null" (-1)
    temps    = M.fromList [(printReg,False), (readReg,False), (nullReg,False)]


-- 
gen :: Instr -> TACMonad ()
gen i = case i of
  (Assig v e _)              -> genAssig v e
  (Assigs is _)              -> mapM_ gen is
  (Break _)                  -> return () -- genBreak
  (Continue _)               -> return () -- genContinue
  (For n e1 e2 is _)         -> return () -- genFor n e1 e2 is
  (ForEach n e is _)         -> return () -- genForEach n e is
  (ForWhile n e1 e2 e3 is _) -> return () -- genForWhile n e1 e2 e3 is
  (IF gs _)                  -> return () -- genIF gs >>= backpatch
  (Program is _)             -> mapM_ gen is
  (While e is _)             -> return () -- genWhile e is
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
genAssig :: Var -> Expr -> TACMonad ()
genAssig var e = do
  eTemp <- genExpr e
  vTemp <- genVar var (typeVar var)

  {- 
    if var.scope == 0 then lo que hice
    else
      base/fp[var.offset] := e
  -}

  tell $ assign vTemp eTemp


-- -- 
-- genFor :: Id -> Expr -> Expr -> InstrSeq -> MTACInstr
-- genFor n e1 e2 is = do
--   (e1Code,e1Temp) <- genExpr e1
--   (e2Code,e2Temp) <- genExpr e2
--   contI           <- continue
--   brkI            <- breakI
--   state@Operands{temps = ts, labs = ls, contL = cont, brkL = brk, astST = st} <- get
--   let
--     ctr  = tacLabel cont
--     out  = tacLabel brk
--     t    = "$t" ++ show (M.size ts)
--     v    = head $ fromJust $ lookupInSymTab n st
--     min  = tacVariable $ TACVar v (offSet v)
--     iter = tacVariable $ Temp t (offSet v)
  
--   put state{temps = M.insert t True ts, labs = brk:ls}
--   stmts <- foldl concatTAC (return []) (map gen is)

--   return $ e1Code ++ T.TACC T.Assign min e1Temp Nothing:e2Code ++
--       T.TACC T.Assign iter min Nothing: contI ++ [
--       T.TACC T.Sub iter iter e2Temp,
--       T.TACC T.Gte iter (Just (T.Constant ("0", TInt))) out
--     ] ++ stmts ++ [
--       T.TACC T.Add iter iter (Just (T.Constant ("1", TInt))),
--       tacGoTo ctr
--     ] ++ brkI


-- {-- TODO
--   controller var <- arrLst:
--   InstrSeq
--   .~

--   |
--   v

--   var = arrLst
--   controller dummy = 0 -> #var:
--     InstrSeq[var := var[dummy]]
--   .~
-- --}
-- -- 
-- genForEach :: Id -> Expr -> InstrSeq -> MTACInstr
-- genForEach n e is = return [] -- concatTAC var for
--   where
--     var = gen (Assig (Var n (typeE e)) e TVoid)

-- {-     
--   c x = if x == n then n ++ "[dummy]" else x

--   is' = map (\x -> cambiarId x c) is

--   for = gen (For "dummy" (Literal (Integer 0) TInt) (Unary Length e TInt) is' TVoid)

--   cambiarId :: Instr -> (String -> String) -> Instr
--   cambiarId (Assig v e t) f = Assig (cambiarVar v f) e t
--   cambiarId (Assigs is t) f = Assigs (map (\x -> cambiarId x f) is) t
--   cambiarId (For n e1 e2 is t) f = For (f n) e1 e2 (map (\x -> cambiarId x f) is) t
--   cambiarId (ForEach n e is t) f = ForEach (f n) e (map (\x -> cambiarId x f) is) t
--   cambiarId (ForWhile n e1 e2 e3 is t) f = ForWhile (f n) e1 e2 e3 (map (\x -> cambiarId x f) is) t
--   cambiarId (Free n t) f = Free (f n) t
--   cambiarId (IF g t) f = IF [(x, y) | x <- map (\z -> cambiarExp z f) (map fst g), y <- map (\z -> cambiarId z f) (map snd g)] t
--   cambiarId (Print es t) f = Print (map (\z -> cambiarExp z f) es) t
--   cambiarId (Return e t) f = Return (cambiarExp e f) t
--   cambiarId (While e is t) f = While (cambiarExp e f) (map (\x -> cambiarId x f) is) t
--   cambiarId x _ = x 

--   cambiarVar :: Var -> (String -> String) -> Var
--   cambiarVar (Param n t r) f = Param (f n) t r
--   cambiarVar (Desref v t) f = Desref (cambiarVar v f) t
--   cambiarVar (Var n t) f = Var (f n) t
--   cambiarVar (Index v e t) f = Index (cambiarVar v f) (cambiarExp e f) t
--   cambiarVar (Field v n t) f = Field (cambiarVar v f) (f n) t

--   cambiarExp :: Expr -> (String -> String) -> Expr
--   cambiarExp (ArrayList lst t) f = ArrayList (map (\x -> cambiarExp x f) lst) t
--   cambiarExp (IfSimple e1 e2 e3 t) f = IfSimple (cambiarExp e1 f) (cambiarExp e2 f) (cambiarExp e3 f) t
--   cambiarExp (Literal (Register es) t) f = Literal (Register (map (\x -> cambiarExp x f) es)) t
--   cambiarExp (Binary op e1 e2 t) f = Binary op (cambiarExp e1 f) (cambiarExp e2 f) t
--   cambiarExp (Unary op e1 t) f = Unary op (cambiarExp e1 f) t
--   cambiarExp (Read e t) f = Read (cambiarExp e f) t
--   cambiarExp (Variable var t) f = Variable (cambiarVar var f) t
--   cambiarExp x _ = x
-- -}


-- -- 
-- {- TODO: si e3Code usa la var de iteracion para calcular la condicion, hay 2 opciones:
--   1 : Cambiar antes esa var por el registro donde se guardo y el que se actualiza
--   2 : Cambiar la variable en si
-- -}
-- genForWhile :: Id -> Expr -> Expr -> Expr -> InstrSeq -> MTACInstr
-- genForWhile n e1 e2 e3 is = do
--   (e1Code,e1Temp) <- genExpr e1
--   (e2Code,e2Temp) <- genExpr e2
--   (e3Code,e3Temp) <- genExpr e3
--   contI           <- continue
--   brkI            <- breakI
--   state@Operands{temps = ts, labs = ls, contL = cont, brkL = brk, astST = st} <- get
--   let
--     ctr  = tacLabel cont
--     out  = tacLabel brk
--     t    = "$t" ++ show (M.size ts)
--     v    = head $ fromJust $ lookupInSymTab n st
--     min  = tacVariable $ v (offSet v)
--     iter = tacVariable $ Temp t (offSet v)

--   put state{temps = ts, labs = brk:ls}
--   stmts <- foldl concatTAC (return []) (map gen is)

--   return $ e1Code ++ T.TACC T.Assign min e1Temp Nothing:e2Code ++
--       T.TACC T.Assign iter min Nothing: contI ++ [
--       T.TACC T.Sub iter iter e2Temp,
--       T.TACC T.Gte iter (Just (T.Constant ("0", TInt))) out
--     ] ++ e3Code ++ T.TACC T.If Nothing e3Temp out:stmts ++ [
--       T.TACC T.Add iter iter (Just (T.Constant ("1", TInt))),
--       tacGoTo ctr
--     ] ++ brkI


-- -- 
-- genIF :: [(Expr, InstrSeq)] -> MTACInstr
-- genIF [] = return []
-- genIF [(e, i)] = do
--   (eCode,eTemp) <- genExpr e
--   iCode         <- foldl concatTAC (return []) (map gen i)
--   state@Operands{labs = ls, astST = st} <- get
--   let
--     lsn  = length ls
--     lT = tacLabel lsn
--     lnext = tacLabel $ lsn + 1
  
--   case e of
--     -- if (true) {s} || else {s} ==>> s
--     (Literal (Boolean True) TBool) -> do
--       put state{labs = lsn:ls}
--       return $ iCode ++ [tacGoTo Nothing]
--     -- if (false) {Don't care}
--     (Literal (Boolean False) TBool) -> return []
--     _ -> do
--       put state{labs = lsn + 1:lsn:ls}
      
--       return $ eCode ++ 
--         [
--           T.TACC T.If Nothing eTemp lT,
--           tacGoTo lnext,
--           T.TACC T.NewLabel lT Nothing Nothing
--         ] ++ iCode ++
--         [
--           tacGoTo Nothing,
--           T.TACC T.NewLabel lnext Nothing Nothing
--         ]
-- genIF ((e, i):gs) = concatTAC (concatTAC (genIF [(e, i)]) (genIF gs)) $ return [tacGoTo Nothing]


-- -- 
-- genWhile :: Expr -> InstrSeq -> MTACInstr
-- genWhile e is = do
--   (eCode,eTemp) <- genExpr e
--   contI         <- continue
--   brkI          <- breakI
--   stmts         <- foldl concatTAC (return []) (map gen is)
--   state@Operands{contL = cont} <- get
--   return $ contI ++ stmts ++ eCode ++ [T.TACC T.If Nothing eTemp (tacLabel cont)] ++ brkI



-- -- TODO: relacionado con arrays/lists
-- genPrint :: [Expr] -> MTACInstr
-- genPrint es = return []
--   -- es'    <- foldl concatTAC (return []) $ map unWrapExprCode es
--   -- let
--   --   len   = length es - 1
--   --   t'  x = "$print[" ++ x ++ "]"
--   --   l'  x = es !! x 
--   --   lvi x = tacVariable $ SymbolInfo (t' x) TDummy 1 Constants ("print",-1) []
--   --   rvi x = tacConstant (show (l' x), TDummy)

--   -- return $ es' ++ [ T.TACC T.Assign (lvi (show x)) (rvi x) Nothing | x <- [0..len] ]


-- -- 
-- genFree :: Id -> MTACInstr
-- genFree varId = do
--   Operands{vars = vs, astST = st} <- get
--   let
--     varT = symType . head . fromJust $ lookupInSymTab varId st
--     var  = Var varId varT
--     addr = fromJust $ M.lookup var vs
--   return [T.TACC T.Free Nothing addr Nothing]


-- -------------------------------------------------------------------------------
-- -- 
-- -- ISSUE: No hace correcursion
-- genProcCall :: Subroutine -> MTACInstr
-- genProcCall (Call s params) = do
--   state@Operands{astST = st, labs = ls} <- get
--   paramsCode <- genParams params 
--   let
--     pInfo  = head . fromJust $ lookupInSymTab s st
--     instrs = getAST $ extraInfo pInfo
--     proc   = tacVariable pInfo
--     args   = tacConstant (show $ length paramsCode, TInt)
--     ret    = [T.TACC T.Return Nothing Nothing Nothing]
--     begin  = [T.TACC T.NewLabel (tacLabel $ length ls) Nothing Nothing]
--   liftIO $ print params
--   stmts <- foldl concatTAC (return []) (map gen instrs)
--   put state{labs = length ls : ls}

--   return $ paramsCode ++ [T.TACC T.Call Nothing proc args] ++ begin ++ stmts ++ ret

-- -- 
-- genParams :: Params -> MTACInstr
-- genParams []             = return []
-- genParams ((e,_):params) = do
--   (eCode,eTemp) <- genExpr e
--   paramsCode    <- genParams params
--   return $ eCode ++
--     if isNothing eTemp then paramsCode
--     else T.TACC T.Param Nothing eTemp Nothing : paramsCode
-- -------------------------------------------------------------------------------


-- -- 
-- genReturn :: Expr -> MTACInstr
-- genReturn e = do
--   (eCode,eTemp) <- genExpr e
--   return $ eCode ++ [T.TACC T.Return Nothing eTemp Nothing] -- ++ goto begin(continue) ++ label end(break)


-- -- 
-- genContinue :: MTACInstr
-- genContinue = do
--   Operands{contL = continue} <- get
--   return [tacGoTo (tacLabel continue)]


-- -- 
-- genBreak :: MTACInstr
-- genBreak = do
--   Operands{brkL = brk} <- get
--   return [tacGoTo (tacLabel brk)]


-- -------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------
-- --                             TAC Expressions
-- -------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------


genExpr :: Expr -> TACMonad TACOP
genExpr e = case e of
  (Literal l t)         -> genLiteral l t
  (Variable v t)        -> genVar v t
  (Unary u e t)         -> genUnOp u e t
  -- (Binary b e1 e2 t)    -> genBinOp b e1 e2 t
  -- (IfSimple eB eT eF t) -> genTerOp eB eT eF t
  -- (ArrayList es t)      -> genArrayList es t
  -- Null                  -> genNull
  -- (Read e _)            -> genRead e
  -- (FuncCall s _)        -> genFuncCall s
  -- (IdType t)            -> genType t
  _ -> return Nothing


-- -- 
-- genNull :: MTACExpr
-- genNull = do
--   Operands{offS = actO:_} <- get
--   let pointer = tacVariable $ SymbolInfo "$null" TNull (-1) Constants actO []
--   return ([T.TACC T.Assign pointer Nothing Nothing], pointer)


-- | Generates the TAC code for literals
{- TODO:
  EmptyVal -> Usado cuando no se coloca msj en el read
  Register
-}
genLiteral :: Literal -> Type -> TACMonad TACOP
genLiteral l typeL = do
  -- pushOffset (getWidth typeL)
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
    _ -> do
      return $ tacConstant (show l, typeL)


-- 
{- TODO:
  casos bloques anidados que acceden a los ids
  Param Id Type Ref
  Field Var Id Type
-}
genVar :: Var -> Type -> TACMonad TACOP
genVar var tVar = do
  actO <- pushOffset (getWidth tVar)
  lv   <- newTemp actO
  rv   <- pushVariable var lv actO

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
    _     -> return lv


-- 
-- ISSUE: fromJust Nothing con New para Registros
genUnOp :: UnOp -> Expr -> Type -> TACMonad TACOP
genUnOp op e tOp = do
  actO  <- pushOffset (getWidth tOp)
  lv    <- newTemp actO
  rv    <- genExpr e
  l0    <- newLabel
  l1    <- newLabel
  l2    <- newLabel
  let
    c0    = tacConstant ("0", TInt)
    c25   = tacConstant ("25", TInt)
    c32   = tacConstant ("32", TInt)
    check = gte lv c0 l0 ++ goto l2 ++ tacNewLabel l0 ++ sub lv rv c25
    goNew = goto l2 ++ tacNewLabel l1
  
  case op of
    Length    -> tell (len lv rv)    >> return lv
    Negative  -> tell (minus lv rv)  >> return lv
    New       -> tell (new lv rv)    >> return lv
    Not       -> tell (tacNot lv rv) >> return lv
    UpperCase -> do
      tell (sub lv rv $ tacConstant ("97", TInt))
      tell check
      tell (gte lv c0 l0)
      tell goNew
      tell (sub lv rv c32)
      tell (tacNewLabel l2)
      return lv
    LowerCase -> do
      tell (sub lv rv $ tacConstant ("65", TInt))
      tell check
      tell (lte lv c0 l0)
      tell goNew
      tell (add lv rv c32)
      tell (tacNewLabel l2)
      return lv


-- 
-- genBinOp :: BinOp -> Expr -> Expr -> Type -> MTACExpr
-- genBinOp op e1 e2 tOp = do
--   (e1Code,e1Temp) <- genExpr e1
--   (e2Code,e2Temp) <- genExpr e2
--   state@Operands{temps = ts, labs = ls, offS = os@(actO:_), astST = st} <- get
--   let
--     lsn   = length ls
--     t     = "$t" ++ show (M.size ts)
--     lF    = tacLabel lsn
--     rvl   = tacLabel $ lsn + 1
--     tInfo = head . fromJust $ lookupInSymTab (show tOp) st
--     newO  = (fst actO, snd actO + getWidth tInfo tOp)
--     lvt   = tacVariable $ SymbolInfo t tOp (-1) TempReg actO []
--     rv'   = tacVariable $ SymbolInfo t (typeE e1) (-1) TempReg actO []
  
--   put state{temps = M.insert t True ts, labs = lsn + 1:lsn:ls, offS = newO : os}
  
--   case op of
--   -- Aritmethics
--     Add       -> return (e1Code ++ e2Code ++ [T.TACC T.Add lvt e1Temp e2Temp], lvt)
--     DivEntera -> return (e1Code ++ e2Code ++ [T.TACC T.Div lvt e1Temp e2Temp], lvt)
--     Division  -> return (e1Code ++ e2Code ++ [T.TACC T.Div lvt e1Temp e2Temp], lvt)
--     Minus     -> return (e1Code ++ e2Code ++ [T.TACC T.Sub lvt e1Temp e2Temp], lvt)
--     Module    -> return (e1Code ++ e2Code ++ [T.TACC T.Mod lvt e1Temp e2Temp], lvt)
--     Mult      -> return (e1Code ++ e2Code ++ [T.TACC T.Mult lvt e1Temp e2Temp], lvt)
--     _ -> return Nothing
  -- Booleans
--     And       -> return (e1Code ++ e2Code ++
--       [
--         T.TACC T.If Nothing e1Temp lF,
--         T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--         tacGoTo rvl,
--         T.TACC T.NewLabel lF Nothing Nothing,
--         T.TACC T.Assign lvt e2Temp Nothing,
--         T.TACC T.NewLabel rvl Nothing Nothing
--       ], lvt)
--     Or        -> return (e1Code ++ e2Code ++
--       [
--         T.TACC T.If Nothing e1Temp lF,
--         T.TACC T.Assign lvt e2Temp Nothing,
--         tacGoTo rvl,
--         T.TACC T.NewLabel lF Nothing Nothing,
--         T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--         T.TACC T.NewLabel rvl Nothing Nothing
--       ], lvt)
--   -- Comparators
--     Greater   -> return (e1Code ++ e2Code ++
--       if typeE e1 == TInt then
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Gt rv' (tacConstant ("0", TInt)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ]
--       else
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Gt rv' (tacConstant ("0.0", TFloat)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ], lvt)
--     GreaterEq -> return (e1Code ++ e2Code ++
--       if typeE e1 == TInt then
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Gte rv' (tacConstant ("0", TInt)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ]
--       else
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Gte rv' (tacConstant ("0.0", TFloat)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ], lvt)
--     Less      -> return (e1Code ++ e2Code ++
--       if typeE e1 == TInt then
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Lt rv' (tacConstant ("0", TInt)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ]
--       else
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Lt rv' (tacConstant ("0.0", TFloat)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ], lvt)
--     LessEq    -> return (e1Code ++ e2Code ++
--       if typeE e1 == TInt then
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Lte rv' (tacConstant ("0", TInt)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ]
--       else
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Lte rv' (tacConstant ("0.0", TFloat)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ], lvt)
--     Eq        -> return (e1Code ++ e2Code ++
--       if typeE e1 == TInt then
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Eq rv' (tacConstant ("0", TInt)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ]
--       else
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Eq rv' (tacConstant ("0.0", TFloat)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ], lvt)
--     NotEq     -> return (e1Code ++ e2Code ++
--       if typeE e1 == TInt then
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Neq rv' (tacConstant ("0", TInt)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ]
--       else
--         [
--           T.TACC T.Sub rv' e1Temp e2Temp,
--           T.TACC T.Neq rv' (tacConstant ("0.0", TFloat)) lF,
--           T.TACC T.Assign lvt (tacConstant ("False", TBool)) Nothing,
--           tacGoTo rvl,
--           T.TACC T.NewLabel lF Nothing Nothing,
--           T.TACC T.Assign lvt (tacConstant ("True", TBool)) Nothing,
--           T.TACC T.NewLabel rvl Nothing Nothing
--         ], lvt)
--   -- Lists
--     Anexo  -> return (e1Code ++ e2Code ++ [T.TACC T.Anexo lvt e1Temp e2Temp], lvt)
--     Concat -> return (e1Code ++ e2Code ++ [T.TACC T.Concat lvt e1Temp e2Temp], lvt)


-- -- 
-- genTerOp :: Expr -> Expr -> Expr -> Type -> MTACExpr
-- genTerOp eB eT eF tOp = do
--   (eBCode,eBTemp) <- genExpr eB
--   (eTCode,eTTemp) <- genExpr eT
--   (eFCode,eFTemp) <- genExpr eF
--   state@Operands{temps = ts, labs = ls, offS = os@(actO:_), astST = st} <- get
--   let
--     lsn   = length ls
--     t     = "$t" ++ show (M.size ts)
--     lT    = tacLabel lsn
--     lF    = tacLabel $ lsn + 1
--     tInfo = head . fromJust $ lookupInSymTab (show tOp) st
--     newO  = (fst actO, snd actO + getWidth tInfo tOp)
--     lvt   = tacVariable $ SymbolInfo t tOp (-1) TempReg actO []

--   put state{temps = M.insert t True ts, labs = lsn + 1:lsn:ls, offS = newO:os}
--   return (eBCode ++ eTCode ++ eFCode ++
--     [
--       T.TACC T.If Nothing eBTemp lT,
--       T.TACC T.Assign lvt eFTemp Nothing,
--       tacGoTo lF,
--       T.TACC T.NewLabel lT Nothing Nothing,
--       T.TACC T.Assign lvt eTTemp Nothing,
--       T.TACC T.NewLabel lF Nothing Nothing
--     ], lvt)


-- -- TODO: Offset y que se genere bien
-- genArrayList :: [Expr] -> Type -> MTACExpr
-- genArrayList elems tE = do
--   -- elemsCode <- foldl concatTAC (return []) $ map unWrapExprCode elems
--   state@Operands{temps = ts, offS = os@(actO:prevO:_), astST = st} <- get
--   let
--     t   x = "$t" ++ show x
--     lvt   = tacVariable $ SymbolInfo (t (M.size ts)) tArr (-1) TempReg actO []
--     len   = length elems - 1
--     tArr  = typeArrLst tE
--     tArrW = getWidth tInfo tArr
--     tInfo = head . fromJust $ lookupInSymTab (show tArr) st
--     osi   = zip (replicate (len + 1) (fst actO)) (map (* tArrW) [(snd prevO)..])
--     newOS = (fst actO, (len + 1) * tArrW) : os
--     lvi x o = tacVariable $ SymbolInfo (t x) tArr (-1) TempReg o []
--     lit x = elems !! x
--     rvi x = tacConstant (show (lit x), tArr)
--     arrL  = [T.TACC T.Assign (lvi x o) (rvi x) Nothing | (x,o) <- zip [(M.size ts)..] osi]
--     newTS x = M.insert (t x) True ts
  
--   put state{temps = newTS (M.size ts), offS = newOS}

--   if null elems then return ([], lvt)
--   else return (arrL, lvt)


-- -- 
-- genRead :: Expr -> MTACExpr
-- genRead e = do
--   msg <- gen (Print [e] TVoid)
--   state@Operands{offS = actO:_} <- get
--   let
--     lv = tacVariable $ SymbolInfo "$read" TStr (-1) TempRead actO []
--     rv = tacConstant ("'R'", TChar)
  
--   return (msg ++ [T.TACC T.Assign lv rv Nothing], lv)


-- -- ISSUE: No hace correcursion
-- genFuncCall :: Subroutine -> MTACExpr
-- genFuncCall (Call f params) = do
--   Operands{astST = st} <- get
--   let
--     fInfo  = head . fromJust $ lookupInSymTab f st
--     instrs = getAST $ extraInfo fInfo

--   paramsCode <- genParams params
--   stmts      <- foldl concatTAC (return []) (map gen instrs)
--   state@Operands{temps = ts, labs = ls, offS = os@(actO:_)} <- get
--   let
--     func   = tacVariable fInfo
--     args   = tacConstant (show $ length paramsCode, TInt)
--     temp   = "$t" ++ show (M.size ts)
--     typeF  = symType fInfo
--     newO   = (fst actO, snd actO + getWidth fInfo typeF)
--     lv     = tacVariable $ SymbolInfo temp typeF (-1) TempReg actO []
--     begin  = [T.TACC T.NewLabel (tacLabel $ length ls) Nothing Nothing]

--   put state{temps = M.insert temp True ts, labs = length ls : ls, offS = newO:os}
--   return (paramsCode ++ [T.TACC T.Call lv func args] ++ begin ++ stmts, lv)


-- -- TODO
-- genType :: Type -> MTACExpr
-- genType t = do
--   -- state@Operands{temps = ts, offS = os@(actO:_), astST = st} <- get
--   let
--     -- tInfo = head . fromJust $ lookupInSymTab (show t) st
--     -- temp  = "$t" ++ show (M.size ts)
--     -- newO  = (fst actO, snd actO + getWidth tInfo t)
--     -- lv    = tacVariable $ SymbolInfo temp t (-1) TempReg actO []
--     rv    = tacConstant (show t, t)
  
--   -- put state{temps = M.insert temp True ts, offS = newO:os}
--   return ([], rv)


-- -------------------------------------------------------------------------------
-- -------------------------------------------------------------------------------
-- --- Auxiliares que deben ir en este archivo


newTemp :: OffSet -> TACMonad TACOP
newTemp actO = do
  state@Operands{temps = ts} <- get
  let t = Temp (show $ M.size ts - 3) actO
  put state{temps = M.insert t True ts}
  return $ tacVariable t


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
pushVariable :: Var -> TACOP -> OffSet -> TACMonad TACOP
pushVariable var temp actO = do
  state@Operands{vars = vs, astST = st} <- get
  let info = head . fromJust $ lookupInSymTab (getName var) st
  put state{vars = M.insert var temp vs}
  return $ tacVariable $ TACVar info actO


-- -------------------------------------------------------------------------------
-- -- Backpatch for goto's in guards
-- backpatch :: [TAC] -> MTACInstr
-- backpatch tac = do
--   state@Operands{labs = ls, brkL = brk, contL = cont} <- get
--   let
--     lsn    = length ls
--     lout   = getLout $ last tac
--     l      = getLabel lout
--     lout'  = if isNothing lout then tacLabel lsn else lout
--     replac = tacGoTo lout'
--     target = tacGoTo Nothing

--   put state{labs = lsn : ls}
--   return [(\x -> if x == target then replac else x) x | x <- tac]
-- -------------------------------------------------------------------------------


-- -------------------------------------------------------------------------------
-- continue :: MTACInstr
-- continue = do
--   state@Operands{labs = ls} <- get
--   let newL = length ls + 1
--   put state{labs = newL:ls, contL = newL}
--   return [T.TACC T.NewLabel (tacLabel newL) Nothing Nothing]
-- -------------------------------------------------------------------------------


-- -------------------------------------------------------------------------------
-- breakI :: MTACInstr
-- breakI = do
--   state@Operands{labs = ls} <- get
--   let newL = length ls + 1
--   put state{labs = newL:ls, brkL = newL}
--   return [T.TACC T.NewLabel (tacLabel newL) Nothing Nothing]
-- -------------------------------------------------------------------------------


-- -------------------------------------------------------------------------------
-- unWrapExprCode :: Expr -> MTACInstr
-- unWrapExprCode e = do
--   (eCode,_) <- genExpr e
--   return eCode
-- -------------------------------------------------------------------------------
