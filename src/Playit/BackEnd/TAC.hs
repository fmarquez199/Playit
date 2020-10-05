{- |
 * Three address code
 *
 * Copyright : (c) 
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.BackEnd.TAC (tacInitState, genTAC, dataFilePath) where

import Control.Monad.IO.Class      (liftIO)
import Control.Monad               (when, unless, void)
import Control.Monad.Trans.RWS     (ask, tell, get, put)
import Data.List.Utils             (replace)
import Data.Maybe                  (fromJust, isNothing)
import Playit.BackEnd.Utils    
import Playit.BackEnd.Types
import Playit.FrontEnd.SymbolTable (lookupInSymTab)
import Playit.FrontEnd.Types
import Playit.FrontEnd.Utils       (typeVar, baseTypeT, isArrLst, baseTypeE, typeE)
import qualified Data.Map          as M
import qualified TACType           as T


dataFilePath :: String
dataFilePath = "./output/data.asm"

-- Colocar los temps de print, read y null al inicio?
tacInitState :: SymTab -> Operands
tacInitState = Operands M.empty temps M.empty [] brk cont 0 False False []
  where
    retnReg  = Temp "_return" TInt (4, 4)  -- $v0, offset fijo?
    nullReg  = Temp "_null" TInt (4, 4)    -- $zero, offset fijo?
    cont     = tacLabel "cont"
    brk      = tacLabel "brk"
    temps    = M.fromList [(retnReg, False), (nullReg, False)]


genTAC :: Instr -> TACMonad ()
genTAC ast = tell (tacCall Nothing "main" 0 ++ [tacNewLabel (tacLabel "main")]) >>
          genCode ast >> genSubroutines

-- 
genCode :: Instr -> TACMonad ()
genCode i = case i of
  (Program is _)             -> mapM_ genCode is >> tell [tacExit]
  (Assigs is _)              -> mapM_ genCode is
  (Assig v e _)              -> genAssig v e
  (Break _)                  -> genBreak
  (Continue _)               -> genContinue
  (For n e1 e2 is _)         -> breakI   >>= genFor n e1 e2 is
  (ForEach n e is _)         -> breakI   >>= genForEach n e is
  (ForWhile n e1 e2 e3 is _) -> breakI   >>= genForWhile n e1 e2 e3 is
  (IF gs _)                  -> newLabel >>= genIF gs
  (While e is _)             -> breakI   >>= genWhile e is
  (Print es _)               -> genPrint es
  (Free id _)                -> genFree id
  (ProcCall s _)             -> genProcCall s
  (Return e _)               -> genExpr e >>= genReturn


genSubroutines :: TACMonad ()
genSubroutines = do
  state <- get
  mapM_ genSubroutine (subs state)
  when (callM state) (resetOffset >> malloc)
  when (callF state) (resetOffset >> free)


genSubroutine :: (Id, Params, InstrSeq, Bool) -> TACMonad ()
genSubroutine (s, ps, is, isProc) = resetOffset >>
  tell [tacNewLabel $ tacLabel s] >> getParams s ps 0 >> mapM_ genCode is >>
    when isProc (genReturn Nothing)

{-  El problema es que en efecto cuando consulto el parámetro este no ha sido 
apilado en vars, en ese caso lo apilo cuando paso los parámetros y es generando 
la subrutina que tengo que tener cuidado
-}
getParams :: Id -> Params -> Int -> TACMonad ()
getParams _ [] _ = return ()
getParams id ps n = do
  Operands{vars = vs, astST = st} <- get
  let syms = getSymTab st
      subParams = filter isPs $ extraInfo $ head $ fromJust $ M.lookup id syms
      paramsLst = head $ map (\(Params x) -> x) subParams
      (typ, name) = paramsLst !! n
      width = getWidth typ
      var = Var name typ
  formal <- pushOffset width >>= newTemp typ width >>= genVar var
  par <- genExpr $ fst $ head ps
  tell [tacParam' par n formal]
  if length ps > 1 then getParams id (tail ps) (n' typ) else return ()
  where
    isPs :: ExtraInfo -> Bool
    isPs (Params _) = True
    isPs _ = False

    n' :: Type -> Int
    n' typ = if typ == TFloat then n + 2 else n + 1


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                            TAC Instructions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- TODO: Registros/Uniones
genAssig :: Var -> Expr -> TACMonad ()
genAssig v e = case typeVar v of
  -- Si es el operador ternario la expr puede ser bool o no
  TBool -> do
    tmp <- pushOffset (getWidth (typeVar v)) >>= newTemp TBool 1 >>= genVar v
    nextL  <- newLabel
    trueL  <- newLabel
    falseL <- newLabel
    genBoolExpr e trueL falseL
    tell [tacNewLabel trueL]
    tell (tacAssign tmp (tacConstant ("True", TBool)))
    tell (tacGoto nextL)
    tell [tacNewLabel falseL]
    tell (tacAssign tmp (tacConstant ("False", TBool)))
    tell [tacNewLabel nextL]
-- Registros y uniones
  -- TNew n -> do
  --   e<-genExpr e
  --   return ()
--
  TStr -> do
    case e of
      Literal (Str s) _ -> do
        let 
          literal  = show v
          litLabel = literal ++ "_str"
          strLen   = show $ length s
        
        liftIO $ _word ("len_" ++ literal) strLen dataFilePath
        liftIO $ _asciiz litLabel s dataFilePath

        v' <- pushOffset 4 >>= newTemp TInt 4 >>= genVar v
        -- tell tacAssign vTemp eTemp litLabel
        tell [tacRef v' (tacLabel litLabel)]
      
      Variable v' _ -> do
        (rv, offset, width) <- getOffset v'
        lv <- pushOffset offset >>= newTemp TStr width >>= genVar v
        t <- pushOffset 1 >>= newTemp TChar 1
        tell (tacGet t rv (tacConstant ("0", TInt)))
        tell (tacSet lv (tacConstant ("0", TInt)) t)
        copyStr lv rv t 1 (width - 4)

      Read (Literal (Str s) _) _ -> do
        let 
          var     = show v
          varBuffer = var ++ "_str"
          strLabel  = var ++ "_prompt"
        
        liftIO $ _space varBuffer "80" dataFilePath
        liftIO $ _asciiz strLabel s dataFilePath
        v' <- pushOffset 4 >>= newTemp TInt 4 >>= genVar v
        tell [tacPrint (tacConstant (s, TStr)) (tacLabel strLabel)]
        tell [tacRead (tacConstant (var, TStr)) (tacLabel varBuffer)] -- TODO: var o temp?
        tell [tacRef v' (tacLabel varBuffer)]

      Read (Variable v' _) _ -> do
        (rv, _, _) <- getOffset v'
        lv <- pushOffset 4 >>= newTemp TInt 4 >>= genVar v
        let 
          var      = show v
          varLabel = var ++ "_str"
        
        liftIO $ _space varLabel "80" dataFilePath
        tell [tacPrint rv rv] -- TODO: rv -> string a imprimir
        tell [tacRead (tacConstant (var, TStr)) (tacLabel varLabel)]
        tell [tacRef lv (tacLabel varLabel)]
        
      -- TODO!!: 
      -- casos con EmptyVAlue -> read sin str para imprimir
      -- funciones
      t -> error $ "NotImplementedError " ++ show t

-- TODO: No guardar valores numericos en .data, ya se colocan en un temp para no mas accesos a mem
  TInt -> do
    case e of
      Literal (Integer i) _ -> do
        let varBuffer = show v ++ "_int"
        liftIO $ _space varBuffer "4" dataFilePath
        v' <- pushOffset 4 >>= newTemp TInt 4 >>= genVar v
        -- tell (tacAssign v' (tacConstant (show i, TInt)) (tacLabel varBuffer) )
        tell (tacAssign v' (tacConstant (show i, TInt)))
        tell (tacAssign (tacLabel varBuffer) v') -- TODO: quitar para no acceder a memoria
      
      Variable v' _ -> do
        (rv, _, _) <- getOffset v'
        lv <- pushOffset 4 >>= newTemp TInt 4 >>= genVar v
        tell (tacAssign lv rv)
        tell (tacAssign (tacLabel (show v ++ "_int")) rv)
      
      Read (Literal (Str s) _) _ -> do
        let 
          var = show v 
          varBuffer = var ++ "_int"
          intLabel  = var ++ "_prompt"

        liftIO $ _space varBuffer "4" dataFilePath
        liftIO $ _asciiz intLabel s dataFilePath

        v' <- pushOffset 4 >>= newTemp TInt 4 >>= genVar v
        tell [tacPrint (tacConstant (s, TInt)) (tacLabel intLabel)]
        tell [tacRead (tacConstant (var, TInt)) (tacLabel varBuffer)]
        tell [tacDeref v' (tacLabel varBuffer)]
      
      Read (Variable v' _) _ -> do
        (rv, _, _) <- getOffset v'
        let 
          var = show v 
          varBuffer = var ++ "_int"
          -- intLabel  = var ++ "_prompt"

        liftIO $ _space varBuffer "4" dataFilePath
        
        v' <- pushOffset 4 >>= newTemp TInt 4 >>= genVar v
        tell [tacPrint rv rv] -- TODO: rv -> string a imprimir
        tell [tacRead (tacConstant (var, TInt)) (tacLabel varBuffer)]
        tell [tacDeref v' (tacLabel varBuffer)]
        tell (tacAssign (tacLabel varBuffer) v')

      e -> do
        let 
          var = show v 
          varBuffer = var ++ "_int"

        -- liftIO $ _space varBuffer "4" dataFilePath

        t <- genExpr e
        v' <- pushOffset 4 >>= newTemp TInt 4 >>= genVar v
        tell (tacAssign v' t)
        tell (tacAssign (tacLabel varBuffer) t)

  TFloat -> do
    case e of
      Literal (Floatt f) _ -> do
        let varBuffer = show v ++ "_float"
        liftIO $ _double varBuffer (show f) dataFilePath
        v' <- pushOffset 8 >>= newTemp TFloat 8 >>= genVar v
        -- tell (tacAssign v' (tacConstant (show f, TFloat)) (tacLabel varBuffer) )
        -- tell (tacAssign v' (tacConstant (show f, TFloat)) )
        tell (tacAssign v' (tacLabel varBuffer) )
        tell (tacAssign (tacLabel varBuffer) v') -- TODO: quitar para no acceder a memoria
      
      Variable v' _ -> do
        (rv, _, _) <- getOffset v'
        let
          var = show v
          varBuffer = var ++ "_float"

        liftIO $ _space varBuffer "8" dataFilePath
        
        lv <- pushOffset 8 >>= newTemp TFloat 8 >>= genVar v
        tell (tacAssign lv rv)
        tell (tacAssign (tacLabel (show v ++ "_float")) rv)

      Read (Literal (Str s) _) _ -> do
        let 
          var = show v 
          varBuffer = var ++ "_float"
          floatLabel  = var ++ "_prompt"

        liftIO $ _space varBuffer "8" dataFilePath
        liftIO $ _asciiz floatLabel s dataFilePath

        v' <- pushOffset 8 >>= newTemp TFloat 8 >>= genVar v
        tell [tacPrint (tacConstant (s, TFloat)) (tacLabel floatLabel)]
        tell [tacRead (tacConstant (var, TFloat)) (tacLabel varBuffer)]
        tell [tacDeref v' (tacLabel varBuffer)]
      
      Read (Variable v' _) _ -> do
        (rv, _, _) <- getOffset v'
        let 
          var = show v 
          varBuffer = var ++ "_float"

        liftIO $ _space varBuffer "8" dataFilePath

        v' <- pushOffset 8 >>= newTemp TFloat 8 >>= genVar v
        tell [tacPrint rv rv] -- TODO: rv -> string a imprimir
        tell [tacRead (tacConstant (var, TFloat)) (tacLabel varBuffer)]
        tell [tacDeref v' (tacLabel varBuffer)]
        tell (tacAssign (tacLabel varBuffer) v')
      
      e -> do
        let 
          var = show v 
          varBuffer = var ++ "_float"

        liftIO $ _space varBuffer "8" dataFilePath

        t <- genExpr e
        v' <- pushOffset 8 >>= newTemp TFloat 8 >>= genVar v
        tell (tacAssign v' t)
        -- tell (tacAssign (tacLabel var) t)
        tell (tacAssign (tacLabel varBuffer) t)
-- 
  t -> do
    -- isIndexVar var && isIndexExpr e -> do
    -- if isIndexVar var then tell (tacSet vTemp index eTemp)
    -- else   
    eTemp <- genExpr e
    {- 
      if isField var then
      else
        base/fp[var.offset] := e
    -}
    if isLit e then do
      let w = getWidth t
      vTemp <- pushOffset w >>= newTemp (typeE e) w >>= genVar v
      -- pushLiteral eTemp vTemp
      tell (tacAssign vTemp eTemp)
    else
      void (genVar v eTemp)


-- 
genFor :: Id -> Expr -> Expr -> InstrSeq -> TACOP -> TACMonad ()
genFor n e1 e2 is nextL = forComparison n e1 e2 nextL >>= forInstrs is nextL


-- para arrays
genForEach :: Id -> Expr -> InstrSeq -> TACOP -> TACMonad ()
genForEach n e is nextL = do
  let
    t = baseTypeE e
    w = getWidth t
  begin <- newLabel
  contn <- continue
  var   <- pushOffset w >>= newTemp (typeE e) w >>= genVar (Var n t)
  expr  <- genExpr e
  count <- pushOffset 4 >>= newTemp TInt 4 >>= genVar (Var ("$i_" ++ n) t)
  tell (tacUn T.Deref count expr)
  tell [tacNewLabel begin]
  tell (tacBin T.Lte count (tacConstant ("0", TInt)) nextL)
  iterVarShft <- genBinOp Add TInt expr (tacConstant (show w, TInt))
  tell (tacUn T.Deref var iterVarShft)
  mapM_ genCode is
  tell [tacNewLabel contn]
  countIncrmt <- genBinOp Minus TInt count (tacConstant ("1", TInt))
  tell (tacAssign count countIncrmt)
  tell (tacGoto begin)
  tell [tacNewLabel nextL]

-- for con condicion
genForWhile :: Id -> Expr -> Expr -> Expr -> InstrSeq -> TACOP -> TACMonad ()
genForWhile n e1 e2 cond is nextL = do
  iteration <- forComparison n e1 e2 nextL
  genBoolExpr cond fall nextL
  forInstrs is nextL iteration


-- 
genIF :: [(Expr, InstrSeq)] -> TACOP -> TACMonad ()
genIF [] nextL               = tell [tacNewLabel nextL]
genIF ((e, is):guards) nextL = do
  let isLast = null guards
  falseL <- if isLast then return nextL else newLabel
  genBoolExpr e fall falseL
  mapM_ genCode is
  unless isLast $ tell (tacGoto nextL)
  unless isLast $ tell [tacNewLabel falseL]
  when isLast $ return ()
  genIF guards nextL


-- 
genWhile :: Expr -> InstrSeq -> TACOP -> TACMonad ()
genWhile e is nextL = do
  begin <- continue
  tell [tacNewLabel begin]
  genBoolExpr e fall nextL
  mapM_ genCode is
  tell (tacGoto begin)
  tell [tacNewLabel nextL]


-- TODO: width del tipo string
-- TODO: integrar lo que hay en genAssig
genPrint :: [Expr] -> TACMonad ()
genPrint [] = tell []
genPrint [e] = 
  case e of
    Literal (Str s) _ -> 
      -- TODO!!: sustituir los espacios por _ en lugar de poner todo el str junto
      let strLabel = concat (words s) ++ "_str"
      in do
        liftIO $ _asciiz strLabel s dataFilePath
        tell [tacPrint (tacConstant (s, TStr)) (tacLabel strLabel)]
    
    Literal (Integer i) _ -> do
      let 
        lit      = show i
        litLabel = "integer" ++ lit ++"_int"
      
      liftIO $ _word litLabel lit dataFilePath
      tell [tacPrint (tacConstant (lit, TInt)) (tacLabel litLabel)]
    
    Literal (Floatt f) _ -> do
      let
        lit      = show f
        litLabel = "float" ++ lit ++"_float"

      liftIO $ _double litLabel lit dataFilePath
      tell [tacPrint (tacConstant (lit, TFloat)) (tacLabel litLabel)]
    
    Literal (Boolean b) _ ->
      -- "\nboolTrue: .asciiz \"Win\"\n"
      -- appendFile dataFilePath "boolFalse: .asciiz \"Lose\"\n"
      let boolLabel = if b then tacLabel "boolTrue" else tacLabel "boolFalse"
      in tell [tacPrint (tacConstant (show b, TBool)) boolLabel]
    
    Literal (Character c) _ -> do
      let litLabel = "char" ++ c : "_char"
      liftIO $ _asciiz litLabel [c] dataFilePath
      tell [tacPrint (tacConstant (show c, TChar)) (tacLabel litLabel)]
    
    Literal (ArrLst ls) t -> do
      let 
        arr      = show ls
        arrLabel = "array" ++ replace "," "" (init $ tail arr) ++ "_str"
      liftIO $ _asciiz arrLabel arr dataFilePath
      tell [tacPrint (tacConstant (arr, t)) (tacLabel arrLabel)]
    
    Literal _ _ -> tell [] -- Register [Expr]

    Variable v _ -> case typeVar v of
      TInt -> tell [tacPrint (tacConstant (show v, TInt)) (tacLabel (show v ++ "_int"))]
      TFloat -> tell [tacPrint (tacConstant (show v, TFloat)) (tacLabel (show v ++ "_float"))]
      TStr -> tell [tacPrint (tacConstant (show v, TStr)) (tacLabel (show v ++ "_str"))]
      TBool -> do
        var <- pushOffset 1 >>= newTemp TBool 1 >>= genVar v
        nextL  <- newLabel
        trueL  <- newLabel
        falseL <- newLabel
        genBoolExpr e trueL falseL
        tell [tacNewLabel trueL]
        tell [tacPrint var (tacLabel "boolTrue")]
        tell (tacGoto nextL)
        tell [tacNewLabel falseL]
        tell [tacPrint var (tacLabel "boolFalse")]
        tell [tacNewLabel nextL]

    _ -> tell []
genPrint (e:es) = genPrint [e] >> genPrint es


-- Aqui se llama a free, Prologo
genFree :: Id -> TACMonad ()
genFree varId = do
  state@Operands{vars = vs, astST = st} <- get
  let -- TODO: Si se cambia por 'free Expr' no tendria que buscar en la symtab
    varT = symType . head . fromJust $ lookupInSymTab varId st
    var  = fromJust $ M.lookup (Var varId varT) vs
  tell [tacParam var 0]
  tell (tacCall Nothing "free" 1)
  -- prólogo
  put state{callF = True}


-- Hace prologo
genProcCall :: Subroutine -> TACMonad ()
genProcCall (Call s params) = do
  pushSubroutine s params True
  genParams (map fst params) 0
  -- Prologo antes de pasar el poder al proc
  tell (tacCall Nothing s $ length params)


-- Hace epilogo
genReturn :: TACOP -> TACMonad ()
genReturn e = tell [T.ThreeAddressCode T.Return Nothing e Nothing]


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
  Variable v t        -> do
    vs <- vars <$> get
    if M.member v vs then return $ fromJust $ M.lookup v vs -- Aumentar las veces que esta siendo usada (TACOP, Int <veces usada>)
    else pushOffset (getWidth t) >>= newTemp t (getWidth t) >>= genVar v
  Unary u e t         -> genUnOp u e t
  Binary b e1 e2 t    -> do
    e1Temp <- genExpr e1
    e2Temp <- genExpr e2
    genBinOp b t e1Temp e2Temp
  IfSimple eB eT eF t -> genTerOp eB eT eF t
  ArrayList es t      ->
    let width = getWidth t {- (baseTypeT t) -} -- TODO!!: Verificar
    in pushOffset width >>= newTemp t width >>= genArrayList es width 0
  Null                -> genNull
  Read e _            -> genPrint [e] >> genRead
  FuncCall s t        -> genFuncCall s t
  IdType t            -> genType t


-- Array de bools, apuntador a bool
genBoolExpr :: Expr -> TACOP -> TACOP -> TACMonad ()
genBoolExpr e trueL falseL =  
  case e of
    Literal (Boolean True) _  -> unless (isFall trueL) $ tell (tacGoto trueL)
    Literal (Boolean False) _ -> unless (isFall falseL) $ tell (tacGoto falseL)
    Unary Not e _             -> genBoolExpr e falseL trueL
  -- Variables
  -- Comparators
    Binary op e1 e2 _ | op `elem` [Greater, GreaterEq, Less, LessEq, Eq, NotEq] -> do
      leftExpr  <- genExpr e1
      rightExpr <- genExpr e2
      genComparison leftExpr rightExpr trueL falseL op
  -- Conjunction and disjunction
    Binary op e1 e2 _ | op `elem` [And, Or] -> do
      e1TrueL <- -- for `or` we need to generate a new `true` label if the current is `fall`
        if op == Or then if isFall trueL then newLabel else return trueL
        else return fall
      e1FalseL <-
        if op == And then if isFall falseL then newLabel else return falseL
        else return fall
      
      genBoolExpr e1 e1TrueL e1FalseL
      genBoolExpr e2 trueL falseL
      if op == And then
        when (isFall falseL) $ tell [tacNewLabel e1FalseL]
      else
        when (isFall trueL) $ tell [tacNewLabel e1TrueL]
  -- 
    FuncCall s t -> do
      let 
        isTrueNotFall  = not $ isFall trueL
        isFalseNotFall = not $ isFall falseL
        
      cond <- genFuncCall s t
      if isTrueNotFall && isFalseNotFall then do
        tell (tacBin T.Eq cond (tacConstant ("Win", TBool)) trueL)
        tell (tacGoto falseL)
      else if isTrueNotFall then
        tell (tacBin T.Eq cond (tacConstant ("Win", TBool)) trueL)
      else when isFalseNotFall $
        tell (tacBin T.Eq cond (tacConstant ("Lose", TBool)) falseL)
  -- 
    Variable var _ -> do
      let 
        isTrueNotFall  = not $ isFall trueL
        isFalseNotFall = not $ isFall falseL
        
      cond <- pushOffset 1 >>= newTemp TBool 1 >>= genVar var
      if isTrueNotFall && isFalseNotFall then do
        tell (tacBin T.Eq cond (tacConstant ("Win", TBool)) trueL)
        tell (tacGoto falseL)
      else if isTrueNotFall then
        tell (tacBin T.Eq cond (tacConstant ("Win", TBool)) trueL)
      else when isFalseNotFall $
        tell (tacBin T.Eq cond (tacConstant ("Lose", TBool)) falseL)

    e -> error $ "\n\tUnexpected boolean expression:  " ++ show e ++ "\n"


genComparison :: TACOP -> TACOP -> TACOP -> TACOP -> BinOp -> TACMonad ()
genComparison leftExpr rightExpr trueL falseL op = do
  let
    trueNotFall  = not $ isFall trueL
    falseNotFall = not $ isFall falseL
  
  if trueNotFall && falseNotFall then
    tell (tacBin (binOpToTACOP op) leftExpr rightExpr trueL) >> tell (tacGoto falseL)
  else
    if trueNotFall then tell (tacBin (binOpToTACOP op) leftExpr rightExpr trueL)
    else
      when falseNotFall $
        tell (tacBin (negation $ binOpToTACOP op) leftExpr rightExpr falseL)


-- 
genNull :: TACMonad TACOP
genNull = return $ tacVariable $ Temp "_null" TInt (4, 4)


-- | Generates the TAC code for literals
{- TODO:
  EmptyVal -> Usado cuando no se coloca msj en el read
  ArrLst -> caso que llega hasta aqui?
  Register
  Union
  
  integrar lo que hay en genAssig
  TODO!!: colocarlo en el .data, si es un array colocar su tam
-}
-- import Text.Regex.Posix ((=~))
genLiteral :: Literal -> Type -> {- String -> -} TACMonad TACOP
genLiteral l typeL {- _dataLabel -} =
  -- case l of
    {-
      ArrLst elems -> -- No llega aqui
        liftIO (print ("Llegue a literal ArrLst: " ++ show elems)) >> return (tacLabel "lit arrLst")
    -}
    -- EmptyVal -> 
    -- Register es -> -- actualizar el valor de cada campo
    -- _ ->
  -- if l of
  --   Str s -> liftIO $ _word ("len_" ++ literal) strLen >> _asciiz litLabel s
  --   return 
  -- else 
    return $ tacConstant (show l, typeL)


-- 
{- TODO:
  casos bloques anidados que acceden a los ids
  Param Id Type Ref
  Field Var Id Type
  Index  Var Expr Type
-}
genVar :: Var -> TACOP -> TACMonad TACOP
genVar var temp =
  case var of
    Param _ _ Reference -> do -- TODO: No llega aqui
      liftIO (print ("Llegue a genVar param por ref: " ++ show var))
      tacVar <- pushVariable var temp
      tell (tacUn T.Deref temp tacVar) >> return temp
    Desref _ t -> do
      tacVar <- pushVariable (getRefVar var) temp
      tell (tacUn T.Deref temp tacVar) >> return temp
    -- Field v f t -> return()
    -- Index v e t -> do
    --   index   <- genExpr e
    --   arrTemp <- 
    _     -> pushVariable var temp


-- Prolog con New
genUnOp :: UnOp -> Expr -> Type -> TACMonad TACOP
genUnOp op e tOp = do
  rv   <- genExpr e
  actO <- pushOffset (getWidth tOp)
  lv   <- newTemp tOp (getWidth tOp) actO
  
  case op of
    Length   -> tell (tacUn T.Length lv rv) >> return lv
    Negative -> tell (tacUn T.Minus lv rv)  >> return lv
    New      -> do
      tell [tacParam rv 0]
      tell (tacCall lv "malloc" 1)
      -- prólogo
      state <- get
      put state{callM = True}
      return lv
    charOp   -> do
      l0 <- newLabel
      l1 <- newLabel
      l2 <- newLabel
      let
        c0    = tacConstant ("0", TInt)
        c25   = tacConstant ("25", TInt)
        c32   = tacConstant ("32", TInt)
        check = tacBin T.Gte lv c0 l0 ++ tacGoto l2 ++ tacNewLabel l0 : tacBin T.Sub lv rv c25
        goNew = tacGoto l2 ++ [tacNewLabel l1]

      if charOp == UpperCase then do
        tell (tacBin T.Sub lv rv $ tacConstant ("97", TInt))
        tell check
        tell (tacBin T.Gte lv c0 l0)
        tell goNew
        tell (tacBin T.Sub lv rv c32)
        tell [tacNewLabel l2]
        return lv
      else do
        tell (tacBin T.Sub lv rv $ tacConstant ("65", TInt))
        tell check
        tell (tacBin T.Lte lv c0 l0)
        tell goNew
        tell (tacBin T.Add lv rv c32)
        tell [tacNewLabel l2]
        return lv


-- TODO: Listas
genBinOp :: BinOp -> Type -> TACOP -> TACOP -> TACMonad TACOP
genBinOp op tOp rv1 rv2 = do
  actO <- pushOffset (getWidth tOp)
  lv   <- newTemp tOp (getWidth tOp) actO
  
  -- case op of
  -- Aritmethics
  --  op ->
  tell (tacBin (binOpToTACOP op) lv rv1 rv2)  >> return lv
  -- Lists
    -- Anexo  -> return (e1Code ++ e2Code ++ [T.ThreeAddressCode T.Anexo lvt e1Temp e2Temp], lvt)
    -- Concat -> return (e1Code ++ e2Code ++ [T.ThreeAddressCode T.Concat lvt e1Temp e2Temp], lvt)


-- 
genTerOp :: Expr -> Expr -> Expr -> Type -> TACMonad TACOP
genTerOp eB eT eF tOp = do
  actO   <- pushOffset (getWidth tOp)
  lv     <- newTemp tOp (getWidth tOp) actO
  next   <- newLabel
  falseL <- newLabel
  genBoolExpr eB fall falseL
  eTTemp <- genExpr eT
  tell (tacAssign lv eTTemp)
  tell (tacGoto next)
  tell [tacNewLabel falseL]
  eFTemp <- genExpr eF
  tell (tacAssign lv eFTemp)
  tell [tacNewLabel next]
  return lv


-- Tamano en el primer elemento
-- TODO: poner en .data el arreglo y su tam ??
genArrayList :: [Expr] -> Int -> Int -> TACOP -> TACMonad TACOP
genArrayList [] _ index arrTemp               =
  let len = tacConstant (show index, TInt)
  in tell (tacSet arrTemp (tacConstant ("0", TInt)) len) >> return arrTemp
genArrayList (elem:elems) width index arrTemp = do
  elemTemp <- genExpr elem
  tell (tacSet arrTemp (tacConstant (show (index + 1), TInt)) elemTemp)
  actO     <- pushOffset width
  genArrayList elems width (index + 1) (modifyOffSet arrTemp actO width)


-- TODO: width del tipo string
-- Creo que no llega hasta aqui, en genAssig toco sus casos
-- TODO: integrar aqui lo que hay en genAssig y asi esta en un solo sitio
genRead :: TACMonad TACOP
genRead = do
  lv    <- pushOffset 4 >>= newTemp TInt 4
  -- param <- genExpr e
  -- tell [tacRead]
  return lv


-- Hace prologo
genFuncCall :: Subroutine -> Type -> TACMonad TACOP
genFuncCall (Call f params) t = do
  pushSubroutine f params False
  genParams (map fst params) 0
  lv <- pushOffset (getWidth t) >>= newTemp t (getWidth t) -- Deberia ser el offset del tipo de retorno de la funcion, como lo obtengo?
  -- Prologo antes de pasar el poder al proc
  tell (tacCall lv f $ length params)
  return lv


-- Retorna el tamaño a ser reservado en memoria
genType :: Type -> TACMonad TACOP
genType t = return (tacConstant (show (getWidth t), TInt))


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--- Auxiliares que deben ir en este archivo


-------------------------------------------------------------------------------
forComparison :: Id -> Expr -> Expr -> TACOP -> TACMonad (Id, TACOP, TACOP, TACOP)
forComparison n e1 e2 nextL = do
  begin   <- newLabel
  cont    <- continue
  genAssig (Var n TInt) e1 -- 
  iterVar <- genExpr (Variable (Var n TInt) TInt)
  e1Temp  <- genExpr e1
  tell (tacAssign iterVar e1Temp)
  tell (tacAssign (tacLabel (n ++ "_int")) iterVar) -- TODO: quitar para no acceder a memoria
  e2Temp  <- genExpr e2
  tell [tacNewLabel begin]
  genComparison iterVar e2Temp fall nextL LessEq
  return (n, begin, cont, iterVar)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
forInstrs :: InstrSeq -> TACOP -> (Id, TACOP, TACOP, TACOP) -> TACMonad ()
forInstrs is nextL (n, begin, cont, iterVar) = do
  mapM_ genCode is
  tell [tacNewLabel cont]
  iterVarIncr <- genBinOp Add TInt iterVar (tacConstant ("1", TInt))
  tell (tacAssign iterVar iterVarIncr)
  tell (tacAssign (tacLabel (n ++ "_int")) iterVar) -- TODO: quitar para no acceder a memoria
  tell (tacGoto begin)
  tell [tacNewLabel nextL]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- 
genParams :: [Expr] -> Int -> TACMonad ()
genParams [] _ = tell []
genParams (p:ps) n = do
  param <- genExpr p
  tell $ [tacParam param n]
  genParams ps $ n + 1
-------------------------------------------------------------------------------


--  containsErrorString :: String -> Bool
--  containsErrorString x = x =~ "ERROR" :: Bool

--  fileContainsErrorString fileName = do
--     s <- readFile fileName
--     return $ containsErrorString s


-- isLabelInData :: String -> String -> IO Bool
-- isLabelInData label file = do
--   _data <- readFile file
--   return (label =~ label :: Bool)
