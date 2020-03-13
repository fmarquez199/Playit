{- |
 * TAC auxiliary functions
 *
 * Copyright : (c) 
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.BackEnd.Utils where

import Control.Monad.Trans.RWS     (tell, get, put)
import Data.Maybe
import Playit.BackEnd.Types
import Playit.FrontEnd.SymbolTable (lookupInSymTab)
import Playit.FrontEnd.Types
import Playit.FrontEnd.Utils       (getName)
import qualified Data.Map          as M
import qualified TACType           as T


-------------------------------------------------------------------------------
-- Creates the TAC's variable operand
tacVariable :: TACInfo -> TACOP
tacVariable v = Just $ T.Id v
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Creates the TAC's constant operand
tacConstant :: (String, Type) -> TACOP
tacConstant c = Just $ T.Constant c
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Creates the TAC's label operand
tacLabel :: String -> TACOP
tacLabel l = Just $ T.Label l
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
binOpToTACOP :: BinOp -> T.Operation
binOpToTACOP op = case op of
  Add       -> T.Add
  DivEntera -> T.Div
  Division  -> T.Div
  Minus     -> T.Sub
  Module    -> T.Mod
  Mult      -> T.Mult
  Greater   -> T.Gt
  GreaterEq -> T.Gte
  Less      -> T.Lt
  LessEq    -> T.Lte
  Eq        -> T.Eq
  NotEq     -> T.Neq
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
negation :: T.Operation -> T.Operation
negation op = case op of
  T.Lt  -> T.Gte
  T.Lte -> T.Gt
  T.Gt  -> T.Lte
  T.Gte -> T.Lt
  T.Eq  -> T.Neq
  T.Neq -> T.Eq
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
fall :: TACOP
fall = Just $ T.Label "-1"

isFall :: TACOP -> Bool
isFall (Just (T.Label l)) = l == "-1"
isFall _                  = error "Calling isFall with no label operand"
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacBin :: T.Operation -> TACOP -> TACOP -> TACOP -> [TAC]
tacBin op lv rv1 rv2 = [T.ThreeAddressCode op lv rv1 rv2]

tacUn :: T.Operation -> TACOP -> TACOP -> [TAC]
tacUn op lv rv = [T.ThreeAddressCode op lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Creates the TAC's GoTo
tacGoto :: TACOP -> [TAC]
tacGoto label = [T.ThreeAddressCode T.GoTo Nothing Nothing label]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Creates the TAC's GoTo
tacNewLabel :: TACOP -> [TAC]
tacNewLabel label = [T.ThreeAddressCode T.NewLabel label Nothing Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacAssign :: TACOP -> TACOP -> [TAC]
tacAssign lv rv = [T.ThreeAddressCode T.Assign lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacIf :: TACOP -> TACOP -> [TAC]
tacIf cond label = [T.ThreeAddressCode T.If Nothing cond label]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacSet :: TACOP -> TACOP -> TACOP -> [TAC]
tacSet x i y = [T.ThreeAddressCode T.Set x i y]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacGet :: TACOP -> TACOP -> TACOP -> [TAC]
tacGet x y i = [T.ThreeAddressCode T.Get x y i]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacCall :: TACOP -> String -> Int -> [TAC]
tacCall lv s n = [T.ThreeAddressCode T.Call lv (tacLabel s) (tacConstant (show n,TInt))]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacParam :: TACOP -> TAC
tacParam p = T.ThreeAddressCode T.Param Nothing p Nothing
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Modify the symbol offset
modifyOffSet :: TACOP -> OffSet -> TACOP
modifyOffSet (Just (T.Id (Temp n _))) newO      = tacVariable $ Temp n newO
modifyOffSet (Just (T.Id (TACVar info _))) newO = tacVariable $ TACVar info newO
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Gets the with of the Type
getWidth :: Type -> Int
getWidth array@(TArray _ t) = 4 * getWidth t
getWidth TBool              = 1
getWidth TChar              = 1
getWidth TFloat             = 8
getWidth TInt               = 4
getWidth (TList t)          = getWidth t -- ?
getWidth TListEmpty         = 4 -- como base?
getWidth (TNew n)           = 4 -- Reg or Union
getWidth (TPointer t)       = 4 * getWidth t -- Una palabra del procesador
getWidth TNull              = 4 -- Reservemos ese espacio asi igual
-- getWidth TStr               = 1 -- Se calcula cuando se tiene el valor del string
-- getWidth TRegister          = 1 -- Suma de los witdth de cada campo. Esto esta en su offset
-- getWidth TUnion             = 1 -- witdth mayor. Esto esta en su offset
getWidth _                  = -1 -- This shouldn't happen
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
lenArray :: Expr -> Int
lenArray (ArrayList ls _) = length ls
lenArray _                = 1
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
getAST :: [ExtraInfo] -> InstrSeq
getAST []        = []
getAST (AST i:_) = i
getAST (_:rs)    = getAST rs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
getRefVar :: Var -> Var
getRefVar (Desref v _) = v
getRefVar var          = var
-------------------------------------------------------------------------------


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
newTemp :: OffSet -> TACMonad TACOP
newTemp actO = do
  state@Operands{temps = ts} <- get
  let t = Temp ("$t" ++ show (M.size ts - 4)) actO
  put state{temps = M.insert t True ts}
  return $ tacVariable t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Tal vez colocar labs como [String]
newLabel :: TACMonad TACOP
newLabel = do
  state@Operands{labs = ls} <- get
  let newL = length ls
  put state{labs = newL:ls}
  return $ tacLabel $ show newL
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
pushLiteral :: Literal -> TACOP -> TACMonad ()
pushLiteral l operand = do
  state@Operands{lits = ls} <- get
  put state{lits = M.insert l operand ls}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- NOTA: si se guarda en un temporal no se accede a memoria. 
pushVariable :: Var -> Type -> TACMonad TACOP
pushVariable var tVar = do
  Operands{vars = vs, astST = st} <- get
  if M.member var vs then return $ fromJust $ M.lookup var vs -- Aumentar las veces que esta siendo usada (TACOP, Int <veces usada>)
  else do
    let info = head . fromJust $ lookupInSymTab (getName var) st
    actO <- pushOffset (getWidth tVar)
    temp <- newTemp actO
    
    state@Operands{vars = vs, astST = st} <- get
    put state{vars = M.insert var temp vs}
    
    if category info == Parameters Reference then
      return $ tacVariable $ TACVar info actO
    else
      return temp
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
pushSubroutine :: Id -> Bool -> TACMonad ()
pushSubroutine s isProc = do
  state@Operands{subs = subroutines, astST = st} <- get
  let ast = getAST . extraInfo . head . fromJust $ lookupInSymTab s st
  put state{subs = (s, ast, isProc):subroutines}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Actualizar offset dependiendo del tamaño del tipo de parametro a pasar ??
getParam :: Int -> TACMonad TACOP
getParam x = do
  state@Operands{temps = ts} <- get
  let a = Temp ("$a" ++ show x) (-1)
  put state{temps = M.insert a True ts}
  return $ tacVariable a
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
setMemoryTemps :: TACMonad (TACOP,TACOP,TACOP,TACOP,TACOP)
setMemoryTemps = do
  param0 <- getParam 0
  head   <- pushOffset 8  >>= newTemp
  elem   <- pushOffset 12 >>= newTemp
  temp1  <- pushOffset 4  >>= newTemp
  temp2  <- pushOffset 4  >>= newTemp
  return (head, elem, param0, temp1, temp2)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
setElemIndexs :: (TACOP,TACOP,TACOP,TACOP,TACOP)
setElemIndexs = (zero,one,two,three,sixteen)
  where
    zero    = tacConstant ("0", TInt)
    one     = tacConstant ("1", TInt)
    two     = tacConstant ("2", TInt)
    three   = tacConstant ("3", TInt)
    sixteen = tacConstant ("16", TInt)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Functions

-------------------------------------------------------------------------------
-- prologue
-- prologue
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- epilogue
-- epilogue
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
syscall :: Int -> TACOP -> TACOP -> TACMonad ()
syscall v0 lv param = do
  tell [tacParam param]                         -- $a0
  tell [tacParam $ tacConstant (show v0, TInt)] -- $v0
  tell (tacCall lv "syscall" 2)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
{-
  _head[0] = pointer to first list element
  _head[1] = amount of memory allocated 
  _elem[0] = pointer to next list element
  _elem[1] = flag isFree (0 -> ocuppied, 1 -> free)
  _elem[2] = size of allocated block
  _elem[3] = address to heap allocated memory
-}
malloc :: TACMonad ()
malloc = do
  (head,elem,par0,temp1,temp2) <- setMemoryTemps
  -- temp1: Guarda el valor del parámetro pasado $a0 (requestedBytes)
  -- temp2: Contenido de _head cuando hay mem y flag isFree de _elem cuando no
  let
    (zero,one,two,three,sixteen) = setElemIndexs
    retn                         = tacVariable $ Temp "_return" (-1)
    allocate                     = tacLabel "allocate"
    noMemory                     = tacLabel "noMemory"
    lookMem                      = tacLabel "lookMemory"
    nextElemM                    = tacLabel "nextElementMalloc"
    exitMall                     = tacLabel "exitMalloc"
  
  tell (tacNewLabel $ tacLabel "malloc")
  -- prologo
  -- requestedBytes := $a0      De esto no deberia encargarse el prologo?
  tell (tacAssign temp1 par0)

  -- _return := syscall9(requestedBytes)
  syscall 9 retn temp1

  -- if _return = 0 goto 2
  tell (tacBin T.Eq retn zero noMemory)

-- 1: Hay memoria disponible, entonces crea el bloque con la info
  tell (tacNewLabel allocate)
  -- _elem     := syscall9(16)
  syscall 9 elem sixteen
  -- _elem[0]  := _head[0]
  tell (tacGet temp2 head zero)
  tell (tacSet elem zero temp2)
  -- _elem[1]  := 0
  tell (tacSet elem one zero)
  -- _elem[2]  := requestedBytes
  tell (tacSet elem two temp1)
  -- _elem[3] := _return
  tell (tacSet elem three retn)
  -- _head[0]  := _elem
  tell (tacSet head zero elem)
  -- _head[1]  := _head[1] + requestedBytes
  tell (tacGet temp2 head one)
  tell (tacBin T.Add temp2 temp2 temp1)
  tell (tacSet head one temp2)
  
-- 5: Fin, retorna la dir de mem
  -- epilogo
  -- return _return
  tell (tacNewLabel exitMall)
  tell [T.ThreeAddressCode T.Return Nothing retn Nothing]
  
-- 2: No hay memoria disponible, entonces se busca en la lista si hay algun bloque libre que encaje en el tamaño de memoria solicitado
  tell (tacNewLabel noMemory)
  -- _elem  := _head[0]
  tell (tacUn T.Deref elem head)
  -- isFree := _elem[1]
  tell (tacGet temp2 elem one)

-- 3: 
  tell (tacNewLabel lookMem)
  -- if isFree = 0 goto 4
  tell (tacBin T.Eq temp2 zero nextElemM)
  -- blockSize := _elem[2]
  tell (tacGet temp2 elem two)
  -- if blockSize < requestedBytes goto 4
  tell (tacBin T.Lt temp2 temp1 nextElemM)
  -- _return = _elem[3]
  tell (tacGet retn elem three)
  -- goto 5
  tell (tacGoto exitMall)

-- 4: Se mueve al siguiente elemento para ver si cabe ahi la cantidad de memoria solicitada
  tell (tacNewLabel nextElemM)
  -- Revisar si es el ultimo de la lista
  -- if _elem[0] == 0  goto 5
  tell (tacGet temp2 elem zero)
  tell (tacBin T.Eq temp2 zero exitMall)
  -- _elem  := _elem[0]
  tell (tacGet elem elem zero)
  -- isFree := _elem[1]
  tell (tacGet temp2 elem one)
  -- goto 3
  tell (tacGoto lookMem)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
{-
  _head[0] = pointer to first list element
  _head[1] = amount of memory allocated 
  _elem[0] = pointer to next list element
  _elem[1]4 = flag isFree (0 -> ocuppied, 1 -> free)
  _elem[2]8 = size of allocated block
  _elem[3]12 = adress to heap allocated memory
-}
free :: TACMonad ()
free = do
  (head,elem,par0,temp1,temp2) <- setMemoryTemps
  fre <- pushOffset 4 >>= newTemp -- Dir de mem a liberar
  -- temp1: Guarda la direccion de memoria a ser liberada
  -- temp2: Flag isFree
  let
    (zero,one,two,three,_) = setElemIndexs
    beginFree              = tacLabel "beginFree"
    nextElemF              = tacLabel "nextElementFree"
    exitFree               = tacLabel "exitFree"
  
  tell (tacNewLabel (tacLabel "free"))
  -- address := $a0
  tell (tacAssign temp1 par0)

  -- _elem := _head[0]
  tell (tacGet elem head zero)

-- 1: Revisa si llega al ultimo elemento y si la dir de mem a liberar es la indicada y la libera
  tell (tacNewLabel beginFree)
  -- if _elem[0] == 0 goto 3
  tell (tacGet temp2 elem zero)
  tell (tacBin T.Eq temp2 zero exitFree)
  -- _free := _elem[3]
  tell (tacGet fre elem three)
  -- if _free != address goto 2
  tell (tacBin T.Neq fre temp1 nextElemF)
  -- _free    := *_free
  tell (tacUn T.Deref fre fre)
  -- _free[1] := 1
  tell (tacSet fre one one)
  -- goto 3
  tell (tacGoto exitFree)

-- 2: Siguiente elemento a revisar si es el que debe ser liberado
  tell (tacNewLabel nextElemF)
  -- _elem     := _elem[0]
  tell (tacGet elem elem zero)
  -- goto 1
  tell (tacGoto beginFree)

-- 3:
  tell (tacNewLabel exitFree)
  tell [T.ThreeAddressCode T.Return Nothing Nothing Nothing]
-------------------------------------------------------------------------------
