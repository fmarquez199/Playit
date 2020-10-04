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
import Playit.FrontEnd.Utils       (getName, typeVar)
import qualified Data.Map          as M
import qualified TACType           as T


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- TAC
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
isLit :: Expr -> Bool
isLit (Literal _ _) = True
isLit _             = False
-------------------------------------------------------------------------------


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
tacNewLabel :: TACOP -> TAC
tacNewLabel label = T.ThreeAddressCode T.NewLabel label Nothing Nothing
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- TODO:
tacAssign :: TACOP -> TACOP 
          -- -> TACOP -- ^ Label in .data to refer when assign strings and arrays
          -> [TAC]
tacAssign lv rv {- _dataLabel -} = [T.ThreeAddressCode T.Assign lv rv Nothing]
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
tacCall lv s n = 
  [T.ThreeAddressCode T.Call lv (tacLabel s) (tacConstant (show n,TInt))]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacParam :: TACOP 
         -> Int   -- ^ Numero de parametro
         -> TAC
tacParam p n = T.ThreeAddressCode T.Param Nothing p (tacConstant (show n, TInt))
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Para pasarlo como arg
tacParam' :: TACOP -> Int -> TACOP -> TAC
tacParam' p n r = T.ThreeAddressCode T.Param p (tacConstant (show n, TInt)) r
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- 
tacPrint :: TACOP -- ^ String to print in TAC
         -> TACOP -- ^ Label in .data section that have the string to print
         -> TAC
tacPrint e dataLabel = T.ThreeAddressCode T.Print Nothing e dataLabel
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacRead :: TACOP -- ^ Var to store the input
        -> TACOP -- ^ Label in .data section that have the buffer to store the input
        -> TAC
tacRead var buffer = T.ThreeAddressCode T.Read Nothing var buffer
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacRef :: TACOP -> TACOP -> TAC
tacRef x y = T.ThreeAddressCode T.Ref x y Nothing
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
tacDeref :: TACOP -> TACOP -> TAC
tacDeref x y = T.ThreeAddressCode T.Deref x y Nothing
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
tacExit :: TAC
tacExit = T.ThreeAddressCode T.Exit Nothing Nothing Nothing
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Modify the symbol offset
modifyOffSet :: TACOP -> OffSet -> Width -> TACOP
modifyOffSet (Just (T.Id (Temp n t _))) o w = tacVariable $ Temp n t (o, w)
modifyOffSet (Just (T.Id (TACVar i _))) o w = tacVariable $ TACVar i (o, w)
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

asigStr :: TACOP -> Int -> String -> TACOP -> TACOP -> TACMonad ()
asigStr v _ "" _ _ = tell []
asigStr v l s i t = do
  tell (tacAssign i (tacConstant (show l, TInt)))
  tell (tacAssign t (tacConstant ([head s], TChar)))
  tell (tacSet v i t)
  asigStr v (l + 1) (tail s) i t

copyStr :: TACOP -> TACOP -> TACOP -> Int -> Int -> TACMonad ()
copyStr lv rv t i n = if i > n then tell [] else do
  tell (tacGet t rv (tacConstant (show i, TInt)))
  tell (tacSet lv (tacConstant (show i, TInt)) t)
  copyStr lv rv t (i + 1) n

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
newTemp :: Type -> Width -> OffSet -> TACMonad TACOP
newTemp typ w o = do
  state@Operands{temps = ts} <- get
  let t = if typ == TFloat then Temp ("$t" ++ show (1 - M.size ts)) typ (o, w)
      else Temp ("$t" ++ show (M.size ts - 2)) typ (o, w)
  put state{temps = M.insert t True ts}
  return $ tacVariable t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
newLabel :: TACMonad TACOP
newLabel = do
  state@Operands{labs = ls} <- get
  let newL = "l." ++ show (length ls)
  put state{labs = newL:ls}
  return $ tacLabel $ show newL
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
pushOffset :: Width -> TACMonad OffSet
pushOffset width = do
  state@Operands{base = actO} <- get
  let newO = actO + width
  put state{base = newO}
  return actO

resetOffset :: TACMonad ()
resetOffset = do
  state <- get
  put state{base = 0}

getOffset :: Var -> TACMonad (TACOP, OffSet, Width)
getOffset var = do
  Operands{vars = vs} <- get
  if M.member var vs then
    case (fromJust $ fromJust $ M.lookup var vs) of
      v@(T.Id (Temp _ _ (o, w))) -> return (Just v, o, w)
      v@(T.Id (TACVar _ (o, w))) -> return (Just v, o, w)
      _ -> return (Nothing, 0, 0)
  else return (Nothing, 0, 0)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
pushLiteral :: TACOP -> TACOP -> TACMonad ()
pushLiteral lit temp = do
  state@Operands{lits = ls} <- get
  put state{lits = M.insert lit temp ls}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- NOTA: si se guarda en un temporal no se accede a memoria. 
pushVariable :: Var -> TACOP -> TACMonad TACOP
pushVariable var temp = do
  Operands{vars = vs, astST = st} <- get
  if M.member var vs then return $ fromJust $ M.lookup var vs -- Aumentar las veces que esta siendo usada (TACOP, Int <veces usada>)
  else do
    let info = head . fromJust $ lookupInSymTab (getName var) st

    state@Operands{base = actO} <- get
    put state{vars = M.insert var temp vs}
    
    if category info == Parameters Reference then
      return $ tacVariable $ TACVar info (actO, getWidth (typeVar var))
    else
      return temp
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
pushSubroutine :: Id -> Params -> Bool -> TACMonad ()
pushSubroutine s ps isProc = do
  state@Operands{subs = subroutines, astST = st} <- get
  let ast = getAST . extraInfo . head . fromJust $ lookupInSymTab s st
  put state{subs = (s, ps, ast, isProc):subroutines}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Actualizar offset dependiendo del tamaño del tipo de parametro a pasar ??
getParam :: Type -> Int -> TACMonad TACOP
getParam t x = do
  state@Operands{temps = ts} <- get
  let a = Temp ("$a" ++ show x) t (-1, getWidth t)
  put state{temps = M.insert a True ts}
  return $ tacVariable a
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Used in 'malloc' and 'free'
setMemoryTemps :: TACMonad (TACOP,TACOP,TACOP,TACOP,TACOP)
setMemoryTemps = do
  param0 <- getParam TInt 0
  head   <- pushOffset 8  >>= newTemp TInt 4
  elem   <- pushOffset 12 >>= newTemp TInt 4
  temp1  <- pushOffset 4  >>= newTemp TInt 4
  temp2  <- pushOffset 4  >>= newTemp TInt 4
  return (head, elem, param0, temp1, temp2)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Used in 'malloc' and 'free'
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
tacSyscall :: Int -> TACOP -> [TACOP] -> TACMonad ()
tacSyscall v0 lv params = do
  tell (map (\(x,y) -> tacParam x y) [(x,y) | x <- params, y <- [0..length params - 1]])
  tell [tacParam (tacConstant (show v0, TInt)) 0] -- $v0
  tell (tacCall lv "syscall" (length params + 1))
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
    retn                         = tacVariable $ Temp "_return" TInt (-1, 4)
    allocate                     = tacLabel "allocate"
    noMemory                     = tacLabel "noMemory"
    lookMem                      = tacLabel "lookMemory"
    nextElemM                    = tacLabel "nextElementMalloc"
    exitMall                     = tacLabel "exitMalloc"
  
  tell [tacNewLabel $ tacLabel "malloc"]
  -- prologo
  -- requestedBytes := $a0      De esto no deberia encargarse el prologo?
  tell (tacAssign temp1 par0)

  -- _return := syscall9(requestedBytes)
  tacSyscall 9 retn [temp1]

  -- if _return = 0 goto 2
  tell (tacBin T.Eq retn zero noMemory)

-- 1: Hay memoria disponible, entonces crea el bloque con la info
  tell [tacNewLabel allocate]
  -- _elem     := syscall9(16)
  tacSyscall 9 elem [sixteen]
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
  tell [tacNewLabel exitMall]
  tell [T.ThreeAddressCode T.Return Nothing retn Nothing]
  
-- 2: No hay memoria disponible, entonces se busca en la lista si hay algun bloque libre que encaje en el tamaño de memoria solicitado
  tell [tacNewLabel noMemory]
  -- _elem  := _head[0]
  tell (tacUn T.Deref elem head)
  -- isFree := _elem[1]
  tell (tacGet temp2 elem one)

-- 3: 
  tell [tacNewLabel lookMem]
  -- if isFree = 0 goto 4
  tell (tacBin T.Eq temp2 zero nextElemM)
  -- blockSize := _elem[2]
  tell (tacGet temp2 elem two)
  -- if blockSize < requestedBytes goto 4
  tell (tacBin T.Lt temp2 temp1 nextElemM)
  -- _return = _elem[3] >>>> Si el bloque es mayor se deberia particionar???
  tell (tacGet retn elem three)
  -- goto 5
  tell (tacGoto exitMall)

-- 4: Se mueve al siguiente elemento para ver si cabe ahi la cantidad de memoria solicitada
  tell [tacNewLabel nextElemM]
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
  fre <- pushOffset 4 >>= newTemp TInt 4 -- Dir de mem a liberar
  -- temp1: Guarda la direccion de memoria a ser liberada
  -- temp2: Flag isFree
  let
    (zero,one,two,three,_) = setElemIndexs
    beginFree              = tacLabel "beginFree"
    nextElemF              = tacLabel "nextElementFree"
    exitFree               = tacLabel "exitFree"
  
  tell [tacNewLabel $ tacLabel "free"]
  -- address := $a0
  tell (tacAssign temp1 par0)

  -- _elem := _head[0]
  tell (tacGet elem head zero)

-- 1: Revisa si llega al ultimo elemento y si la dir de mem a liberar es la indicada y la libera
  tell [tacNewLabel beginFree]
  -- if _elem[0] == 0 goto 3
  tell (tacGet temp2 elem zero)
  tell (tacBin T.Eq temp2 zero exitFree)
  -- _free := _elem[3]
  tell (tacGet fre elem three)
  -- if _free != address goto 2
  tell (tacBin T.Neq fre temp1 nextElemF)
  -- _free    := *_free
  tell (tacUn T.Deref fre fre)
  -- _free[1] := 1 >>>> Deberia es eliminar el elemento de la lista, asi no habria que juntar 2 elementos al ver que esten libres en malloc
  tell (tacSet fre one one)
  -- goto 3
  tell (tacGoto exitFree)

-- 2: Siguiente elemento a revisar si es el que debe ser liberado
  tell [tacNewLabel nextElemF]
  -- _elem     := _elem[0]
  tell (tacGet elem elem zero)
  -- goto 1
  tell (tacGoto beginFree)

-- 3:
  tell [tacNewLabel exitFree]
  tell [T.ThreeAddressCode T.Return Nothing Nothing Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Assembler .data section code generation
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Generates the code for defining strings (.asciiz data)
-- 
_asciiz :: String -> String -> String -> IO ()
_asciiz name str file = appendFile file $ "\n" ++ name ++ ": .asciiz " ++ show str
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Generates the code for '.word' data
-- 
_word :: String -> String -> String -> IO ()
_word name num file = appendFile file $ "\n" ++ name ++ ": .word " ++ num
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Generates the code for '.space' data
-- 
_space :: String -> String -> String -> IO ()
_space name num file = appendFile file $ "\n" ++ name ++ ": .space " ++ num
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Generates the code for '.double' data
-- 
_double :: String -> String -> String -> IO ()
_double name num file = appendFile file $ "\n" ++ name ++ ": .double " ++ num
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Assembler instructions code generation
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

comment :: String -> String -> IO ()
comment com file = appendFile file com

syscall :: String -> IO ()
syscall file = appendFile file "\n\t\tsyscall\n"

-- load and store

lw :: String -> String -> String -> IO ()
lw regDest dir file = appendFile file $ "\n\t\tlw " ++ regDest ++ ", " ++ dir

la :: String -> String -> String -> IO ()
la regDest dir file = appendFile file $ "\n\t\tla " ++ regDest ++ ", " ++ dir

li :: String -> String -> String -> IO ()
li regDest int file = appendFile file $ "\n\t\tli " ++ regDest ++ ", " ++ int

sw :: String -> String -> String -> IO ()
sw regSour dir file = appendFile file $ "\n\t\tsw " ++ regSour ++ ", " ++ dir

l_d :: String -> String -> String -> IO ()
l_d regDest dir file = appendFile file $ "\n\t\tl.d " ++ regDest ++ ", " ++ dir

li_d :: String -> String -> String -> IO ()
li_d regDest dir file = appendFile file $ "\n\t\tli.d " ++ regDest ++ ", " ++ dir

s_d :: String -> String -> String -> IO ()
s_d regSour dir file = appendFile file $ "\n\t\ts.d " ++ regSour ++ ", " ++ dir

swl :: String -> String -> String -> IO ()
swl regSour dir file = appendFile file $ "\n\t\tswl " ++ regSour ++ ", " ++ dir

swr :: String -> String -> String -> IO ()
swr regSour dir file = appendFile file $ "\n\t\tswr " ++ regSour ++ ", " ++ dir

-- aritmetic

add :: String -> String -> String -> String -> IO ()
add result reg1 reg2 file =
  appendFile file $ "\n\t\tadd " ++ result ++ ", " ++ reg1 ++ ", " ++ reg2

addi :: String -> String -> String -> String -> IO ()
addi result reg1 int file =
  appendFile file $ "\n\t\taddi " ++ result ++ ", " ++ reg1 ++ ", " ++ int

sub :: String -> String -> String -> String -> IO ()
sub result reg1 reg2 file =
  appendFile file $ "\n\t\tsub " ++ result ++ ", " ++ reg1 ++ ", " ++ reg2

mult :: String -> String -> String -> String -> IO ()
mult result reg1 reg2 file =
  appendFile file $ "\n\t\tmult " ++ result ++ ", " ++ reg1 ++ ", " ++ reg2

div' :: String -> String -> String -> String -> IO ()
div' result reg1 reg2 file =
  appendFile file $ "\n\t\tdiv " ++ result ++ ", " ++ reg1 ++ ", " ++ reg2

div_d :: String -> String -> String -> String -> IO ()
div_d result reg1 reg2 file =
  appendFile file $ "\n\t\tdiv.d " ++ result ++ ", " ++ reg1 ++ ", " ++ reg2


-- jumps


beqz :: String -> String -> String -> IO ()
beqz comp label file = appendFile file $ "\n\t\tbeqz " ++ comp ++ ", " ++ label

b, jal, jr :: String -> String -> IO ()
b   label file = appendFile file $ "\n\t\tb " ++ label
jal dir   file = appendFile file $ "\n\t\tjal " ++ dir
jr  reg   file = appendFile file $ "\n\t\tjr " ++ reg


-- data transfer


-- | 
-- Copy the content of regSour to regDest
-- para los parametros
move :: String -> String -> String -> IO ()
move regDest regSour file =
  appendFile file $ "\n\t\tmove " ++ regDest ++ ", " ++ regSour


mov_d :: String -> String -> String -> IO ()
mov_d regDest regSour file =
  appendFile file $ "\n\t\tmov.d " ++ regDest ++ ", " ++ regSour


-- | Copy regSour to regDest. Note ordering
-- 
mtc1_d :: String -> String -> String -> IO ()
mtc1_d regSour regDest file =
  appendFile file $ "\n\t\tmtc1.d " ++ regSour ++ ", " ++ regDest

mfhi :: String -> String -> IO ()
mfhi regDest file = appendFile file $ "\n\t\tmfhi " ++ regDest


-- mfc1.d Rd, FRs1

-- Mueve  desde  el  Coprocesador  1
-- un valor de doble precisión

-- Mueve  el  contenido  de  los  registros  de  punto-flotante
-- FRs1 y FRs1+1 hacia los registros Rd y Rd+1 de la CPU
