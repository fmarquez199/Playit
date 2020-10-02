{- |
 * FinalCode
 *
 * Copyright : (c) 
 *  Francisco Javier 12-11163
 *  Natascha Gamboa  12-11250
-}
module Playit.BackEnd.FinalCode (genFinalCode) where

import Control.Monad.IO.Class                (liftIO)
import Data.List                             (subsequences)
import Data.Maybe                            (isJust, fromJust)
import Data.Strings                          (strSplit)
import Playit.BackEnd.RegAlloc.GraphColoring (VertColorMap)
import Playit.BackEnd.Types                  (TAC, TACOP, TACInfo(..), InterfGraph)
import Playit.BackEnd.Utils                  {- (tacNewLabel, tacLabel) -}
import Playit.FrontEnd.Types                 (Type(..), symType)
import TACType                               (ThreeAddressCode(..), Operation(..), Operand(..))
import qualified Data.Graph  as G
import qualified Data.IntMap as I


-- ThreeAddressCode NewLabel (Just l) Nothing Nothing
genFinalCode :: [TAC] -> InterfGraph -> VertColorMap -> String -> IO ()
genFinalCode [] _ _ fileName = appendFile fileName ""
genFinalCode tac g c n =
  let h = head tac
  in 
  case tacOperand h of
    -- Faltan And, Or
    -- Not supported Anexo, Concat
    x | x `elem` [Add, Sub, Mult, Div, Mod, Gt, Gte, Lt, Lte, Eq, Neq, Get, Set, Call] ->
      
      liftIO (print (show h ++ " tac: " ++ show (tacInfo $ tacRvalue2 h))) >> 
        genThreeOperandsOp h g c n >> 
          genFinalCode (tail tac) g c n

    x | x `elem` [Minus, Ref, Deref] -> genTwoOperandsOp h g c n >> genFinalCode (tail tac) g c n
    x | x `elem` [Return, If, GoTo]          -> genJumps h g c n         >> genFinalCode (tail tac) g c n
    x | x `elem` [Print, Read, Exit]         -> genSyscalls h g c n      >> genFinalCode (tail tac) g c n
    
    NewLabel -> do
      if take 2 (show h) == "l." then
        let lbl = tail . init . reverse . snd . splitAt 2 . reverse $ show h
        in appendFile n (lbl ++ ":\n")
      else do
        appendFile n (show h ++ "\n")
        if "main: " == show h then putStrLn "compiling" else prologue n
      
      genFinalCode (tail tac) g c n
    
    -- Length -> genLength h -- busca el label en .data que corresponda, si x = # [1,2] -> en TAC guardar el array en .data y colocarle un nombre y tu tam
    Assign -> genAssign h g c n >> genFinalCode (tail tac) g c n
    Param  -> genParam h g c n  >> genFinalCode (tail tac) g c n
    -- Access
    _      -> appendFile n ""   >> genFinalCode (tail tac) g c n

-- ThreeAddressCode Add (Just x) (Just y) (Just z)
-- ThreeAddressCode Sub (Just x) (Just y) (Just z)
-- ThreeAddressCode Mult (Just x) (Just y) (Just z)
-- ThreeAddressCode Div (Just x) (Just y) (Just z)
-- ThreeAddressCode Mod (Just x) (Just y) (Just z)
-- ThreeAddressCode Gt (Just x) (Just y) (Just z)
-- ThreeAddressCode Gte (Just x) (Just y) (Just z)
-- ThreeAddressCode Lt (Just x) (Just y) (Just z)
-- ThreeAddressCode Lte (Just x) (Just y) (Just z)
-- ThreeAddressCode Eq (Just x) (Just y) (Just z)
-- ThreeAddressCode Neq (Just x) (Just y) (Just z)
-- ThreeAddressCode Call (Just x) (Just f) (Just n)
-- ThreeAddressCode Get (Just x) (Just y) (Just i)
-- ThreeAddressCode Set (Just x) (Just i) (Just y)
-- No considerados:
-- ThreeAddressCode Anexo (Just x) (Just y) (Just z)
-- ThreeAddressCode Concat (Just x) (Just y) (Just z)
-- ThreeAddressCode Access (Just x) (Just r) (Just f)
genThreeOperandsOp :: TAC -> InterfGraph -> I.IntMap Int -> String -> IO ()
genThreeOperandsOp tac i@(_, _, t) color name = do
  let
    inst = show' (tacOperand tac) ++ " "
    dest = (makeReg color $ getTempNum t $ tacInfo $ tacLvalue tac) ++ ", "
  case tacOperand tac of
    x | x `elem` [Add, Sub, Mult, Div, Mod] -> let
      reg1 = (makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac) ++ ", "
      in case tacInfo $ tacRvalue2 tac of
        Nothing ->
          let reg2 = show (fromJust $ tacRvalue2 tac) ++ "\n"
          in if isFloat $ tacType $ tacLvalue tac then do
            let
              save = "sw $v0, -4($sp)\nsw $v1, -8($sp)\naddi $sp, $sp, -8\n"
              mfc1 = "mfc1.d $v0, " ++ reg1 ++ "\n"
              rslt = "addi " ++ dest ++ "$v0, 0\n"
              load = "lw $v0, 4($sp)\nlw $v1, 8($sp)\naddi $sp, $sp, 8\n"
              code = save ++ inst ++ reg1 ++ reg1 ++ reg2 ++ mfc1 ++ rslt
            appendFile name $ code ++ load
          else
            let code = inst ++ dest ++ reg1 ++ reg2 
            in appendFile name code
        _ ->
          let reg2 = (makeReg color $ getTempNum t $ tacInfo $ tacRvalue2 tac)
          in appendFile name $ inst ++ dest ++ reg1 ++ reg2 ++ "\n"
    x | x `elem` [Gt, Gte, Lt, Lte, Eq, Neq] ->
      let reg2 = (tail . init $ show (fromJust $ tacRvalue2 tac)) ++ "\n"
          reg1 = if isJust $ tacInfo $ tacRvalue1 tac then
            (makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac) ++ ", "
          else show (fromJust $ tacRvalue1 tac) ++ ", "
      in appendFile name $ inst ++ dest ++ reg1 ++ reg2
    Call ->
      let save = "addi " ++ dest ++ "$v0, 0\n"
      in genJumps tac i color name >> putStrLn (show $ t $ fromJust $ tacInfo $ tacLvalue tac) >> appendFile name save
    Get -> do-- x = y[i]
      let
        reg1 = (makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac)
        y = reg1 ++ ", "
        reg2 = (makeReg color $ getTempNum t $ tacInfo $ tacRvalue2 tac)
        code = "add " ++ y ++ y ++ reg2 ++ "\nlw " ++ dest ++ reg1
      appendFile name code
    Set -> do -- x[i] = y
      let
        reg1 = (makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac)
        reg2 = (makeReg color $ getTempNum t $ tacInfo $ tacRvalue2 tac)
        code = "add " ++ dest ++ dest ++ reg1 ++ "\nsw " ++ dest ++ "0("
      appendFile name $ code ++ init reg2 ++ ")\n"
    _ -> appendFile name ""


-- ThreeAddressCode Minus (Just x) (Just y) Nothing
-- ThreeAddressCode Ref (Just x) (Just y) Nothing
-- ThreeAddressCode Deref (Just x) (Just y) Nothing
genTwoOperandsOp :: TAC -> InterfGraph -> VertColorMap -> String -> IO ()
genTwoOperandsOp tac (_, _, getReg) color file =
  let
    inst = show' (tacOperand tac) ++ " "
    dest = (makeReg color $ getTempNum getReg $ tacInfo $ tacLvalue tac) ++ ", "
  in 
  case tacInfo $ tacRvalue1 tac of
    Nothing -> -- No puedes tener un label en estos operadores.
      let label = show (fromJust $ tacRvalue1 tac) ++ "\n"
      in appendFile file $ inst ++ dest ++  label
    _ -> let
      reg1 = (makeReg color $ getTempNum getReg $ tacInfo $ tacRvalue1 tac) ++ "\n"
      code = inst ++ dest ++ reg1
      in appendFile file code


-- ThreeAddressCode Return Nothing (Just x) Nothing
-- ThreeAddressCode Return Nothing Nothing NothinggetTempNum t 
-- ThreeAddressCode Call Nothing (Just f) (Just n)
-- ThreeAddressCode If Nothing (Just b) (Just label)
-- ThreeAddressCode GoTo Nothing Nothing (Just label)
genJumps :: TAC -> InterfGraph -> VertColorMap -> String -> IO ()
genJumps tac (_, _, t) color name = case tacRvalue2 tac of
  Nothing -> case tacRvalue1 tac of
    Nothing -> epilogue name -- Return Proc
    _ -> case tacInfo $ tacRvalue1 tac of -- Return Func
      Nothing -> --return a constant
        let code = "addi $v0, $0, " ++ show (fromJust $ tacRvalue1 tac) ++ "\n"
        in appendFile name code >> epilogue name
      _ -> --return a value into a register
        let
          dest = makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac
          code = "addi $v0, " ++ dest ++ ", 0\n"
        in appendFile name code >> epilogue name
  _ -> do
    let
      goto = show' (tacOperand tac) ++ " "
      dest = (tail . init $ show (fromJust $ tacRvalue2 tac)) ++ "\n"
    if isJust $ tacRvalue1 tac then
      if isCall $ tacOperand tac then -- Call Subroutines
        let code = goto ++ show (fromJust $ tacRvalue1 tac) ++ "\n"
        in activateCalled name >> appendFile name code >> activateCaller name
      else
        let cond = makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac
        in appendFile name $ goto ++ cond ++ ", " ++ dest -- If
    else appendFile name $ goto ++ dest -- GoTo

-- ThreeAddressCode Print Nothing (Just e) Nothing
-- ThreeAddressCode Read Nothing Nothing Nothing
-- ThreeAddressCode Assign Nothing (Just x) Nothing
-- ThreeAddressCode Exit Nothing Nothing Nothing
genSyscalls :: TAC -> InterfGraph -> VertColorMap -> String -> IO ()
genSyscalls tac (_, _, t) color file = case tacOperand tac of
  Print -> -- syscalls 1,3,4,11
    {- 
    * code in $v0 |    service   | args
    *      1      | int          | $a0
    *      2      | float        | $f12
    *      3      | double       | $f12
    *      4      | string       | $a0
    *      11     | char         | $a0. Prints ASCII char corresponding to contents of low-order byte
    *      34     | hex int      | $a0
    *      35     | binary int   | $a0
    *      36     | unsigned int | $a0

      li $v0, 4
      la $a0, strLabel
      syscall
    -}
    if isJust $ tacInfo $ tacRvalue2 tac then
      putStrLn "HOLA" >>
      let arg = makeReg color $ getTempNum t $ tacInfo $ tacRvalue2 tac
      in appendFile file $ "li $v0, 1\naddi $a0, " ++ arg ++ ", 0\nsyscall\n"
    else
      let label = show (fromJust $ tacRvalue2 tac) ++ "\nsyscall\n"
      in 
        case strSplit "_" $ show $ fromJust $ tacRvalue2 tac of
          (_, "int") -> appendFile file $ "li $v0, 1\nlw $a0, " ++ label
          (_, "float") -> appendFile file $ "li $v0, 3\nl.d $f12, " ++ label
          -- (_, "str") -> appendFile file $ "li $v0, 4\nla $a0, " ++ label
          _ -> appendFile file $ "li $v0, 4\nla $a0, " ++ label
          -- t -> putStrLn $ "Print " ++ fst t ++ "_" ++ snd t
  
  Read -> -- syscalls 5,7,8,12
    {- 
    * code in $v0 |    service   | arg | result
    *      5      | int          |     | $v0 
    *      6      | float        |     | $f0
    *      7      | double       |     | $f0
    *      12     | char         |     | $v0
    *      8      | string       | $a0, input buffer addr     | Follows semantics of UNIX 'fgets'. For specified length n, string can be no longer than n-1. 
    *             |              | $a1, max num chars to read | If less than that, adds newline to end. In either case, then pads with null byte If n = 1, input is ignored and null byte placed at buffer address. If n < 1, input is ignored and nothing is written to the buffer.
    *      14     | read from file
    * Null terminated strings
    *      51     |
    *      52     |
    *      53     |
    *      54     |

      li $v0, 4
      la $a0, strLabel
      syscall
    -}
    let label = show (fromJust $ tacRvalue2 tac)
    in case strSplit "_" $ show $ fromJust $ tacRvalue2 tac of
      (_, "int") -> let code = "li $v0, 5\nsyscall\nsw $v0, " ++ label ++ "\n"
        in appendFile file code
      (_, "float") -> let code = "li $v0, 7\nsyscall\nswl $f0, " ++ label
        in appendFile file $ code ++ "\nswr $f0, " ++ label ++ "\n"
      (_, "str") -> let
          len  = "li $a1, len" ++ show (fromJust $ tacRvalue2 tac) ++ "\n"
          code = "la $a0, " ++ label ++ "\n" ++ len ++ "li $v0, 8\nsyscall\n"
        in appendFile file code
      t -> putStrLn $ "Read " ++ fst t ++ "_" ++ snd t
      
  Exit -> appendFile file "li $v0, 10\nsyscall\n"
  _ -> -- syscall 9
  {-
   * sbrk (allocate heap memory) | $a0 = number of bytes to allocate | $v0 contains address of allocated memory 
   ##### Crear jugadores
    # $t0: 	Jugador actual.
    # $t1:	Numero de jugadores
    # $t2:	Nombre del jugador
    #####
      # Creo al jugador 1
        li $v0, 9
        li $a0, 24
        syscall
        
        sw $v0, jugadores	# Enlazo el jugador 1 a la lista y se establece como el primero de la lista
                #( no necesariamente es el primero que va a jugar)
        move $t0, $v0		# Actualizo el jugador actual con el primero insertado
        li $t1, 0		# Inicializo el numero de jugadores
  -}
    let arg = makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac
    in appendFile file $ "addi $a0, " ++ arg ++ ", 0\nli $v0, 9\nsyscall\n"

-- ThreeAddressCode Assign (Just x) (Just y) Nothing
-- ThreeAddressCode Assign (Just x) Nothing Nothing
genAssign :: TAC -> InterfGraph -> VertColorMap -> String -> IO ()
genAssign tac (_, _, t) color name =
  if isJust $ tacInfo $ tacLvalue tac then
    let dest = makeReg color $ getTempNum t $ tacInfo $ tacLvalue tac
    in case tacRvalue1 tac of
      Nothing ->
        let code = "li " ++ dest ++ ", 0\n"
        in appendFile name code
      _ ->
        if isJust $ tacInfo $ tacRvalue1 tac then do
          let
            reg1 = makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac
            code = "addi " ++ dest ++ ", " ++ reg1 ++ ", 0\n"
          putStrLn reg1
          appendFile name code
        else 
          let immediate = show (fromJust $ tacRvalue1 tac) ++ "\n"
          in appendFile name $ "li " ++ dest ++ ", " ++ immediate
  else
    if isJust $ tacInfo $ tacRvalue1 tac then
      let value = makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac
          save = "sw " ++ value ++ ", " ++ (show $ fromJust $ tacLvalue tac)
      in appendFile name $ save ++ "\n"
    else
      let value = show (fromJust $ tacRvalue1 tac)
          save = "sw " ++ value ++ ", " ++ (show $ fromJust $ tacLvalue tac)
      in appendFile name $ save ++ "\n"

-- ThreeAddressCode Param Nothing (Just p) (Just n)
genParam :: TAC -> InterfGraph -> VertColorMap -> String -> IO ()
genParam tac (_, _, t) color name =
  if isJust $ tacRvalue2 tac then
    let p = (makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac) ++ ", 0"
    in case show (tacRvalue2 tac) of
      "Just 0" -> appendFile name $ "ori $a0, " ++ p ++ "\n"
      "Just 1" -> appendFile name $ "ori $a1, " ++ p ++ "\n"
      "Just 2" -> appendFile name $ "ori $a2, " ++ p ++ "\n"
      "Just 3" -> appendFile name $ "ori $a3, " ++ p ++ "\n"
      _ -> appendFile name $ "sw " ++ p ++ "($sp)\naddi $sp, $sp, -4\n"
  else
    appendFile name "" -- Imposible llegar a este punto

activateCaller :: String -> IO ()
activateCaller name = appendFile name (ac1 ++ ac2 ++ ac3 ++ ac4 ++ ac5) where
  ac1 = "addi $sp, $sp, 56\nlw $a0, 0($sp)\nlw $a1, -4($sp)\n"
  ac2 = "lw $a2, -8($sp)\nlw $a3, -12($sp)\nlw $t0, -16($sp)\n"
  ac3 = "lw $t1, -20($sp)\nlw $t2, -24($sp)\nlw $t3, -28($sp)\n"
  ac4 = "lw $t4, -32($sp)\nlw $t5, -36($sp)\nlw $t6, -40($sp)\n"
  ac5 = "lw $t7, -44($sp)\nlw $t8, -48($sp)\nlw $t9, -52($sp)\n"

prologue :: String -> IO ()
prologue name = appendFile name (pro0 ++ pro1 ++ pro2 ++ mvsp) where
  pro0 = "sw $ra, 0($sp)\nsw $s0, -4($sp)\nsw $s1, -8($sp)\nsw $s2, -12($sp)\n"
  pro1 = "sw $s3, -16($sp)\nsw $s4, -20($sp)\nsw $s5, -24($sp)\n"
  pro2 = "sw $s6, -28($sp)\nsw $s7, -32($sp)\nsw $fp, -36($sp)\n"
  mvsp = "addi $sp, $sp, -40\n"

activateCalled :: String -> IO ()
activateCalled name = appendFile name (ac1 ++ ac2 ++ ac3 ++ ac4 ++ ac5) where
  ac1 = "sw $a0, 0($sp)\nsw $a1, -4($sp)\nsw $a2, -8($sp)\nsw $a3, -12($sp)\n"
  ac2 = "sw $t0, -16($sp)\nsw $t1, -20($sp)\nsw $t2, -24($sp)\n"
  ac3 = "sw $t3, -28($sp)\nsw $t4, -32($sp)\nsw $t5, -36($sp)\n"
  ac4 = "sw $t6, -40($sp)\nsw $t7, -44($sp)\nsw $t8, -48($sp)\n"
  ac5 = "sw $t9, -52($sp)\naddi $sp, $sp, -56\n"

epilogue :: String -> IO ()
epilogue name = appendFile name (mvsp ++ epi0 ++ epi1 ++ epi2) where
  mvsp = "addi $sp, $sp, 40\nlw $ra, 0($sp)\nlw $s0, -4($sp)\n"
  epi0 = "lw $s1, -8($sp)\nlw $s2, -12($sp)\nlw $s3, -16($sp)\n"
  epi1 = "lw $s4, -20($sp)\nlw $s5, -24($sp)\nlw $s6, -28($sp)\n"
  epi2 = "lw $s7, -32($sp)\nlw $fp, -36($sp)\njr $ra\n"

---- Auxs

-- | Pone el nombre de los regs
makeReg :: VertColorMap -> Int -> String
makeReg colorGraph number = 
  let n = fromJust $ I.lookup number colorGraph
  in if n > 25 then "$f" ++ show (2 * n - 50) else "$" ++ show n

-- | Ayuda a encontrar la clave para moverse en el mapa (grafo dsatur)
-- TODO!!: a veces da error con fromJust de Nothing
getTempNum :: (TACInfo -> Maybe G.Vertex) -> Maybe TACInfo -> G.Vertex
getTempNum _ Nothing = error "tacInfo is Nothing getTempNum"
getTempNum getReg tacInfo = 
  if mVertex == Nothing then error "vertex is Nothing getTempNum"
  else fromJust mVertex
  where
    mVertex = getReg $ fromJust $ tacInfo

-- |
tacInfo :: TACOP -> Maybe TACInfo
tacInfo (Just (Id tac)) = Just tac
-- Constant (String, Type) | 
-- Label String
tacInfo _ = Nothing

-- |
tacType :: TACOP -> Maybe Type
tacType (Just (Constant (_, t))) = Just t
tacType (Just (Id (TACVar s _))) = Just $ symType s
tacType (Just (Id (Temp _ t _))) = Just t
tacType _ = Nothing

-- |
isFloat :: Maybe Type -> Bool
isFloat (Just TFloat) = True
isFloat _ = False

-- |
isCall :: Operation -> Bool
isCall Call = True
isCall _    = False

show' :: Operation -> String
show' Add = "add"
show' Sub = "sub"
show' Minus = "neg"
show' Mult = "mul"
show' Div = "div"
show' Mod = "rem"
show' And = "and"
show' Or = "or"
show' Gt = "bgt"
show' Gte = "bge"
show' Lt = "blt"
show' Lte = "ble"
show' Eq = "beq"
show' Neq = "bne"
show' GoTo = "b"
show' If = "bnez"
show' Call = "jal"
show' Return = "jr"
show' Get = "lw"
show' Set = "sw"
show' Ref = "la"
show' Deref = "lw"
show' _ = "NM"
