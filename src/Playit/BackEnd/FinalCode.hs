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
genFinalCode tac g c file    = do
  let 
    tacInstr = head tac
    tacNext  = tail tac
  
  case tacOperand tacInstr of
    -- Faltan And, Or
    -- Not supported Anexo, Concat
    x | x `elem` [Add, Sub, Mult, Div, Mod, Gt, Gte, Lt, Lte, Eq, Neq, Get, Set, Call] ->
      
      -- liftIO (print (show tacInstr ++ " tac: " ++ show (tacInfo $ tacRvalue2 tacInstr))) >> 
      comment ("\n\t\t# 3 Operands operation: " ++ show tacInstr) file >>
      genThreeOperandsOp tacInstr g c file

    x | x `elem` [Minus, Ref, Deref] -> genTwoOperandsOp tacInstr g c file
    x | x `elem` [Return, If, GoTo]  -> genJumps tacInstr g c file        
    x | x `elem` [Print, Read, Exit] -> genSyscalls tacInstr g c file     
    
    NewLabel ->
      let lbl = tail . init . reverse . snd . splitAt 2 . reverse $ show tacInstr
      in 
      if elem "l." $ subsequences $ show tacInstr then
          appendFile file ("\n" ++ lbl ++ ":")
      else do
        appendFile file ("\n" ++ show tacInstr ++ ":")
        if "main: " == show tacInstr then return ()
        else prologue file
    
    -- Length -> genLength tacInstr -- busca el label en .data que corresponda, si x = # [1,2] -> en TAC guardar el array en .data y colocarle un nombre y tu tam
    Assign -> genAssign tacInstr g c file
    Param  -> genParam tacInstr g c file 
    -- Access
    o -> comment ("\n\t\t# Operand not supported yet: " ++ show o) file
  
  genFinalCode tacNext g c file


-- TODO!!: Chars
genAssign :: TAC -> InterfGraph -> VertColorMap -> String -> IO ()
genAssign tac (_, _, getReg) colorGraph file = do
  comment "\n\t\t# Assign" file
  let
    lval     = tacLvalue tac
    lvalInfo = tacInfo lval
    rv1      = tacRvalue1 tac
    rv1Info  = tacInfo rv1
  
  if isJust lvalInfo then
    let 
      dest = makeReg colorGraph $ getReg' getReg lvalInfo
    in 
    case rv1 of
      Nothing -> comment (", rv1 is Nothing ") file >> li dest "0" file
      _       ->
        if isJust rv1Info then
          let
            reg1 = makeReg colorGraph $ getReg' getReg rv1Info
            mov  = if isFloat $ tacType lval then mov_d else move
          in
          comment (", var: " ++ show rv1) file >> mov dest reg1 file

        else 
          let imm  = show (fromJust rv1)
              load = if elem '_' imm then l_d else li
          in 
            comment (", const: " ++ show rv1) file >> load dest imm file
  else
    -- cuando se refiere a asignar un valor en el .data
    -- NO, innecesario,
    comment ", store in mem" file >>
    if isJust rv1Info then
      let 
        value = makeReg colorGraph $ getReg' getReg rv1Info
        store = if isFloat $ tacType rv1 then s_d else sw
      in
        store value (show $ fromJust lval) file
    else
      let value = show (fromJust rv1)
      in sw value (show $ fromJust lval) file


-- TODO:
-- ThreeAddressCode Anexo (Just x) (Just y) (Just z)
-- ThreeAddressCode Concat (Just x) (Just y) (Just z)
-- ThreeAddressCode Access (Just x) (Just r) (Just f)
genThreeOperandsOp :: TAC -> InterfGraph -> I.IntMap Int -> String -> IO ()
genThreeOperandsOp tac i@(_, _, t) color file =
  let
    inst = show' (tacOperand tac) ++ " "
    dest = (makeReg color $ getReg' t $ tacInfo $ tacLvalue tac) ++ ", "
  in
  case tacOperand tac of
    x | x `elem` [Add, Sub, Mult, Div, Mod] -> do
      comment (", " ++ show x) file
      let
        reg1 = (makeReg color $ getReg' t $ tacInfo $ tacRvalue1 tac) ++ ", "
      
      -- TODO: Primero se deberia verificar si es float o no
      case tacInfo $ tacRvalue2 tac of
        Nothing ->
          -- TODO: cuando es float la inst no es la correcta
          let code = inst ++ dest ++ reg1 ++ show (fromJust $ tacRvalue2 tac) 
          in comment ", const" file >> appendFile file code
        _ ->
          let 
            reg2 = (makeReg color $ getReg' t $ tacInfo $ tacRvalue2 tac)
          in 
          comment ", var" file >>
          if isFloat $ tacType $ tacLvalue tac then do
            comment ", Float" file

            if x == Div && not (isFloat $ tacType $ tacRvalue1 tac) then do
              let 
                next1 = '$':(show $ 1 + (read (init (init (tail reg1))) :: Int))
                next2 = '$':(show $ 1 + (read (tail reg2) :: Int))
              
              comment ", div (/)" file
              floatingDiv file
              s_d    "$f10" "0($sp)"    file
              s_d    "$f12" "-8($sp)"   file
              sw     next1 "-16($sp)"   file
              addi   "$sp" "$sp" "-20"  file
              li     next1 "0"          file
              mtc1_d reg1 "$f12"        file
              lw     next1 "4($sp)"     file
              sw     next2 "4($sp)"     file
              li     next2 "0"          file
              mtc1_d reg2 "$f10"        file
              div_d  dest "$f12" "$f10" file
              addi   "$sp" "$sp" "20"   file
              lw     next2 "-16($sp)"   file
              l_d    "$f10" "0($sp)"    file
              l_d    "$f12" "-8($sp)"   file
            else
              let code = (init inst) ++ ".d " ++ dest ++ reg1 ++ reg2
              in
              comment ", op != Div or rv2 is float. Se asume es float" file >>
                appendFile file code
          else 
            comment ", Int" >> appendFile file $ inst ++ dest ++ reg1 ++ reg2

    x | x `elem` [Gt, Gte, Lt, Lte, Eq, Neq] ->
      let 
        reg2 = (tail . init $ show (fromJust $ tacRvalue2 tac))
        reg1 = 
          if isJust $ tacInfo $ tacRvalue1 tac then
            (makeReg color $ getReg' t $ tacInfo $ tacRvalue1 tac) ++ ", "
          else 
            show (fromJust $ tacRvalue1 tac) ++ ", "
      in 
        comment (", " ++ show x) file >>
          appendFile file (inst ++ dest ++ reg1 ++ reg2)
    Call ->
      comment ", Subroutine call" file >>
        genJumps tac i color file >> addi dest "$v0" "0" file
    Get -> -- x = y[i]
      let
        reg1 = (makeReg color $ getReg' t $ tacInfo $ tacRvalue1 tac)
        reg2 = (makeReg color $ getReg' t $ tacInfo $ tacRvalue2 tac)
      in
        comment ", Get" file >> add reg1 reg1 reg2 file >> lw  dest reg1 file
    Set -> -- x[i] = y
      let
        reg1 = (makeReg color $ getReg' t $ tacInfo $ tacRvalue1 tac)
        reg2 = (makeReg color $ getReg' t $ tacInfo $ tacRvalue2 tac)
      in
        comment ", Set" file >>
          add dest dest reg1 file >> sw dest ("0(" ++ init reg2 ++ ")") file
    o -> 
      comment ("\n\t\t# Operand not supported yet: " ++ show o) file


-- TODO!!: Upper/Lower case, "tacInfo" es Nothing
genTwoOperandsOp :: TAC -> InterfGraph -> VertColorMap -> String -> IO ()
genTwoOperandsOp tac (_, _, getReg) color file = do
  comment ("\n\t\t# 2 Operands operation: " ++ show tac) file
  let
    inst = show' (tacOperand tac) ++ " "
    dest = (makeReg color $ getReg' getReg $ tacInfo $ tacLvalue tac) ++ ", "
  
  -- TODO: Primero se deberia verificar si es float o no
  case tacInfo $ tacRvalue1 tac of
    Nothing ->
      -- TODO: cuando es float la inst no es la correcta
      let label = show (fromJust $ tacRvalue1 tac)
      in comment ", const" file >> appendFile file (inst ++ dest ++  label)
    _ -> 
      let
        reg1 = (makeReg color $ getReg' getReg $ tacInfo $ tacRvalue1 tac) 
        float = isFloat $ tacType $ tacRvalue1 tac
        inst' = if float then (init inst) ++ ".d " else inst
        code = inst' ++ dest ++ reg1
      in 
        appendFile file code


genJumps :: TAC -> InterfGraph -> VertColorMap -> String -> IO ()
genJumps tac (_, _, t) color file = do
  comment "\n\t\t# Jump" file
  case tacRvalue2 tac of
    Nothing -> 
      case tacRvalue1 tac of
        Nothing -> comment ", return from Proc" file >> epilogue file
        _       -> comment ", return from Func" file >> 
          case tacInfo $ tacRvalue1 tac of
            --return a constant
            Nothing -> 
              let retVal = show (fromJust $ tacRvalue1 tac)
              in comment ", return constant" file >> move "$v0" retVal file >> epilogue file
            --return a value into a register
            _ -> 
              let retVal = makeReg color $ getReg' t $ tacInfo $ tacRvalue1 tac
              in comment ", return varule" file >> move "$v0" retVal file >> epilogue file
    _ -> do
      let
        goto = show' (tacOperand tac) ++ " "
        dest = (tail . init $ show (fromJust $ tacRvalue2 tac))
      if isJust $ tacRvalue1 tac then
        if isCall $ tacOperand tac then
          let code = goto ++ show (fromJust $ tacRvalue1 tac)
          in 
            comment ", Call Subroutines" file >> activateCalled file >> 
              appendFile file code >> activateCaller file
        else
          let cond = makeReg color $ getReg' t $ tacInfo $ tacRvalue1 tac
          in comment ", if" file >> appendFile file (goto ++ cond ++ ", " ++ dest)
      else 
        comment ", goto" file >> appendFile file (goto ++ dest)


genParam :: TAC -> InterfGraph -> VertColorMap -> String -> IO ()
genParam tac (_, _, getReg) colorGraph file = 
  case tacLvalue tac of
    Nothing ->
      let 
        param    = tacRvalue1 tac
        paramOp   = tacInfo param
        Constant (paramNum, _) = fromJust $ tacRvalue2 tac
        regSour  = 
          if isJust paramOp then makeReg colorGraph (getReg' getReg $ tacInfo param)
          else show $ fromJust param
      in
      comment ("\n\t\t# Param: " ++ show tac) file >>

        if isFloat $ tacType param then 
          if paramNum < "4" then mov_d ("$f" ++ show (2 * (read paramNum :: Int) - 50)) regSour file
          else
            -- offset
            comment ", to stack" file >> addi "$sp" "$sp" "-8" file >> sw regSour "($sp)" file
        else
          if paramNum < "4" then move ("$a" ++ paramNum) regSour file
          else
            -- offset
            comment ", to stack" file >> addi "$sp" "$sp" "-4" file >> sw regSour "($sp)" file

    _ -> -- Param actual pos formal
      let 
        param = show $ fromJust $ tacRvalue1 tac
        arg   = makeReg colorGraph $ getReg' getReg $ tacInfo $ tacRvalue2 tac
      in 
      if isFloat $ tacType $ tacLvalue tac then 
        mov_d arg ("$f" ++ param) file
      else
        move arg ("$a" ++ param) file


genSyscalls :: TAC -> InterfGraph -> VertColorMap -> String -> IO ()
genSyscalls tac (_, _, t) color file = 
  let
    rv2 = tacRvalue2 tac
  in
  case tacOperand tac of
    Print -> do
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
      -}
      comment "\n\t\t# Print" file

      if isJust $ tacInfo rv2 then do
        comment (", Int, rv2: " ++ show rv2) file
        let arg = makeReg color $ getReg' t $ tacInfo rv2
        li   "$v0" "1" file
        move "$a0" arg file
        syscall        file
      else do
        let label = show (fromJust rv2)

        case strSplit "_" label of
          (_, "int")   -> comment ", Int" file >> li "$v0" "1" file >> lw  "$a0"  label file
          (_, "float") -> comment ", Double" file >> li "$v0" "3" file >> l_d "$f12" label file
          _            -> comment ", String" file >> li "$v0" "4" file >> la  "$a0"  label file

        syscall file
        li "$v0" "4"       file
        la "$a0" "newLine" file
        syscall file

    Read -> -- syscalls 5,7,8,12
      {- 
        * code in $v0 |    service   | arg | result
        *      5      | int          |     | $v0 
        *      6      | float        |     | $f0
        *      7      | double       |     | $f0
        *      12     | char         |     | $v0
        *      8      | string       | $a0, input buffer addr     |
        *             |              | $a1, max num chars to read |
                Follows semantics of UNIX 'fgets'. For specified length n, string 
                can be no longer than n-1, if less than that, adds newline to end. 
                In either case, then pads with null byte If n = 1, input is ignored 
                and null byte placed at buffer address. If n < 1, input is ignored 
                and nothing is written to the buffer.
        
        *      14     | read from file
        * Null terminated strings
        *      51     |
        *      52     |
        *      53     |
        *      54     |

          la $s0, <label donde se guarda el valor del input>
          
          li $v0, 8
          la $a0, buffer	# Load byte space into address, donde se guarda el input
          li $a1, 12 	    # allot the byte space for string
          syscall

          # Guardo el input
          lw $t0, ($a0)	
          sw $t0, 0($s0)
      -}
      let label = show (fromJust rv2)
      in 
      comment "\n\t\t# Read" file >>
        case strSplit "_" label of
          
          (_, "int") -> comment ", Int" file >> 
            li "$v0" "5" file >> syscall file >> sw "$v0" label file

          (_, "float") -> do
            comment ", Float" file
            li "$v0" "7" file 
            syscall file
            swl "$f0" label file 
            swr "$f0" label file
          
          (_, "str") -> do
            comment ", String" file
            la "$a0" label file
            li "$a1" ("len" ++ label) file
            li "$v0" "8" file
            syscall file

          t -> comment (", ?, " ++ show tac) file
        
    Exit -> comment "\n\t\t# Exit" file >> li "$v0" "10" file >> syscall file
    _    -> -- syscall 9
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
      let arg = makeReg color $ getReg' t $ tacInfo $ tacRvalue1 tac
      in comment "\n\t\t# Ask for memory" file >>
        move "$a0" arg file >> li "$v0" "9" file >> syscall file


-- | Empila los registros que son responsabilidad del llamado.
-- * Antes de pasar control a la subrutina
-- 
activateCalled :: String -> IO ()
activateCalled file = do
  comment "\n\t\t# Activate Called" file
  sw   "$a0" "0($sp)" file
  sw   "$a1" "-4($sp)" file
  sw   "$a2" "-8($sp)" file
  sw   "$a3" "-12($sp)" file
  sw   "$t0" "-16($sp)" file
  sw   "$t1" "-20($sp)" file
  sw   "$t2" "-24($sp)" file
  sw   "$t3" "-28($sp)" file
  sw   "$t4" "-32($sp)" file
  sw   "$t5" "-36($sp)" file
  sw   "$t6" "-40($sp)" file
  sw   "$t7" "-44($sp)" file
  sw   "$t8" "-48($sp)" file
  sw   "$t9" "-52($sp)" file
  s_d  "$f0" "-60($sp)" file
  s_d  "$f2" "-68($sp)" file
  s_d  "$f4" "-76($sp)" file
  s_d  "$f6" "-84($sp)" file
  addi "$sp" "$sp" "-84" file

-- | Empila los registros que son responsabilidad del llamador.
-- * Antes de ejecutar subrutina
-- 
prologue :: String -> IO ()
prologue file = do
  comment "\n\t\t# Prologue" file
  sw   "$ra" "0($sp)" file
  sw   "$s0" "-4($sp)" file
  sw   "$s1" "-8($sp)" file
  sw   "$s2" "-12($sp)" file
  sw   "$s3" "-16($sp)" file
  sw   "$s4" "-20($sp)" file
  sw   "$s5" "-24($sp)" file
  sw   "$s6" "-28($sp)" file
  sw   "$s7" "-32($sp)" file
  sw   "$fp" "-36($sp)" file
  addi "$sp" "$sp" "-40" file


-- | Desempila los registros que son responsabilidad del llamado.
-- * Antes de regresar control
-- 
epilogue :: String -> IO ()
epilogue file = do
  comment "\n\t\t# Epilogue" file
  addi "$sp" "$sp" "40" file
  lw   "$ra" "0($sp)" file
  lw   "$s0" "-4($sp)" file
  lw   "$s1" "-8($sp)" file
  lw   "$s2" "-12($sp)" file
  lw   "$s3" "-16($sp)" file
  lw   "$s4" "-20($sp)" file
  lw   "$s5" "-24($sp)" file
  lw   "$s6" "-28($sp)" file
  lw   "$s7" "-32($sp)" file
  lw   "$fp" "-36($sp)" file
  jr   "$ra" file

-- | Desempila los registros que son responsabilidad del llamador
-- * Despues de regresar de la subrutina
-- 
activateCaller :: String -> IO ()
activateCaller file = do
  comment "\n\t\t# Activate Caller" file
  addi "$sp" "$sp" "84" file
  lw   "$a0" "0($sp)" file
  lw   "$a1" "-4($sp)" file
  lw   "$a2" "-8($sp)" file
  lw   "$a3" "-12($sp)" file
  lw   "$t0" "-16($sp)" file
  lw   "$t1" "-20($sp)" file
  lw   "$t2" "-24($sp)" file
  lw   "$t3" "-28($sp)" file
  lw   "$t4" "-32($sp)" file
  lw   "$t5" "-36($sp)" file
  lw   "$t6" "-40($sp)" file
  lw   "$t7" "-44($sp)" file
  lw   "$t8" "-48($sp)" file
  lw   "$t9" "-52($sp)" file
  l_d  "$f0" "-60($sp)" file
  l_d  "$f2" "-68($sp)" file
  l_d  "$f4" "-76($sp)" file
  l_d  "$f6" "-84($sp)" file


---- Auxs

-- | Pone el nombre de los regs
makeReg :: VertColorMap -> Int -> String
makeReg colorGraph number = 
  let n = fromJust $ I.lookup number colorGraph
  in if n > 25 then "$f" ++ show (2 * n - 50) else "$" ++ show n

-- | Get reg (vertex/node) from color DSatur graph
-- 
-- TODO!!: a veces da error con fromJust de Nothing
getReg' :: (TACInfo -> Maybe G.Vertex) -> Maybe TACInfo -> G.Vertex
getReg' _      Nothing = error "\n\t'tacInfo' is Nothing getReg'\n"
getReg' getReg tacInfo = 
  if mVertex == Nothing 
  then error $ "\n\t'vertex' is Nothing getReg'\n\ttacInfo: " ++ show tacInfo ++ "\n"
  else fromJust mVertex
  where
    mVertex = getReg $ fromJust tacInfo

-- | Get the tac temp where's stored the operand
-- 
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
show' Add    = "\n\t\tadd"
show' Sub    = "\n\t\tsub"
show' Minus  = "\n\t\tneg"
show' Mult   = "\n\t\tmul"
show' Div    = "\n\t\tdiv"
show' Mod    = "\n\t\trem"
show' And    = "\n\t\tand"
show' Or     = "\n\t\tor"
show' Gt     = "\n\t\tbgt"
show' Gte    = "\n\t\tbge"
show' Lt     = "\n\t\tblt"
show' Lte    = "\n\t\tble"
show' Eq     = "\n\t\tbeq"
show' Neq    = "\n\t\tbne"
show' GoTo   = "\n\t\tb"
show' If     = "\n\t\tbnez"
show' Call   = "\n\t\tjal"
show' Return = "\n\t\tjr"
show' Get    = "\n\t\tlw"
show' Set    = "\n\t\tsw"
show' Ref    = "\n\t\tla"
show' Deref  = "\n\t\tlw"
show' _      = "\n\t\tNM"

floatingDiv :: String -> IO ()
floatingDiv file = do
  sw   "$t0" "0($sp)"    file
  sw   "$t1" "-4($sp)"   file
  add  "$t0" "$0" "$sp"  file
  addi "$t1" "$0" "8"    file
  div' "$t0" "$t0" "$t1" file
  mfhi "$t0"             file
  beqz "$t0" "aligned"   file
  lw   "$t0" "0($sp)"    file
  lw   "$t1" "-4($sp)"   file
  addi "$sp" "$sp" "-4"  file
  b    "ready"           file
  appendFile file "\naligned:"
  lw   "$t0" "0($sp)"    file
  lw   "$t1" "-4($sp)"   file
  appendFile file "\nready:"

