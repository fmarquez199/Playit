{- |
 * FinalCode
 *
 * Copyright : (c) 
 *  Francisco Javier 12-11163
 *  Natascha Gamboa  12-11250
-}
module Playit.BackEnd.FinalCode (genFinalCode) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe             (isJust, fromJust)
import Playit.BackEnd.RegAlloc.GraphColoring (VertColorMap)
import Playit.BackEnd.Types   (TAC, TACOP, TACInfo(..), InterfGraph)
import Playit.BackEnd.Utils   (tacNewLabel, tacLabel)
import Playit.FrontEnd.Types  (Type(..), symType)
import TACType               (ThreeAddressCode(..), Operation(..), Operand(..))
import qualified Data.Graph  as G
import qualified Data.IntMap as I

tacInfo :: TACOP -> Maybe TACInfo
tacInfo (Just (Id tac)) = Just tac
tacInfo _ = Nothing

tacType :: TACOP -> Maybe Type
tacType (Just (Constant (_, t))) = Just t
tacType (Just (Id (TACVar s _))) = Just $ symType s
tacType (Just (Id (Temp _ t _))) = Just t
tacType _ = Nothing

isFloat :: Maybe Type -> Bool
isFloat (Just TFloat) = True
isFloat _ = False

-- ThreeAddressCode NewLabel (Just l) Nothing Nothing
genFinalCode :: [TAC] -> InterfGraph -> I.IntMap Int -> String -> IO ()
genFinalCode tac g c n =
  if null tac then
    appendFile n ""
  else
    let h = head tac
    in case tacOperand h of
      x | x `elem` [Add, Sub, Mult, Div, Mod, Gt, Gte, Lt, Lte, Eq, Neq] ->
        genThreeOperandsOp h g c n >> genFinalCode (tail tac) g c n
      x | x `elem` [Minus, Length, Ref, Deref] ->
        genTwoOperandsOp h g c n >> genFinalCode (tail tac) g c n
      x | x `elem` [Return, Call, If, GoTo] ->
        genJumps h g c n >> genFinalCode (tail tac) g c n
      x | x `elem` [Print, Read] ->
        genSyscalls h g c n >> genFinalCode (tail tac) g c n
      Assign ->
        genAssign h g c n >> genFinalCode (tail tac) g c n
      NewLabel ->
        appendFile n (show h) >> genFinalCode (tail tac) g c n
      Param -> genParam h g c n >> genFinalCode (tail tac) g c n
      _ -> appendFile n "" >> genFinalCode (tail tac) g c n

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
    inst = show (tacOperand tac) ++ " "
    dest = (makeReg color $ getTempNum t $ tacInfo $ tacLvalue tac) ++ ", "
    reg1 = (makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac) ++ ", "
    reg2 = (makeReg color $ getTempNum t $ tacInfo $ tacRvalue2 tac) ++ "\n"
  case tacOperand tac of
    x | x `elem` [Add, Sub, Mult, Div, Mod] ->
      if isFloat $ tacType $ tacLvalue tac then do
        let
          save = "sw $v0, -4[$sp]\nsw $v1, -8[$sp]\naddi $sp, $sp, -8\n"
          mfc1 = "mfc1.d $v0, " ++ reg1 ++ "\n"
          rslt = "addi " ++ dest ++ "$v0, 0\n"
          load = "lw $v0, 4[$sp]\nlw $v1, 8[$sp]\naddi $sp, $sp, 8\n"
          code = save ++ inst ++ reg1 ++ reg1 ++ reg2 ++ mfc1 ++ rslt ++ load
        appendFile name code
      else
        let code = inst ++ dest ++ reg1 ++ reg2 
        in appendFile name code
    x | x `elem` [Gt, Gte, Lt, Lte, Eq, Neq] ->
      let code = inst ++ dest ++ reg1 ++ reg2 
      in appendFile name code
    Call ->
      let save = "addi " ++ dest ++ "$v0, 0\n"
      in genJumps tac i color name >> appendFile name save
    Get -> do-- x = y[i]
      let
        y = init $ init reg1
        code = "add " ++ reg1 ++ reg1 ++ reg2 ++ "\nlw " ++ dest ++ y
      appendFile name code
    Set -> do -- x[i] = y
      let
        y = init $ init reg1
        code = "add " ++ dest ++ dest ++ y ++ "\nsw " ++ dest ++ reg2
      appendFile name code
    _ -> appendFile name ""

makeReg :: I.IntMap Int -> Int -> String
makeReg color number = 
  let n = fromJust $ I.lookup number color
  in if n > 25 then "$f" ++ show (2 * n - 50) else "$" ++ show n

getTempNum :: (TACInfo -> Maybe G.Vertex) -> Maybe TACInfo -> Int
getTempNum t tacInfo = fromJust $ t $ fromJust $ tacInfo

-- ThreeAddressCode Minus (Just x) (Just y) Nothing
-- ThreeAddressCode Length (Just x) (Just y) _
-- ThreeAddressCode Ref (Just x) (Just y) Nothing
-- ThreeAddressCode Deref (Just x) (Just y) Nothing
genTwoOperandsOp :: TAC -> InterfGraph -> I.IntMap Int -> String -> IO ()
genTwoOperandsOp tac (_, _, t) color name = do
  let
    inst = show (tacOperand tac) ++ " "
    dest = (makeReg color $ getTempNum t $ tacInfo $ tacLvalue tac) ++ ", "
    reg1 = (makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac) ++ "\n"
    code = inst ++ dest ++ reg1
  appendFile name code

-- ThreeAddressCode Return Nothing (Just x) Nothing
-- ThreeAddressCode Return Nothing Nothing Nothing
-- ThreeAddressCode Call Nothing (Just f) (Just n)
-- ThreeAddressCode If Nothing (Just b) (Just label)
-- ThreeAddressCode GoTo Nothing Nothing (Just label)
genJumps :: TAC -> InterfGraph -> I.IntMap Int -> String -> IO ()
genJumps tac (_, _, t) color name = case tacRvalue2 tac of
  Nothing -> case tacRvalue1 tac of
    Nothing -> epilogue name >> activateCaller name -- Return Proc
    _ -> do -- Return Func
      let
        dest = makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac
        code = "addi $v0, " ++ dest ++ ", 0"
      appendFile name code >> epilogue name >> activateCaller name
  _ -> do
    let
      goto = show (tacOperand tac) ++ " "
      dest = show (fromJust $ tacRvalue2 tac) ++ "\n"
    if isJust $ tacRvalue1 tac then
      if isCall $ tacOperand tac then -- Call Subroutines
        let code = goto ++ show (fromJust $ tacRvalue1 tac) ++ "\n"
        in activateCalled name >> appendFile name code >> prologue name
      else
        let cond = makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac
        in appendFile name $ goto ++ cond ++ ", " ++ dest -- If
    else appendFile name $ goto ++ dest -- GoTo

epilogue :: String -> IO ()
epilogue name = appendFile name (mvsp ++ epi0 ++ epi1 ++ epi2) where
  mvsp = "addi $sp, $sp, 40\nlw $ra, 0($sp)\nlw $s0, -4($sp)\n"
  epi0 = "lw $s1, -8($sp)\nlw $s2, -12($sp)\nlw $s3, -16($sp)\n"
  epi1 = "lw $s4, -20($sp)\nlw $s5, -24($sp)\nlw $s6, -28($sp)\n"
  epi2 = "lw $s7, -32($sp)\nlw $fp, -36($sp)\njr $ra"

activateCaller :: String -> IO ()
activateCaller name = appendFile name (ac1 ++ ac2 ++ ac3 ++ ac4 ++ ac5) where
  ac1 = "addi $sp, $sp, 56\nlw $a0, 0($sp)\nlw $a1, -4($sp)\n"
  ac2 = "lw $a2, -8($sp)\nlw $a3, -12($sp)\nlw $t0, -16($sp)\n"
  ac3 = "lw $t1, -20($sp)\nlw $t2, -24($sp)\nlw $t3, -28($sp)\n"
  ac4 = "lw $t4, -32($sp)\nlw $t5, -36($sp)\nlw $t6, -40($sp)\n"
  ac5 = "lw $t7, -44($sp)\nlw $t8, -48($sp)\nlw $t9, -52($sp)\n"

isCall :: Operation -> Bool
isCall Call = True
isCall _ = False

activateCalled :: String -> IO ()
activateCalled name = appendFile name (ac1 ++ ac2 ++ ac3 ++ ac4 ++ ac5) where
  ac1 = "sw $a0, 0($sp)\nsw $a1, -4($sp)\nsw $a2, -8($sp)\nsw $a3, -12($sp)\n"
  ac2 = "sw $t0, -16($sp)\nsw $t1, -20($sp)\nsw $t2, -24($sp)\n"
  ac3 = "sw $t3, -28($sp)\nsw $t4, -32($sp)\nsw $t5, -36($sp)\n"
  ac4 = "sw $t6, -40($sp)\nsw $t7, -44($sp)\nsw $t8, -48($sp)\n"
  ac5 = "sw $t9, -52($sp)\naddi $sp, $sp, -56\n"

prologue :: String -> IO ()
prologue name = appendFile name (pro0 ++ pro1 ++ pro2 ++ mvsp) where
  pro0 = "sw $ra, 0($sp)\nsw $s0, -4($sp)\nsw $s1, -8($sp)\nsw $s2, -12($sp)\n"
  pro1 = "sw $s3, -16($sp)\nsw $s4, -20($sp)\nsw $s5, -24($sp)\n"
  pro2 = "sw $s6, -28($sp)\nsw $s7, -32($sp)\nsw $fp, -36($sp)\n"
  mvsp = "addi $sp, $sp, -40\n"

-- ThreeAddressCode Print Nothing (Just e) Nothing
-- ThreeAddressCode Read Nothing (Just e) Nothing
genSyscalls :: TAC -> InterfGraph -> I.IntMap Int -> String -> IO ()
genSyscalls tac (_, _, t) color name =
  let arg = makeReg color $ getTempNum t $ tacInfo $ tacRvalue1 tac
  in case tacOperand tac of
    Print -> -- syscalls 1,2,4,11
      case tacType $ tacRvalue1 tac of
        Just TInt ->
          appendFile name $ "addi $a0, " ++ arg ++ ", 0\nli $v0, 1\nsyscall\n"
        Just TFloat ->
          appendFile name $ "addi FP12, " ++ arg ++ ", 0\nli $v0, 2\nsyscall\n"
        Just TStr ->
          appendFile name $ "addi $a0, " ++ arg ++ ", 0\nli $v0, 4\nsyscall\n"
        Just TChar ->
          appendFile name $ "addi $a0, " ++ arg ++ ", 0\nli $v0, 11\nsyscall\n"
        _ -> appendFile name ""
    _ -> -- syscalls 5,6,8,12
      case tacType $ tacRvalue1 tac of
        Just TInt ->
          appendFile name $ "addi $a0, " ++ arg ++ ", 0\nli $v0, 5\nsyscall\n"
        Just TFloat ->
          appendFile name $ "addi FP12, " ++ arg ++ ", 0\nli $v0, 6\nsyscall\n"
        Just TStr ->
          let len = ", 0\naddi $a1, $zero, 80\nli $v0, 8\nsyscall\n"
          in appendFile name $ "addi $a0, " ++ arg ++ len
        Just TChar ->
          appendFile name $ "addi $a0, " ++ arg ++ ", 0\nli $v0, 12\nsyscall\n"
        _ -> appendFile name ""

-- ThreeAddressCode Assign (Just x) (Just y) Nothing
-- ThreeAddressCode Assign (Just x) Nothing Nothing
genAssign :: TAC -> InterfGraph -> I.IntMap Int -> String -> IO ()
genAssign tac (_, _, t) color name =
  let dest = makeReg color $ getTempNum t $ tacInfo $ tacLvalue tac
  in case tacRvalue1 tac of
    Nothing ->
      let code = "li " ++ dest ++ ", 0\n"
      in appendFile name code
    _ ->
      if isJust $ tacInfo $ tacRvalue1 tac then do
        let
          reg1 = show (fromJust $ t $ fromJust $ tacInfo $ tacRvalue1 tac)
          code = "addi " ++ dest ++ ", " ++ reg1 ++ ", 0\n"
        appendFile name code
      else 
        let immediate = show (fromJust $ tacRvalue1 tac) ++ "\n"
        in appendFile name $ "li " ++ dest ++ ", " ++ immediate

-- ThreeAddressCode Param Nothing (Just p) (Just n)
genParam :: TAC -> InterfGraph -> I.IntMap Int -> String -> IO ()
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
