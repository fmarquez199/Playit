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
import Playit.BackEnd.Types   (TAC, TACOP, TACInfo(..), InterfGraph)
import Playit.BackEnd.Utils   (tacNewLabel, tacLabel)
import Playit.FrontEnd.Types  (Type(..), symType)
import TACType                (ThreeAddressCode(..), Operation(..), Operand(..))

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
genFinalCode :: [TAC] -> InterfGraph -> String -> IO ()
genFinalCode tac inter name =
  if null tac then
    appendFile name ""
  else
    let h = head tac
    in case tacOperand h of
      x | x `elem` [Add, Sub, Mult, Div, Mod, Gt, Gte, Lt, Lte, Eq, Neq] ->
        genThreeOperandsOp h inter name >> genFinalCode (tail tac) inter name
      x | x `elem` [Minus, Length, Ref, Deref] ->
        genTwoOperandsOp h inter name >> genFinalCode (tail tac) inter name
      x | x `elem` [Return, Call, If, GoTo] ->
        genJumps h inter name >> genFinalCode (tail tac) inter name
      x | x `elem` [Print, Read] ->
        genSyscalls h inter name >> genFinalCode (tail tac) inter name
      Assign ->
        genAssign h inter name >> genFinalCode (tail tac) inter name
      NewLabel ->
        appendFile name (show h) >> genFinalCode (tail tac) inter name
      Param -> genParam h inter name >> genFinalCode (tail tac) inter name
      _ -> appendFile name "" >> genFinalCode (tail tac) inter name

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
genThreeOperandsOp :: TAC -> InterfGraph -> String -> IO ()
genThreeOperandsOp tac i@(_, _, t) name = do
  let
    inst = show (tacOperand tac) ++ " "
    dest = show (fromJust $ t $ fromJust $ tacInfo $ tacLvalue tac) ++ ", "
    reg1 = show (fromJust $ t $ fromJust $ tacInfo $ tacRvalue1 tac) ++ ", "
    reg2 = show (fromJust $ t $ fromJust $ tacInfo $ tacRvalue2 tac) ++ "\n"
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
      in genJumps tac i name >> appendFile name save
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

-- ThreeAddressCode Minus (Just x) (Just y) Nothing
-- ThreeAddressCode Length (Just x) (Just y) _
-- ThreeAddressCode Ref (Just x) (Just y) Nothing
-- ThreeAddressCode Deref (Just x) (Just y) Nothing
genTwoOperandsOp :: TAC -> InterfGraph -> String -> IO ()
genTwoOperandsOp tac (_, _, t) name = do
  let
    inst = show (tacOperand tac) ++ " "
    dest = show (fromJust $ t $ fromJust $ tacInfo $ tacLvalue tac) ++ ", "
    reg1 = show (fromJust $ t $ fromJust $ tacInfo $ tacRvalue1 tac)
    code = inst ++ dest ++ reg1
  appendFile name code

-- ThreeAddressCode Return Nothing (Just x) Nothing
-- ThreeAddressCode Call Nothing (Just f) (Just n)
-- ThreeAddressCode If Nothing (Just b) (Just label)
-- ThreeAddressCode GoTo Nothing Nothing (Just label)
genJumps :: TAC -> InterfGraph -> String -> IO ()
genJumps tac (_, _, t) name = case tacRvalue2 tac of
  Nothing -> do -- Return
    let
      dest = show (fromJust $ t $ fromJust $ tacInfo $ tacRvalue1 tac)
      code = "addi $v0, " ++ dest ++ ", 0\n"
    epilogue name >> appendFile name code
  _ -> do
    let
      goto = show (tacOperand tac) ++ " "
      dest = show (fromJust $ tacRvalue2 tac) ++ "\n"
    if isJust $ tacRvalue1 tac then
      if isCall $ tacOperand tac then -- Call Procedures
        let code = goto ++ show (fromJust $ tacRvalue1 tac) ++ ", " ++ dest
        in appendFile name code >> prologue name
        
      else
        let cond = show (fromJust $ t $ fromJust $ tacInfo $ tacRvalue1 tac)
        in appendFile name $ goto ++ cond ++ ", " ++ dest -- If
    else appendFile name $ goto ++ dest -- GoTo

epilogue :: String -> IO ()
epilogue name = appendFile name "#epilogo se vende por separado\n"

isCall :: Operation -> Bool
isCall Call = True
isCall _ = False

prologue :: String -> IO ()
prologue name = appendFile name "#prologo se vende por separado\n"

-- ThreeAddressCode Print Nothing (Just e) Nothing
-- ThreeAddressCode Read Nothing (Just e) Nothing
genSyscalls :: TAC -> InterfGraph -> String -> IO ()
genSyscalls tac (_, _, t) name = 
  let arg = show (fromJust $ t $ fromJust $ tacInfo $ tacRvalue1 tac)
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
genAssign :: TAC -> InterfGraph -> String -> IO ()
genAssign tac (_, _, t) name = 
  let dest = show (fromJust $ t $ fromJust $ tacInfo $ tacLvalue tac)
  in case tacRvalue1 tac of
    Nothing ->
      let code = "li " ++ show dest ++ ", 0\n"
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

-- ThreeAddressCode Param Nothing (Just p) Nothing
genParam :: TAC -> InterfGraph -> String -> IO ()
genParam tac (_, _, t) name = appendFile name ""