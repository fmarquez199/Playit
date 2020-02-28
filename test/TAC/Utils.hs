module Utils where

import Test.Hspec
import Control.Monad.RWS
import Playit.Lexer       (alexScanTokens)
import Playit.Parser      (parse)
import Playit.SymbolTable (initState)
import Playit.TAC         (gen, tacInitState)
import qualified Playit.TACType as T
import Playit.AuxFuncs (tacConstant, tacVariable, tacLabel, tacGoTo)
import Playit.Types

runTestForValidTAC :: String -> [TAC] -> IO ()
runTestForValidTAC program genTAC = do
  let
    tokens   = alexScanTokens program
    fileCode = ("TestValidTAC.game", program)

  (ast, SymTabState{symTab = st}, _) <- runRWST (parse tokens) fileCode initState
  (tac, _, _) <- runRWST (gen ast) ast (tacInitState st)
  tac `shouldBe` genTAC


---- Programas

intAddition :: String
intAddition = "world %Add%:\n\
               \  Power i = 3 + 4\n\
               \.~"

intSubstraction :: String
intSubstraction = "world %Subtraction%:\n\
                   \  Power i = 3 - 4\n\
                   \.~"

intMultiplication :: String
intMultiplication = "world %Multiplication%:\n\
                     \  Power i = 3 * 4\n\
                     \.~"

intsDivision :: String
intsDivision = "world %Division%:\n\
                \  Skill i = 3 / 4\n\
                \.~"

intDivision :: String
intDivision = "world %IntDivision%:\n\
               \  Power i = 3 // 4\n\
               \.~"

intMod :: String
intMod = "world %Module%:\n\
          \  Power i = 3 % 4\n\
          \.~"

floatAddition :: String
floatAddition = "world %Add%:\n\
                 \  Skill i = 3'0 + 4'0\n\
                 \.~"

floatSubstraction :: String
floatSubstraction = "world %Subtraction%:\n\
                     \  Skill i = 3'0 - 4'0\n\
                     \.~"

floatMultiplication :: String
floatMultiplication = "world %Multiplication%:\n\
                       \  Skill i = 3'0 * 4'0\n\
                       \.~"

floatDivision :: String
floatDivision = "world %Division%:\n\
                 \  Skill i = 3'0 / 4'0\n\
                 \.~"

conjuction :: String
conjuction = "world %Disjunction%:\n\
               \  Battle p = Win && Lose\n\
               \.~"

disjunction :: String
disjunction = "world %Disjunction%:\n\
               \  Battle p = Win || Lose\n\
               \.~"

intLessThan :: String
intLessThan = "world %LessThan%:\n\
               \  Battle p = 3 < 4\n\
               \.~"

intLessEq :: String
intLessEq = "world %LessThan%:\n\
             \  Battle p = 3 <= 4\n\
             \.~"

intEq :: String
intEq = "world %LessThan%:\n\
         \  Battle p = 3 == 4\n\
         \.~"

intNotEq :: String
intNotEq = "world %LessThan%:\n\
            \  Battle p = 3 != 4\n\
            \.~"

intGreaterThan :: String
intGreaterThan = "world %LessThan%:\n\
                  \  Battle p = 3 > 4\n\
                  \.~"

intGreaterEq :: String
intGreaterEq = "world %LessThan%:\n\
                \  Battle p = 3 >= 4\n\
                \.~"

floatLessThan :: String
floatLessThan = "world %LessThan%:\n\
                 \  Battle p = 3'0 < 4'0\n\
                 \.~"

floatLessEq :: String
floatLessEq = "world %LessThan%:\n\
               \  Battle p = 3'0 <= 4'0\n\
               \.~"

floatEq :: String
floatEq = "world %LessThan%:\n\
           \  Battle p = 3'0 == 4'0\n\
           \.~"

floatNotEq :: String
floatNotEq = "world %LessThan%:\n\
              \  Battle p = 3'0 != 4'0\n\
              \.~"

floatGreaterThan :: String
floatGreaterThan = "world %LessThan%:\n\
                    \  Battle p = 3'0 > 4'0\n\
                    \.~"

floatGreaterEq :: String
floatGreaterEq = "world %LessThan%:\n\
                  \  Battle p = 3'0 >= 4'0\n\
                  \.~"

upperCase :: String
upperCase = "world %upperCase%:\n\
             \  Rune c = ^*x*\n\
             \.~"

lowerCase :: String
lowerCase = "world %name%:\n.~"

charLessThan :: String
charLessThan = "world %name%:\n.~"

charLessEq :: String
charLessEq = "world %name%:\n.~"

charEq :: String
charEq = "world %name%:\n.~"

charNotEq :: String
charNotEq = "world %name%:\n.~"

charGreaterThan :: String
charGreaterThan = "world %name%:\n.~"

charGreaterEq :: String
charGreaterEq = "world %name%:\n.~"


---- Salidas

intAddition' :: [TAC]
intAddition' = [assign3, assign4, intOp "+", assignInt]

intSubstraction' :: [TAC]
intSubstraction' = [assign3, assign4, intOp "-", assignInt]

intMultiplication' :: [TAC]
intMultiplication' = [assign3, assign4, intOp "*", assignInt]

intsDivision' :: [TAC]
intsDivision' = [assign3, assign4, intOp "/", assignFloat]

intDivision' :: [TAC]
intDivision' = [assign3, assign4, intOp "//", assignInt]

intMod' :: [TAC]
intMod' = [assign3, assign4, intOp "%", assignInt]

floatAddition' :: [TAC]
floatAddition' = [assign3'0, assign4'0, floatOp "+", assignFloat]

floatSubstraction' :: [TAC]
floatSubstraction' = [assign3'0, assign4'0, floatOp "-", assignFloat]

floatMultiplication' :: [TAC]
floatMultiplication' = [assign3'0, assign4'0, floatOp "*", assignFloat]

floatDivision' :: [TAC]
floatDivision' = [assign3'0, assign4'0, floatOp "/", assignFloat]

conjuction' :: [TAC]
conjuction' = [assignTrue, assignFalse, tacIf "$t0" "lit" 0 True, assignBool False, exitlist 1, label 0, saveBoolVar "$t0" "$t1", label 1, saveBool]

disjunction' :: [TAC]
disjunction' = [assignTrue, assignFalse, tacIf "$t0" "lit" 0 True, saveBoolVar "$t0" "$t1", exitlist 1, label 0, assignBool True, label 1, saveBool]

intLessThan' :: [TAC]
intLessThan' = [assign3, assign4, intOp "-", zeroIntComp "<" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

intLessEq' :: [TAC]
intLessEq' = [assign3, assign4, intOp "-", zeroIntComp "<=" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

intEq' :: [TAC]
intEq' = [assign3, assign4, intOp "-", zeroIntComp "==" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

intNotEq' :: [TAC]
intNotEq' = [assign3, assign4, intOp "-", zeroIntComp "!=" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

intGreaterThan' :: [TAC]
intGreaterThan' = [assign3, assign4, intOp "-", zeroIntComp ">" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

intGreaterEq' :: [TAC]
intGreaterEq' = [assign3, assign4, intOp "-", zeroIntComp ">=" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

floatLessThan' :: [TAC]
floatLessThan' = [assign3'0, assign4'0, floatOp "-", zeroFloatComp "<" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

floatLessEq' :: [TAC]
floatLessEq' = [assign3'0, assign4'0, floatOp "-", zeroFloatComp "<=" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

floatEq' :: [TAC]
floatEq' = [assign3'0, assign4'0, floatOp "-", zeroFloatComp "==" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

floatNotEq' :: [TAC]
floatNotEq' = [assign3'0, assign4'0, floatOp "-", zeroFloatComp "!=" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

floatGreaterThan' :: [TAC]
floatGreaterThan' = [assign3'0, assign4'0, floatOp "-", zeroFloatComp ">" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

floatGreaterEq' :: [TAC]
floatGreaterEq' = [assign3'0, assign4'0, floatOp "-", zeroFloatComp ">=" 0, assignFalse', exitlist 1, label 0, assignTrue', label 1, saveBool']

upperCase' :: [TAC]
upperCase' = [assignChar "$t0" 'x', lowerL "$t0" "97", lLimit, exitlist 2, label 0, upperL "$t0", uLimit, exitlist 2, label 1, up "$t1", label 2, saveChar]

lowerL :: String -> String -> TAC
lowerL reg x = T.TACC T.Sub (temp reg TChar "binOp" False) (temp reg TChar "lit" True) (tacConstant (x, TInt))

assignChar :: String -> Char -> TAC
assignChar reg c = T.TACC T.Assign (temp reg TChar "lit" True) (tacConstant (show c, TChar)) Nothing

lLimit :: TAC
lLimit = T.TACC T.Gte (temp "$t0" TChar "binOp" False) (tacConstant ("0", TInt)) (tacLabel 0)

upperL :: String -> TAC
upperL reg = T.TACC T.Sub (temp reg TChar "binOp" False) (temp reg TChar "binOp" False) (tacConstant ("25", TInt))

uLimit :: TAC
uLimit = T.TACC T.Lte (temp "$t0" TChar "binOp" False) (tacConstant ("0", TInt)) (tacLabel 1)

up :: String -> TAC
up reg = T.TACC T.Sub (temp reg TChar "binOp" False) (temp reg TChar "binOp" False) (tacConstant ("32", TInt))

down :: String -> TAC
down reg = T.TACC T.Add (temp reg TChar "binOp" False) (temp reg TChar "binOp" False) (tacConstant ("32", TInt))

saveChar :: TAC
saveChar = T.TACC T.Assign char (temp "$t1" TChar "binOp" False) Nothing

char :: TACOP
char = tacVariable $ SymbolInfo "c" TChar 1 Variables ("global", -1) []

lowerCase' :: [TAC]
lowerCase' = []

charLessThan' :: [TAC]
charLessThan' = []

charLessEq' :: [TAC]
charLessEq' = []

charEq' :: [TAC]
charEq' = []

charNotEq' :: [TAC]
charNotEq' = []

charGreaterThan' :: [TAC]
charGreaterThan' = []

charGreaterEq' :: [TAC]
charGreaterEq' = []


---- Operadores

true :: TACOP
true = tacConstant ("True", TBool)

false :: TACOP
false = tacConstant ("False", TBool)

int :: TACOP
int = tacVariable $ SymbolInfo "i" TInt 1 Variables ("global", 0) []

float :: TACOP
float = tacVariable $ SymbolInfo "i" TFloat 1 Variables ("global", 0) []

bool :: TACOP
bool = tacVariable $ SymbolInfo "p" TBool 1 Variables ("global", 0) []

temp :: TReg -> Type -> String -> Bool -> TACOP
temp reg t typ True  = tacVariable $ SymbolInfo reg t 1 Constants (typ, -1) []
temp reg t typ False = tacVariable $ SymbolInfo reg t 1 Variables (typ, -1) []

assign3 :: TAC
assign3 = T.TACC T.Assign (temp "$t0" TInt "lit" True) (tacConstant ("3", TInt)) Nothing

assign4 :: TAC
assign4 = T.TACC T.Assign (temp "$t1" TInt "lit" True) (tacConstant ("4", TInt)) Nothing

intOp :: String -> TAC
intOp "+"  = T.TACC T.Add (temp "$t0" TInt "binOp" False) (temp "$t0" TInt "lit" True) (temp "$t1" TInt "lit" True)
intOp "-"  = T.TACC T.Sub (temp "$t0" TInt "binOp" False) (temp "$t0" TInt "lit" True) (temp "$t1" TInt "lit" True)
intOp "*"  = T.TACC T.Mult (temp "$t0" TInt "binOp" False) (temp "$t0" TInt "lit" True) (temp "$t1" TInt "lit" True)
intOp "/"  = T.TACC T.Div (temp "$t0" TFloat "binOp" False) (temp "$t0" TInt "lit" True) (temp "$t1" TInt "lit" True)
intOp "//" = T.TACC T.Div (temp "$t0" TInt "binOp" False) (temp "$t0" TInt "lit" True) (temp "$t1" TInt "lit" True)
intOp "%"  = T.TACC T.Mod (temp "$t0" TInt "binOp" False) (temp "$t0" TInt "lit" True) (temp "$t1" TInt "lit" True)

assignInt :: TAC
assignInt = T.TACC T.Assign int (temp "$t0" TInt "binOp" False) Nothing

assign3'0 :: TAC
assign3'0 = T.TACC T.Assign (temp "$t0" TFloat "lit" True) (tacConstant ("3.0", TFloat)) Nothing

assign4'0 :: TAC
assign4'0 = T.TACC T.Assign (temp "$t1" TFloat "lit" True) (tacConstant ("4.0", TFloat)) Nothing

floatOp :: String -> TAC
floatOp "+" = T.TACC T.Add (temp "$t0" TFloat "binOp" False) (temp "$t0" TFloat "lit" True) (temp "$t1" TFloat "lit" True)
floatOp "-" = T.TACC T.Sub (temp "$t0" TFloat "binOp" False) (temp "$t0" TFloat "lit" True) (temp "$t1" TFloat "lit" True)
floatOp "*" = T.TACC T.Mult (temp "$t0" TFloat "binOp" False) (temp "$t0" TFloat "lit" True) (temp "$t1" TFloat "lit" True)
floatOp "/" = T.TACC T.Div (temp "$t0" TFloat "binOp" False) (temp "$t0" TFloat "lit" True) (temp "$t1" TFloat "lit" True)

assignFloat :: TAC
assignFloat = T.TACC T.Assign float (temp "$t0" TFloat "binOp" False) Nothing

assignTrue :: TAC
assignTrue = T.TACC T.Assign (temp "$t0" TBool "lit" True) true Nothing

assignFalse :: TAC
assignFalse = T.TACC T.Assign (temp "$t1" TBool "lit" True) false Nothing

tacIf :: String -> String -> Int -> Bool -> TAC
tacIf reg mode l b = T.TACC T.If Nothing (temp reg TBool mode b) (tacLabel l)

assignBool :: Bool -> TAC
assignBool True  = T.TACC T.Assign (temp "$t0" TBool "binOp" False) true Nothing
assignBool False = T.TACC T.Assign (temp "$t0" TBool "binOp" False) false Nothing

exitlist :: Int -> TAC
exitlist x = tacGoTo (tacLabel x)

label :: Int -> TAC
label x = T.TACC T.NewLabel (tacLabel x) Nothing Nothing

saveBoolVar :: String -> String -> TAC
saveBoolVar x y = T.TACC T.Assign (temp x TBool "binOp" False) (temp y TBool "lit" True) Nothing

saveBool :: TAC
saveBool = T.TACC T.Assign bool (temp "$t0" TBool "binOp" False) Nothing

zeroIntComp :: String -> Int -> TAC
zeroIntComp "<"  x = T.TACC T.Lt  (temp "$t0" TInt "binOp" False) (tacConstant ("0", TInt)) (tacLabel x)
zeroIntComp "<=" x = T.TACC T.Lte (temp "$t0" TInt "binOp" False) (tacConstant ("0", TInt)) (tacLabel x)
zeroIntComp "==" x = T.TACC T.Eq  (temp "$t0" TInt "binOp" False) (tacConstant ("0", TInt)) (tacLabel x)
zeroIntComp "!=" x = T.TACC T.Neq (temp "$t0" TInt "binOp" False) (tacConstant ("0", TInt)) (tacLabel x)
zeroIntComp ">"  x = T.TACC T.Gt  (temp "$t0" TInt "binOp" False) (tacConstant ("0", TInt)) (tacLabel x)
zeroIntComp ">=" x = T.TACC T.Gte (temp "$t0" TInt "binOp" False) (tacConstant ("0", TInt)) (tacLabel x)

zeroFloatComp :: String -> Int -> TAC
zeroFloatComp "<"  x = T.TACC T.Lt  (temp "$t0" TFloat "binOp" False) (tacConstant ("0.0", TFloat)) (tacLabel x)
zeroFloatComp "<=" x = T.TACC T.Lte (temp "$t0" TFloat "binOp" False) (tacConstant ("0.0", TFloat)) (tacLabel x)
zeroFloatComp "==" x = T.TACC T.Eq  (temp "$t0" TFloat "binOp" False) (tacConstant ("0.0", TFloat)) (tacLabel x)
zeroFloatComp "!=" x = T.TACC T.Neq (temp "$t0" TFloat "binOp" False) (tacConstant ("0.0", TFloat)) (tacLabel x)
zeroFloatComp ">"  x = T.TACC T.Gt  (temp "$t0" TFloat "binOp" False) (tacConstant ("0.0", TFloat)) (tacLabel x)
zeroFloatComp ">=" x = T.TACC T.Gte (temp "$t0" TFloat "binOp" False) (tacConstant ("0.0", TFloat)) (tacLabel x)

assignTrue' :: TAC
assignTrue' = T.TACC T.Assign (temp "$t0" TBool "binOp" False) true Nothing

assignFalse' :: TAC
assignFalse' = T.TACC T.Assign (temp "$t0" TBool "binOp" False) false Nothing

saveBool' :: TAC
saveBool' = T.TACC T.Assign bool (temp "$t0" TBool "binOp" False) Nothing

