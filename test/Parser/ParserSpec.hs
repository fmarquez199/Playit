module ParserSpec where

import Test.Hspec
import Utils
-- import Playit.Lexer
-- import Playit.Parser
-- import Playit.SymbolTable
import Playit.Types


spec :: Spec
spec = describe "Instructions" $ do
    
  it "accepts program with only 1 instruction" $
    runTestForValidProgram helloWorld (\(Program [
      Print [Literal (Str "~HELLO WORLD!~") TStr] TVoid
      ] TVoid) -> True)
  
  it "rejects definitions inside instructions block" $
    runTestForInvalidProgram wrongDefs

  it "accepts determined loop program structure" $
    runTestForValidProgram forWhile (\(Program [
      ForWhile "i" (Literal (Integer 0) TInt) (Literal (Integer 10) TInt)
        (Binary Eq (Binary Module (Variable (Var "i" TInt) TInt) (Literal (Integer 2) TInt) TInt)
          (Literal (Integer 0) TInt) TBool)
      [
        Assigs [] TVoid,
        Assigs [
          Assig (Var "n" TInt)
            (Read (Literal (Str "~Insert any number~") TStr) TStr)
            TVoid
          ] TVoid,
        Assig (Var "winner" TBool)
          (Binary Eq (Variable (Var "i" TInt) TInt) (Variable (Var "n" TInt) TInt) TBool)
          TVoid
      ] TVoid
      ] TVoid) -> True)

  it "accepts undetermined loop program structure" $
    runTestForValidProgram while (\(Program [
      Assigs [Assig (Var "int" TInt) (Literal (Integer 1) TInt) TVoid] TVoid,
      While (Binary Less (Variable (Var "int" TInt) TInt)
      (Literal (Integer 10000) TInt) TBool)
      [
        Assig (Var "int" TInt)
          (Binary Add (Variable (Var "int" TInt) TInt) (Literal (Integer 1) TInt) TInt)
          TVoid
      ] TVoid
      ] TVoid) -> True)

  it "accepts registers initialization program" $
    runTestForValidProgram initReg (\(Program [
      Assigs [Assig (Var "p" (TNew "Potions")) 
        (Literal (Register [Literal (Integer 10) TInt,Literal (Floatt 10.5) TFloat]) (TNew "Potions"))
        TVoid] TVoid,
      Assig (Var "p'" (TNew "Potions"))
        (Literal (Register [Literal (Integer 5) TInt,Literal (Floatt 6.5) TFloat]) (TNew "Potions"))
        TVoid
      ] TVoid) -> True)

  it "accepts correcursion subroutines" $
    runTestForValidProgram correcursion (\(Program [
      ProcCall (Call "g" []) TVoid
      ] TVoid) -> True)


helloWorld :: String
helloWorld = "world %HolaMundo%:\n\
  \  drop ~HELLO WORLD!~\n\
  \.~"

wrongDefs :: String
wrongDefs = "world %Invalid%:\n\
  \  Inventory Potions:\n\
  \  .~\n\
  \.~"

forWhile :: String
forWhile = "world %ForWhile%:\n\
  \  controller Power i = 0 -> 10 lock i % 2 == 0:\n\
  \    Battle winner\n\
  \    Power n = joystick ~Insert any number~\n\
  \    winner = i == n\n\
  \  .~\n\
  \.~"

while :: String
while = "world %While%:\n\
  \  Power int = 1\n\
  \  play:\n\
  \    int++\n\
  \  lock int < 10000\n\
  \  .~\n\
  \.~"

initReg :: String
initReg =
  "Inventory Potions:\n\
  \    Power hp\n\
  \    Skill mana\n\
  \.~\n\
  \world %InitReg%:\n\
  \    Potions p = Potions {10, 10'5}, p'\n\
  \    p' = Potions {5, 6'5}\n\
  \.~"

correcursion :: String
correcursion =
  "boss f():\n\
  \  kill g()\n\
  \.~\n\
  \boss g():\n\
  \  kill f()\n\
  \.~\n\
  \world %Correcursion%:\n\
  \  kill g()\n\
  \.~"


