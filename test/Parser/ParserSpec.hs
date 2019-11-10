module InstructionsSpec where

import Test.Hspec
import Utils
-- import Playit.Lexer
-- import Playit.Parser
-- import Playit.SymbolTable
-- import Playit.Types


spec :: Spec
spec = describe "Instructions" $ do
    
    it "accepts program with only 1 instruction" $
        runTestForValidProgram helloWorld (\"\nworld:\n\
            \  drop [\"~HELLO WORLD!~\"]\n\
            \.~\n" -> True)
    
    it "rejects definitions inside instructions block" $
        runTestForInvalidProgram wrongDefs

    it "accepts ranged loop program structure" $
        runTestForValidProgram rangedController (\"\nworld:\n\
            \  controller i = 0 -> 10 lock ((i % 2) == 0):\n\
            \      n = joystick \"~Insert any number~\"\n\
            \    winner = (i == n)\n\n\
            \  .~\n\
            \.~\n" -> True)


helloWorld :: String
helloWorld = "world %HolaMundo%:\n\
    \  drop ~HELLO WORLD!~\n\
    \.~"

wrongDefs :: String
wrongDefs = "world %Invalid%:\n\
    \  Inventory Potions:\n\
    \  .~\n\
    \.~"

rangedController :: String
rangedController = "world %rangedController%:\n\
    \  controller Power i = 0 -> 10 lock i % 2 == 0:\n\
    \    Battle winner\n\
    \    Power n = joystick ~Insert any number~\n\
    \    winner = i == n\n\
    \  .~\n\
    \.~"


