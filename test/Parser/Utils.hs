module Utils where

import Test.Hspec
import Control.Monad.RWS         (runRWST)

import Playit.FrontEnd.Lexer     (alexScanTokens)
import Playit.FrontEnd.Parser    (parse)
import Playit.FrontEnd.SymbTable (stInitState)
import Playit.FrontEnd.Syntax    (Instruction(..))


runTestForValidProgram :: String -> (Instruction -> Bool) -> IO ()
runTestForValidProgram program predicate = do
  let
    tokens = alexScanTokens program

  (ast, _, _) <- runRWST (parse tokens) ("TestValidProgram.game",program) stInitState
  ast `shouldSatisfy` predicate


runTestForInvalidProgram :: String -> IO ()
runTestForInvalidProgram program =
  let
    tokens = alexScanTokens program
  in
  runRWST (parse tokens) ("TestInvalidProgram.game",program) stInitState `shouldThrow` anyException


runTestForBadTypes :: String -> (Instruction -> Bool) -> IO ()
runTestForBadTypes program predicate = do
  let
    tokens   = alexScanTokens program
    parsedTk = runRWST (parse tokens) ("TestInvalidTypesProgram.game",program) stInitState
  
  (ast, _, _) <- runRWST (parse tokens) ("TestInvalidTypesProgram.game",program) stInitState
  ast `shouldSatisfy` predicate
  parsedTk `shouldThrow` anyException
