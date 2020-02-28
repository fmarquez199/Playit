module Utils where

import Test.Hspec
import Control.Monad.RWS
import Playit.Lexer
import Playit.Parser
import Playit.SymbolTable
import Playit.Types (Instr)

runTestForValidProgram :: String -> (Instr -> Bool) -> IO ()
runTestForValidProgram program predicate = do
  let
    tokens = alexScanTokens program

  (ast, _, _) <- runRWST (parse tokens) ("TestValidProgram.game",program) stInitState
  ast `shouldSatisfy` predicate

runTestForInvalidProgram :: String -> IO ()
runTestForInvalidProgram program = do
  let
    tokens = alexScanTokens program

  runRWST (parse tokens) ("TestInvalidProgram.game",program) stInitState `shouldThrow` anyException

runTestForBadTypes :: String -> (Instr -> Bool) -> IO ()
runTestForBadTypes program predicate = do
  let
    tokens   = alexScanTokens program
    parsedTk = runRWST (parse tokens) ("TestInvalidTypesProgram.game",program) stInitState
  (ast, _, _) <- runRWST (parse tokens) ("TestInvalidTypesProgram.game",program) stInitState
  ast `shouldSatisfy` predicate
  parsedTk `shouldThrow` anyException
