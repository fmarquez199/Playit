module Utils where

import Test.Hspec
import Control.Monad.RWS
import Playit.Lexer
import Playit.Parser
import Playit.SymbolTable
import Playit.Types (Instr)

runTestForValidProgram :: String -> (Instr -> Bool) -> IO ()
runTestForValidProgram program predicate = do
  let tokens = alexScanTokens program
  (ast, _, _) <- runRWST (parse tokens) ("TestValidProgram.game",program) initState
  ast `shouldSatisfy` predicate

runTestForInvalidProgram :: String -> IO ()
runTestForInvalidProgram program = do
  let tokens = alexScanTokens program
  (ast, _, errs) <- runRWST (parse tokens) ("TestInvalidProgram.game",program) initState
  if null errs && null promises then print ast >> print st
  else
    if null promises then mapM_ putStrLn errs
    else printPromises promises `shouldBe` mapM_ putStrLn errs 
