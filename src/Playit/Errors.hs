{- |
 * Errors messages
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.Errors where


import Control.Monad.Trans.RWS
import Data.List.Split (splitOn)
import Playit.Lexer
import Playit.Types



-------------------------------------------------------------------------------
-- | Code line of the error
errorLine :: String -> Int -> String
errorLine code l = head (drop (l-1) (splitOn "\n" code)) ++ "\n"
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Ruler to specify the column of the error
errorRuler :: Int -> String
errorRuler c = "\t\x1b[1;93m" ++ replicate (c-1) '.' ++ "\x1b[5;31m^\x1b[0m\n"
-------------------------------------------------------------------------------
    

-------------------------------------------------------------------------------
-- | Message of the error
errorMsg :: String -> FileCodeReader -> Pos -> String
errorMsg msg (file,code) (l,c) = "\n\n\x1b[1;36m" ++ msg ++ "\x1b[94m: " ++
  file ++ ":\n" ++ "\x1b[93m| " ++ show l ++ "\t\x1b[0;96m" ++
  errorLine code l ++ errorRuler c
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                             Lexical Errors
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Determines if there's error tokens and return its positions
lexerErrors :: [Token] -> (Bool, [Pos])
lexerErrors [] = (False, [(-1::Int, -1::Int)])
lexerErrors (TkError _ p:tks) = (True, p : map isError tks)
lexerErrors (_:tks) = lexerErrors tks
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Get the position of a error token
isError :: Token -> Pos
isError (TkError _ p) = p
isError _ = (-1::Int, -1::Int)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Show the one's token error message
tkError :: FileCodeReader -> Pos -> String
tkError = errorMsg "\x1b[1;94m¡¡¡PLAYIT FATALITY!!!\n"
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Show all lexical errors
showLexerErrors :: FileCodeReader -> [Pos] -> String
showLexerErrors fileCode [] = ""
showLexerErrors fileCode ((-1, -1):pos) = showLexerErrors fileCode pos
showLexerErrors fileCode (p:pos) = concat $ tkError fileCode p : [showLexerErrors fileCode pos]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                             Syntax Errors
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Show the first parser error
parseError :: [Token] -> MonadSymTab a
parseError [] =  error "\n\n\x1b[1;91mInvalid Program\n\n"
parseError (tk:tks) =  do
  fileCode <- ask
  error $ errorMsg "Parse error" fileCode (getPos tk)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tellParserError :: String -> Token -> MonadSymTab ()
tellParserError msg tk = do
  fileCode <- ask
  let pos = (fst $ getPos tk, snd (getPos tk) + length (getTk tk))
  tell [errorMsg msg fileCode pos]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Save in the Writer Monad the errors in the program
errorProg :: String -> InstrSeq -> Token -> Int -> MonadSymTab Instr
errorProg msg instrs tk n = do
  fileCode <- ask
  let pos = (fst $ getPos tk, snd (getPos tk) + n * length (getTk tk))
  tell [errorMsg msg fileCode pos]
  return $ Program (reverse instrs) TError
-------------------------------------------------------------------------------


-- | Save in the Writer Monad the errors in the sequences
-- errorSeq :: String -> 


-------------------------------------------------------------------------------
errorIf :: String -> [(Expr, InstrSeq)] -> Token -> MonadSymTab Instr
errorIf msg cases tk = do
  tellParserError msg tk
  return $ IF cases TError
-------------------------------------------------------------------------------


-- errorGuard :: String -> Expr -> InstrSeq -> Pos -> MonadSymTab (Expr, InstrSeq)
-- errorGuard msg expr instrs pos = do
--     fileCode <- ask
--     tell [errorMsg msg fileCode pos]
--     return (expr, instrs)


-------------------------------------------------------------------------------
errorFor :: String -> Id -> Expr -> Expr -> InstrSeq -> Token -> MonadSymTab Instr
errorFor msg var e1 e2 i tk = do
  tellParserError msg tk
  return $ For var e1 e2 i TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
errorForWhile :: String -> Id -> Expr -> Expr -> Expr -> InstrSeq -> Token -> MonadSymTab Instr
errorForWhile msg var e1 e2 e3 i tk = do
  tellParserError msg tk
  return $ ForWhile var e1 e2 e3 i TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
errorForEach :: String -> Id -> Expr -> InstrSeq -> Token -> MonadSymTab Instr
errorForEach msg var e i tk = do
  tellParserError msg tk
  return $ ForEach var e i TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
errorWhile :: String -> Expr -> InstrSeq -> Token -> MonadSymTab Instr
errorWhile msg e i tk = do
  tellParserError msg tk
  return $ While e i TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Semmantic Errors
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Message of the semmantic error
semmErrorMsg :: String -> String -> FileCodeReader -> Pos -> String
semmErrorMsg t1 t2 (file,code) (l,c) = "\n\n\x1b[1;36mMismatched types\x1b[94m:"
  ++ file ++ ":\n" ++ "Expected: " ++ t1 ++ "    Got: " ++ t2 ++
  "\n\x1b[93m|\n| " ++ show l ++ "\t\x1b[0;96m" ++ errorLine code l ++ errorRuler c
-------------------------------------------------------------------------------
