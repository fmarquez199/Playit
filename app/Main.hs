{-
 *  Entrada principal al compilador del Lenguaje  Playit
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Main where

import qualified Control.Exception as Exc------------------------------------- > (*)
import Control.Monad (forM)
import Control.Monad.Trans.State
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Data.Strings (strEndsWith,strBreak)
import Playit.SymbolTable
import Playit.Lexer
import Playit.Parser (parse)
import Playit.Print

-- Determina si un archivo esta vacio
isEmptyFile :: String -> Bool
isEmptyFile = all (== '\n')


-- Determina que el archivo tenga la extension conrrecta, '.bt'
checkExt :: [String] -> Either String String
checkExt [] = Left "\nError: debe indicar un archivo\n"
checkExt (file:_:_) = Left "\nError: solo se puede indicar un archivo\n"
checkExt [file] =  if strEndsWith file ".game" then Right file else  Left "\nError: archivo no es .game\n"


main :: IO ()
main = do

    args <- getArgs                           -- Tomar argumentos del terminal.
    case checkExt args of
        Left msg -> putStrLn msg
        Right checkedFile -> do
            code <- readFile checkedFile
            if null code || isEmptyFile code then
                putStrLn "\nArchivo vacio. Nada que hacer\n"
            else
                let tokens = alexScanTokens code in
                
                    -- mapM_ (putStrLn . show) tokens

                    if hasError tokens then
                        putStrLn $ tkErrorToString $ filter isError tokens
                    else do
                        (ast, lastState) <- runStateT (parse tokens) initState
                        print ast
                        -- return ()
                        printAST 0 ast -- >> evalStateT (runAST ast) lastState

