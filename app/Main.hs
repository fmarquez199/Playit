{-
 *  Entrada principal al compilador del Lenguaje  Playit
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Main where

import Control.Monad (forM)
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Playit.Lexer

main :: IO ()
main = do

  args <- getArgs                        -- Tomar argumentos de la terminal.
  file <- openFile (head args) ReadMode  -- Leer un archivo.

  content   <- hGetContents file           -- Copia todo el contenido del archivo

  let tokens    = alexScanTokens content  -- Crea la lista de tokens.
  let graphic   = map show tokens    -- Crea la lista de tokens imprimible.
  
  -- Se imprimen todos los tokens
  _ <- forM graphic $ \tokenln -> do
    putStrLn tokenln
    return ()
  -- Se cierra el archivo código.
  hClose file
  
  
