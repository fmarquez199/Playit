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


<<<<<<< HEAD
print_list [] = ""
print_list (x:xs) = x ++ "\n" ++ (print_list xs)

print_error [] = ""
print_error (x:xs)
  | subString "Error: " x = x ++ "\n" ++ (print_error xs)
  | otherwise = "" ++ (print_error xs)

subString "" _ = True
subString _ "" = False
subString (x:xs) y = x `elem` y && (subString xs y)

=======
>>>>>>> develop
main = do

  args <- getArgs                        -- Tomar argumentos de la terminal.
  file <- openFile (head args) ReadMode  -- Leer un archivo.
<<<<<<< HEAD
  content <- hGetContents file           -- Copia todos las lineas del archivo.
  let tokens = (alexScanTokens content)  -- Crea la lista de tokens.
  let graphic = map show tokens    -- Crea la lista de tokens imprimible.
  
  -- Si existe al menos un error: se imprimen solo errores.
  if subString "Error: " (print_list graphic) then
     putStrLn (print_error graphic)
  -- Si no, se imprime todo token encontrado.
  else
    putStrLn (print_list graphic)
  
    -- Archivo cerrado.
=======

  content   <- hGetContents file           -- Copia todo el contenido del archivo

  let tokens    = alexScanTokens content  -- Crea la lista de tokens.
  let graphic   = map show tokens    -- Crea la lista de tokens imprimible.
  
  -- Se imprimen todos los tokens
  _ <- forM graphic $ \tokenln -> do
    putStrLn tokenln
    return ()
  -- Se cierra el archivo código.
>>>>>>> develop
  hClose file
  
  
