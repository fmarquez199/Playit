{-
 *  Este archivo contiene el código empleado para correr las pruebas.
 *  
 *      Al iniciarse su ejecución escanea la carpeta CHask~/test/casos/ por 
 *  archivos que terminen en .game 
 *      El programa lee el contenido del .game lo pasa por el lexer, y este le 
 *  regresa una lista con los tokens reconocidos, a los que el programa les
 *  genera su representación en String a cada uno guardandolos en la lista X.
 *      El programa lee el contenido del archivo de salida esperada .out en la misma
 *  carpeta, genera una lista con las lineas de la salida esperada.
 *      El programa crea un Caso de prueba HUnit que compara cada linea del 
 *  archivo .out salida esperada con la salida real obtenida del lexer.
 *
 *      En caso de un error se muestra el nombre del archivo y la linea esperada 
 *  y el    token que generó el error.
 *
 * Para crear un caso de prueba entonces se deben seguir los pasos:
 * 
 * 1) Crear el .game con el código.
 * 2) Correrlo en el lexer.
 * 3) Copiar la salida y chequear manualmente si es correcta.
 * 4) y ya está en la suit de pruebas.
 *
 * Copyright : (c)
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
 *
-}

-- Para forzar la lectura I/O  
import qualified Data.ByteString as S 
-- Para convertir ByteString a String
import qualified Data.ByteString.Char8 as BS
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Data.Strings (strEndsWith,strBreak)
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Control.Monad (forM)
import Playit.Lexer
import Playit.Parser
import Util(getRecursiveContents)

pruebasLexer :: IO Test.HUnit.Test
pruebasLexer = do
    -- Obtiene todos los archivos en test/casos
    files <- getRecursiveContents "test/casos/lexer"
    -- Filtra los archivos a aquellos que terminen en .game
    filesToTest <- forM files $ \filen -> 
        if strEndsWith filen ".game" then do
            let (fname,ext) = strBreak ".game" filen
            return [fname]
        else return []
        
        -- return fname
    
    -- Aplana la lista filtrada (filesToTest era una lista de lista)
    let filesToTestDotGame = concat filesToTest
    
    -- Recorremos todos los .game y creamos los casos de prueba
    testCases <- forM filesToTestDotGame $ \filen -> do
        -- Lee el codigo 
        fileSource        <- openFile (filen ++ ".game")    ReadMode  
        -- Lee la salida esperada del Lexer
        fileExpectedOut   <- openFile (filen ++ ".outlexer")     ReadMode

        -- Extrae el codigo del archivo
        strSourceCode     <- S.hGetContents fileSource
        -- Extrae la salida esperada del archivo
        strExpectedOut    <- S.hGetContents fileExpectedOut

        -- Separa el contenido por los saltos de lineas 
        let lstStrExpectedOut = lines $ BS.unpack strExpectedOut
        
        -- Obtiene la lista de Tokens reconocidos en el codigo
        let lstRecognizedTkns         = alexScanTokens $ BS.unpack strSourceCode 
        -- Crea una lista de strings con los Tokens 
        let lstStrRecognizedTokens    = map show lstRecognizedTkns     
            
        
        let testCases = [TestCase $ assertEqual ("\n***Error en tokens de:" ++ filen ++ ".game ***") lineExpected lineRecognized | (lineExpected , lineRecognized) <- zip lstStrExpectedOut lstStrRecognizedTokens]
        
        -- Cerramos los archivos
        hClose fileSource
        hClose fileExpectedOut
        
        return testCases
    
    return $ TestList $ concat testCases



pruebasParser :: IO Test.HUnit.Test
pruebasParser = do
    -- Obtiene todos los archivos en test/casos
    files <- getRecursiveContents "test/casos/parser"
    -- Filtra los archivos a aquellos que terminen en .game
    filesToTest <- forM files $ \filen -> 
        if strEndsWith filen ".game" then do
            let (fname,ext) = strBreak ".game" filen
            return [fname]
        else return []
        
        -- return fname
    
    -- Aplana la lista filtrada (filesToTest era una lista de lista)
    let filesToTestDotGame = concat filesToTest
    
    -- Recorremos todos los .game y creamos los casos de prueba
    testCases <- forM filesToTestDotGame $ \filen -> do
        -- Lee el codigo 
        fileSource        <- openFile (filen ++ ".game")    ReadMode  
        -- Lee la salida esperada del Parser
        fileExpectedOut   <- openFile "TestPassed.outparser" ReadMode

        -- Extrae el codigo del archivo
        strSourceCode     <- S.hGetContents fileSource
        -- Extrae la salida esperada del archivo
        strExpectedOut    <- S.hGetContents fileExpectedOut

        -- Separa el contenido por los saltos de lineas 
        let lstStrExpectedOut = lines $ BS.unpack strExpectedOut
        
        -- Obtiene la lista de Tokens reconocidos en el codigo
        let lstRecognizedTkns         = alexScanTokens $ BS.unpack strSourceCode 
        -- TODO: Verificar esto
        let tokensParser              = parse lstRecognizedTkns
        -- Crea una lista de strings con los Tokens 
        let lstStrRecognizedTokens    = show tokensParser     
            
        
        let testCases = [TestCase $ assertEqual ("\n***Error en  parser:" ++ filen ++ ".game ***") lstStrExpectedOut lstStrRecognizedTokens ]
        
        -- Cerramos los archivos
        hClose fileSource
        hClose fileExpectedOut
        
        return testCases
    
    return $ TestList $ concat testCases



main :: IO ()
main = do
    
    tlLexer <- pruebasLexer
    runTestTT tlLexer

    tlParser <- pruebasParser
    runTestTT tlParser

    return ()
