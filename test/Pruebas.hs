import qualified Data.ByteString as S  
import qualified Data.ByteString.Char8 as BS
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Data.Strings (strEndsWith,strBreak)
import System.Environment
import System.IO
import System.Directory
import System.IO.Error
import Control.Exception
import Playit.Lexer
import Control.Monad (forM)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)

main :: IO ()
main = do
    
    files <- getRecursiveContents "test/casos"
    filesToTest <- forM files $ \filen -> do
        fname <- if strEndsWith filen ".game" then do
            let (fname,ext) = strBreak ".game" filen
            return [fname]
        else return []
        
        return fname
    
    let filesToTestDotGame = concat filesToTest
    
    testCases <- forM filesToTestDotGame $ \filen -> do
        -- Lee el codigo 
        fileSource        <- openFile (filen ++ ".game") ReadMode  
        -- Lee la salida esperada del Lexer
        fileExpectedOut   <- openFile (filen ++ ".out") ReadMode

        -- Extrae el codigo del archivo
        strSourceCode     <- S.hGetContents fileSource
        -- Extrae la salida esperada del archivo
        strExpectedOut    <- S.hGetContents fileExpectedOut

        -- Separa el contenido por los saltos de lineas 
        let lstStrExpectedOut = lines $ BS.unpack strExpectedOut
        
        -- Obtiene la lista de Tokens reconocidos en el codigo
        let lstRecognizedTkns         = alexScanTokens $ BS.unpack strSourceCode 
        -- Crea una lista de strings con los Tokens 
        let lstStrRecognizedTokens    = map show lstRecognizedTkns           -- Crea la lista de tokens imprimible.
            
        
        let testCases = [TestCase $ assertEqual ("\n***Error en tokens de:" ++ filen ++ ".game ***") lineExpected lineRecognized | (lineExpected , lineRecognized) <- zip lstStrExpectedOut lstStrRecognizedTokens]

        hClose fileSource
        hClose fileExpectedOut
        
        return $ testCases
    
    runTestTT $ TestList $ concat testCases
    return ()
    


