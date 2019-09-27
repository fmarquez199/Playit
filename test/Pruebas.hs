import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import Playit.Lexer



main :: IO ()
main = do
  putStrLn "Testing "
  file <- openFile ("test/casos/condicionales/casoifsimplecondtrue.hs") ReadMode  -- Leer un archivo.
  content <- hGetContents file
  let tokens = (alexScanTokens content)  -- Crea la lista de tokens.
  let graphic = map show tokens    -- Crea la lista de tokens imprimible.
  runTestTT ( TestCase $ assertEqual "Tokens generados incorrectos." ["Token Bl en la fila: 1, columna: 1","Token puedeConducir en la fila: 1, columna: 4","Token = en la fila: 1, columna: 18","Token F en la fila: 1, columna: 20","Token In en la fila: 2, columna: 1","Token edad en la fila: 2, columna: 4","Token = en la fila: 2, columna: 9","Token 18 en la fila: 2, columna: 11","Token | en la fila: 3, columna: 1","Token edad en la fila: 3, columna: 3","Token >= en la fila: 3, columna: 8","Token 18 en la fila: 3, columna: 11","Token : en la fila: 3, columna: 13","Token puedeConducir en la fila: 4, columna: 3","Token = en la fila: 4, columna: 17","Token T en la fila: 4, columna: 19"] graphic)
  
  hClose file
  

