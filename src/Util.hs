{-
 *
 * getRecursiveContents: Dada la ruta a una carpeta , obtiene todas los archivos
 * de la carpeta y sus sub-carpetas recursivamente.
 * Dato "/" regresa ["/test.game","test2.out","/docs/test.txt"]
 -}

module Util where
import System.Directory(getDirectoryContents,doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM)

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

