{- |
 *  Main module
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Main where

import Control.Monad                     (mapM_)
import Control.Monad.Trans.RWS           (runRWST)
import Control.Monad.Trans.State         (execStateT)
import Data.List                         (nub)
import Data.Strings                      (strEndsWith, strSplit, strSplitAll, strStartsWith, strJoin)
import Playit.BackEnd.RegAlloc.FlowGraph
import Playit.BackEnd.RegAlloc.GraphColoring
import Playit.BackEnd.RegAlloc.InterferenceGraph
import Playit.BackEnd.RegAlloc.LiveVariables
import Playit.BackEnd.FinalCode
import Playit.BackEnd.TAC
import Playit.BackEnd.Utils              (copyOpt)
import Playit.BackEnd.Types              (Operands(vars), RegAlloc(bLiveVars))
import Playit.FrontEnd.Errors            (lexerErrors, showLexerErrors)
import Playit.FrontEnd.Lexer             (alexScanTokens)
import Playit.FrontEnd.Parser            (parse)
import Playit.FrontEnd.SymbolTable       (stInitState)
import Playit.FrontEnd.Types             (SymTabState(..), printData)
import System.Environment                (getArgs)
import System.Directory                  (createDirectoryIfMissing)
import System.IO                         (readFile, writeFile, appendFile)
import Data.Graph                        (vertices)
import Data.Map                          (toList)
import Data.Set                          (fromList)


-- | Determines if an file is empty
isEmptyFile :: String -> Bool
isEmptyFile = all (== '\n')


-- | Determines that '.game' is the extension of the file
checkExt :: [String] -> Either String String
checkExt []         = Left "\nError: no file given\n"
checkExt (file:_:_) = Left "\nError: more than one file given\n"
checkExt [file]     = if strEndsWith file ".game" then Right file
                      else  Left "\nError: extension for file not valid\n"

f :: [String] -> [String]
f ls = if null ls then [] else
  (head ls):f (filter (notlabel (label (head ls))) ls)

label :: String -> String
label = (fst . strSplit ":")

notlabel :: String -> String -> Bool
notlabel l1 l2 = l1 /= label l2

main :: IO ()
main = do
  -- Get arguments from terminal
  args <- getArgs

  case checkExt args of
    Left  msg         -> putStrLn msg
    Right checkedFile -> do
      code <- readFile checkedFile

      if null code || isEmptyFile code then putStrLn "\nError: empty file\n"
      else
        let tokens       = alexScanTokens code
            (hasErr, pos) = lexerErrors tokens
            fileCode     = (checkedFile, code)
            parseCode    = parse tokens
        in
        if hasErr then putStrLn $ showLexerErrors fileCode pos
        else do
          -- mapM_ print tokens
          (ast, state@SymTabState{symTab = st}, errs) <- runRWST parseCode fileCode stInitState
          
          -- print st

          if null errs then do
            createDirectoryIfMissing True "./output/"
            -- rm file and create new one
            writeFile dataFilePath $ "# Assemble " ++ show checkedFile ++ "\n##"
            appendFile dataFilePath "\nboolTrue: .asciiz \"Win\"\n"
            appendFile dataFilePath "boolFalse: .asciiz \"Lose\"\n"
            appendFile dataFilePath "newLine: .asciiz \"\\n\"\n"
            (_, state, tac) <- runRWST (genTAC ast) ast (tacInitState (symTab state))
            -- print state
            print ast -- >> print st >> printPromises (proms state)
            -- putStrLn $ "\nActive scopes: " ++ show (actS state)
            -- putStrLn $ "\nActual scope:" ++ show (stScope state)
            -- putStrLn $ "\nOffSets: " ++ show (offSets state)
            -- putStrLn $ "\nActual offset: " ++ show (actOffS state)
            -- let tac = reverse $ copyOpt $ reverse tac'
            mapM_ print tac
            let (fg@(graph, getNodeFromVertex, getVertexFromKey), leaders) = genFlowGraph tac
                nodes = map getNodeFromVertex (vertices graph)
            -- putStrLn $ "\nFlow Graph: " ++ show graph
            -- putStrLn $ "\nNodes: " ++ printFGNodes nodes
            -- print leaders
            regAlloc <- execStateT (getLiveVars fg) (initRegAlloc nodes)
            -- return ()
            let
              liveVars = fromList $ map snd $ toList $ bLiveVars regAlloc
              inter@(ig, nodeFromVertex, vertexFromKey) = genInterferenceGraph liveVars
              igNodes = map nodeFromVertex (vertices ig)
              color =  colorDsatur inter
            -- putStrLn $ "\nLive Vars: " ++ printLiveVars (toList (bLiveVars regAlloc))
            -- putStrLn $ "\nInterference Graph: " ++ printIGNodes igNodes
            -- putStrLn $ "\nDSatur coloring: " ++ show color
            -- putStrLn $ "Ahora el código final en " ++ checkedFile
            let outputFile = last (strSplitAll "/" (fst (strSplit "." checkedFile))) ++ ".asm"
            d <- readFile dataFilePath
            -- TODO!!: al eliminar labels, eliminar el primero y dejar el ultimo
            let
              db s = strStartsWith (last s) ".space 8" || strStartsWith (last s) ".double"
              w s = strStartsWith (last s) ".space 4" || strStartsWith (last s) ".word"
              o s = strStartsWith (last s) ".asciiz"
              d' = f (lines d)
              double = unlines $ nub $ map (strJoin ": ") $ filter db (map (strSplitAll ": ") $ tail $ d')
              four = unlines $ nub $ map (strJoin ": ") $ filter w (map (strSplitAll ": ") $ tail $ d')
              one = unlines $ nub $ map (strJoin ": ") $ filter o (map (strSplitAll ": ") $ tail $ d')
              d'' = "# Assemble " ++ show checkedFile ++ "\n##\n\t.data\n" ++ 
                double ++ four ++ one ++ "\n\t\t.text\n"
            
            writeFile ("./output/" ++ outputFile) d''
            genFinalCode (tail tac) inter color ("./output/" ++ outputFile)
            -- close outputFile
          else
            mapM_ putStrLn errs
