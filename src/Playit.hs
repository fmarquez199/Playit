module Playit (playit) where

import Control.Monad                     (mapM_, when)
import Control.Monad.Trans.RWS           (runRWST)
import Control.Monad.Trans.State         (execStateT)
import Data.List                         (nub)
import Data.Strings                      (strSplit, strSplitAll, strStartsWith, strJoin)
import Data.Either                       (isLeft, fromLeft, fromRight)
import Data.Graph                        (vertices)
import Data.Map                          (toList)
import Data.Set                          (fromList)
import Options.Applicative
import Playit.FrontEnd
-- import Playit.BackEnd
import Playit.Utils        
import System.Environment                (getArgs)
import System.Directory                  (doesFileExist, createDirectoryIfMissing)
import System.IO                         (IOMode(..), readFile, openFile, writeFile, appendFile, stderr)
import System.Exit                       (exitFailure, exitSuccess)
import System.FilePath                   (takeExtension)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC


-- -----------------------------------------------------------------------------
data CmdArgs = CmdArgs
  { game           :: String -- ^ Source code filename
  , play           :: Bool   -- ^ Runs the .game in MARS
  , previewGame    :: Bool   -- ^ Prints the source code in '.game' file
  , showMyTokens   :: Bool   -- ^ Prints the recognized tokens
  , showSymTable   :: Bool
  , showAST        :: Bool
  , showTac        :: Bool
  , showBlocks     :: Bool
  , showFlowGraph  :: Bool
  , showLiveVars   :: Bool
  , showInterGraph :: Bool
  , showRegAssig   :: Bool
  , showTargetCode :: Bool
  }
  deriving Show

cmdParser :: Parser CmdArgs
cmdParser = CmdArgs
  <$> argument str
    ( metavar "FILENAME"
    <> value "" )
  <*> switch
    ( long "play"
    <> short 'p'
    <> help "Runs the .game in MARS" )
  <*> switch
    ( long "game"
    <> short 'g'
    <> help "Preview .game" )
  <*> switch
    ( long "tokens"
    <> short 'k'
    <> help "Show tokens" )
  <*> switch
    ( long "symtable"
    <> short 's'
    <> help "Show symtable" )
  <*> switch
    ( long "ast"
    <> short 'a'
    <> help "Show AST" )
  <*> switch
    ( long "tac"
    <> short 't'
    <> help "Show three-address code" )
  <*> switch
    ( long "basic-blocks"
    <> short 'b'
    <> help "Show the basic blocks" )
  <*> switch
    ( long "flow-graph"
    <> short 'g'
    <> help "Show flow graph and nodes" )
  <*> switch
    ( long "live-vars"
    <> short 'v'
    <> help "Show the live variables" )
  <*> switch
    ( long "inter-graph"
    <> short 'i'
    <> help "Show interference graph" )
  <*> switch
    ( long "reg-assig"
    <> short 'r'
    <> help "Show register assignment (DSatur)" )
  <*> switch
    ( long "target-code"
    <> short 'm'
    <> help "Show Target Code (.asm file)" )


-- -----------------------------------------------------------------------------
data Glitches = Glitches
  {
    noGameBuilt  :: IO (), -- ^ Empty file
    noGameToPlay :: IO (), -- ^ No file given
    noGame       :: IO (), -- ^ No .game extension
    noGameFound  :: IO (), -- ^ File doesn't exist
    noGameDLC    :: IO (), -- ^ Unknown flag
    twoGames     :: IO ()  -- ^ Two files were given
  }

glitch :: BL.ByteString -> IO ()
glitch g =
  BLC.hPutStr stderr (BL.concat [bold, red, BLC.pack "Glitch @%=$)&+# ", nocolor, g, newLine])
  >> exitFailure

glitches :: Glitches
glitches = Glitches
  {
    noGameBuilt  = glitch $ BLC.pack "This .game its a void universe to be build",
    noGameToPlay = glitch $ BLC.pack "You didn't chose a .game to play",
    noGame       = glitch $ BLC.pack "This is not a .game",
    noGameFound  = glitch $ BLC.pack "This .game itsn't installed yet",
    noGameDLC    = glitch $ BLC.pack "This .game doesn't support the DLC(s)",
    twoGames     = glitch $ BLC.pack "Can't play 2 games at the same time"
  }


-- -----------------------------------------------------------------------------
runLexer :: (BL.ByteString, BL.ByteString) -> Either (IO ()) [MyToken]
runLexer fileCode@(_, code) =
  let
    LexerResult errs tokens = alexScanTokens code
  in
    if null errs then Right tokens
    else Left (showLexerErrors errs fileCode)

-- play 
-- play = -- run MARS, execute .asm


-- -----------------------------------------------------------------------------
f :: [String] -> [String]
f ls = if null ls then [] else (head ls) : f (filter (notlabel (label (head ls))) ls)

label :: String -> String
label = (fst . strSplit ":")

notlabel :: String -> String -> Bool
notlabel l1 l2 = l1 /= label l2


-- -----------------------------------------------------------------------------
-- 
playit :: IO ()
playit = do
  let flags = info (cmdParser <**> helper) 
                (  fullDesc <> progDesc "Build an entire .game"
                <> header "playit - Compiler for Playit programming language")
  cmdArgs <- execParser flags
  let game' = game cmdArgs
  gameInstalled <- doesFileExist game'

  if not gameInstalled then noGameFound glitches
  else
    if takeExtension game' /= ".game" then noGame glitches
    else do
      handle <- openFile game' ReadMode
      code   <- BL.hGetContents handle

      -- '\n' == 10 in Word8
      if BL.all (== 10) code || BL.null code then noGameBuilt glitches
      else
        -- FrontEnd
        let fileCode = (BLC.pack game', code)
            lexer    = runLexer fileCode
        in
        if isLeft lexer then fromLeft (print "fromLeft") lexer >> return ()
        else do
          let parseCode = parse $ fromRight [MyToken TkError BL.empty (-1,-1)] lexer
          (ast, state@SymTabState{symTab = st}, errs) <- runRWST parseCode fileCode stInitState
          
          if not $ null errs then mapM_ putStrLn errs
          else
            -- BackEnd
            (_, state, tac) <- runRWST (genTAC ast) ast (tacInitState st)
            let (fg@(graph, getNodeFromVertex, getVertexFromKey), leaders) = genFlowGraph tac
                nodes = map getNodeFromVertex (vertices graph)
            regAlloc <- execStateT (getLiveVars fg) (initRegAlloc nodes)
            let
              liveVars = fromList $ map snd $ toList $ bLiveVars regAlloc
              inter@(ig, nodeFromVertex, vertexFromKey) = genInterferenceGraph liveVars
              igNodes = map nodeFromVertex (vertices ig)
              color   = colorDsatur inter

            -- write target code in file
            createDirectoryIfMissing True "./output/"
            -- rm file and create new one
            writeFile dataFilePath $ "# Assemble " ++ show game' ++ "\n##"
            appendFile dataFilePath "\nboolTrue: .asciiz \"Win\"\n"
            appendFile dataFilePath "boolFalse: .asciiz \"Lose\"\n"
            appendFile dataFilePath "newLine: .asciiz \"\\n\"\n"
            d <- readFile dataFilePath
            -- TODO!!: al eliminar labels, eliminar el primero y dejar el ultimo
            let
              outputFile = last (strSplitAll "/" (fst (strSplit "." game'))) ++ ".asm"
              db s   = strStartsWith (last s) ".space 8" || strStartsWith (last s) ".double"
              w s    = strStartsWith (last s) ".space 4" || strStartsWith (last s) ".word"
              o s    = strStartsWith (last s) ".asciiz"
              d'     = f (lines d)
              double = unlines $ nub $ map (strJoin ": ") $ filter db (map (strSplitAll ": ") $ tail $ d')
              four   = unlines $ nub $ map (strJoin ": ") $ filter w (map (strSplitAll ": ") $ tail $ d')
              one    = unlines $ nub $ map (strJoin ": ") $ filter o (map (strSplitAll ": ") $ tail $ d')
              d''    = "# Assemble " ++ show game' ++ "\n##\n\t.data\n" ++ double ++ four ++ one ++ "\n\t\t.text\n"
            
            writeFile ("./output/" ++ outputFile) d''
            genFinalCode (tail tac) inter color ("./output/" ++ outputFile)
            -- close outputFile
            
            -- when (previewGame) print code
            -- when (showMyTokens cmdArgs) mapM_ print (fromRight [MyToken TkError BL.empty (-1,-1)] lexer)
            -- when (showshowSymTable cmdArgs) print st
            -- when (showshowAST cmdArgs) print ast
            -- when (showshowTac cmdArgs) mapM_ print (reverse $ copyOpt $ reverse tac)
              -- printPromises (proms state)
            -- when (showshowBlocks cmdArgs) print leaders -- putStrLn $ "\nNodes: " ++ printFGNodes nodes
            -- when (showshowFlowGraph cmdArgs) putStrLn $ "\nFlow Graph: " ++ show graph
            -- when (showshowLiveVars cmdArgs) putStrLn $ "\nLive Vars: " ++ printLiveVars (toList (bLiveVars regAlloc))
            -- when (showshowInterGraph cmdArgs) putStrLn $ "\nInterference Graph: " ++ printIGNodes igNodes
            -- when (showshowRegAssig cmdArgs) putStrLn $ "\nDSatur coloring: " ++ show color
            -- when (showshowTargetCode cmdArgs) print outputFile
            -- when (play cmdArgs) doPlay fileCode
            -- exitSuccess
