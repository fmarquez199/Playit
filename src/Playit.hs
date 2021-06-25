module Playit (playit) where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Playit.Import              as I


-- -----------------------------------------------------------------------------
data CmdArgs = CmdArgs
  { game           :: String -- ^ Source code filename
  , play           :: Bool   -- ^ Runs the .game in MARS
  , previewGame    :: Bool   -- ^ Prints the source code in '.game' file
  , showMyTokens   :: Bool   -- ^ Prints the recognized tokens
  , showSymTable   :: Bool
  , showAST        :: Bool
  , showTAC        :: Bool
  , showBlocks     :: Bool
  , showFlowGraph  :: Bool
  , showLiveVars   :: Bool
  , showInterGraph :: Bool
  , showRegAssig   :: Bool
  , showTargetCode :: Bool
  } deriving (Show)

cmdParser :: I.Parser CmdArgs
cmdParser = CmdArgs
  <$> I.argument I.str ( I.metavar "FILENAME" <> I.value "" )
  <*> I.switch ( I.long "play"         <> I.short 'p' <> I.help "Runs the .game in MARS" )
  <*> I.switch ( I.long "game"         <> I.short 'g' <> I.help "Preview .game" )
  <*> I.switch ( I.long "tokens"       <> I.short 'k' <> I.help "Show tokens" )
  <*> I.switch ( I.long "symtab"       <> I.short 's' <> I.help "Show symtable" )
  <*> I.switch ( I.long "ast"          <> I.short 'a' <> I.help "Show AST" )
  <*> I.switch ( I.long "tac"          <> I.short 't' <> I.help "Show three-address code" )
  <*> I.switch ( I.long "basic-blocks" <> I.short 'b' <> I.help "Show the basic blocks" )
  <*> I.switch ( I.long "flow-graph"   <> I.short 'f' <> I.help "Show flow graph and nodes" )
  <*> I.switch ( I.long "live-vars"    <> I.short 'v' <> I.help "Show the live variables" )
  <*> I.switch ( I.long "inter-graph"  <> I.short 'i' <> I.help "Show interference graph" )
  <*> I.switch ( I.long "reg-assig"    <> I.short 'r' <> I.help "Show register assignment (DSatur)" )
  <*> I.switch ( I.long "target-code"  <> I.short 'm' <> I.help "Show Target Code (.asm file)" )


-- -----------------------------------------------------------------------------
data Glitches = Glitches
  { noGameBuilt  :: IO () -- ^ Empty file
  , noGameToPlay :: IO () -- ^ No file given
  , noGame       :: IO () -- ^ No .game extension
  , noGameFound  :: IO () -- ^ File doesn't exist
  , noGameDLC    :: IO () -- ^ Unknown flag
  , twoGames     :: IO () -- ^ Two files were given
  }

glitch :: BL.ByteString -> IO ()
glitch g =
  BLC.hPutStr I.stderr (BL.concat [I.bold, I.red, BLC.pack "6l;tc# @%=$ ", I.nocolor, g, I.newLine])
  >> I.exitFailure

glitches :: Glitches
glitches = Glitches
  { noGameBuilt  = glitch $ BLC.pack "This .game its a void universe to be build"
  , noGameToPlay = glitch $ BLC.pack "You didn't chose a .game to play"
  , noGame       = glitch $ BLC.pack "This is not a .game"
  , noGameFound  = glitch $ BLC.pack "This .game itsn't installed yet"
  , noGameDLC    = glitch $ BLC.pack "This .game doesn't support the DLC(s)"
  , twoGames     = glitch $ BLC.pack "Can't play 2 games at the same time"
  }


-- -----------------------------------------------------------------------------
runLexer :: String -> BL.ByteString -> Either (IO ()) [I.Token]
runLexer filename code =
  let
    I.LexerResult errs tokens = I.alexScanTokens filename code
  in
    if null errs then Right $ reverse tokens
                 else Left (mapM_ print errs)

-- play 
-- play = -- run MARS, execute .asm


-- -----------------------------------------------------------------------------
f :: [String] -> [String]
f ls = if null ls then [] else (head ls) : f (filter (notlabel (label (head ls))) ls)
  where
    label :: String -> String
    label = (fst . I.strSplit ":")

    notlabel :: String -> String -> Bool
    notlabel l1 l2 = l1 /= label l2


-- -----------------------------------------------------------------------------
-- correr el parser aunque haya errores lexicos, para al final mandar todos los errores
playit :: IO ()
playit = do
  let flags = I.info (cmdParser I.<**> I.helper) 
                (  I.fullDesc <> I.progDesc "Build an entire .game"
                <> I.header "playit - Compiler for Playit programming language")
  cmdArgs <- I.execParser flags
  let game' = game cmdArgs
  gameInstalled <- I.doesFileExist game'

  if not gameInstalled then noGameFound glitches
  else
    if I.takeExtension game' /= ".game" then noGame glitches
    else do
      handle <- I.openFile game' I.ReadMode
      code   <- BL.hGetContents handle
      

      -- '\n' == 10 in Word8
      if BL.all (== 10) code || BL.null code then noGameBuilt glitches
      else
        -- FrontEnd
        let fileCode = I.ParserR game' (BL.split 10 code)
            lexer    = runLexer game' code
            lexerL   = print "lexer fromLeft fail"
            lexerR   = [I.Token I.TkError (BLC.pack "lexer fromRight fail") (-1,-1)]
        in
        if I.isLeft lexer then I.fromLeft lexerL lexer >> return ()
        else do
          putStrLn . show $ I.fromRight lexerR lexer
          let
            parseCode = I.parse $ I.fromRight lexerR lexer
          
          (ast, state@I.ParserS{I.psSymTab = st}, errs) <- I.runRWST parseCode fileCode I.pInitState
          
          if I.psError state then print errs -- I.mapM_ putStrLn errs
          else
            print ast >> print (I.psInLoop state) >> print (I.psError state)
        {-    -- BackEnd
            (_, state, tac) <- I.runRWST (I.genTAC ast) ast (I.tacInitState st)
            let
              (fg@(graph, getNodeFromVertex, getVertexFromKey), leaders) = I.genFlowGraph tac
              nodes = map getNodeFromVertex (I.vertices graph)
            regAlloc <- I.execStateT (I.getLiveVars fg) (I.initRegAlloc nodes)
            let
              liveVars = I.fromList $ map snd $ I.toList $ I.bLiveVars regAlloc
              inter@(ig, nodeFromVertex, vertexFromKey) = I.genInterferenceGraph liveVars
              igNodes = map nodeFromVertex (I.vertices ig)
              color   = I.colorDsatur inter

            -- write target code in file
            I.createDirectoryIfMissing True "./output/"
            -- rm file and create new one
            I.writeFile I.dataFilePath $ "# Assemble " ++ show game' ++ "\n##"
            I.appendFile I.dataFilePath "\nboolTrue: .asciiz \"Win\"\n"
            I.appendFile I.dataFilePath "boolFalse: .asciiz \"Lose\"\n"
            I.appendFile I.dataFilePath "newLine: .asciiz \"\\n\"\n"
            d <- readFile dataFilePath
            -- TODO!!: al eliminar labels, eliminar el primero y dejar el ultimo
            let
              outputFile = last (I.strSplitAll "/" (fst (I.strSplit "." game'))) ++ ".asm"
              db s   = I.strStartsWith (last s) ".space 8" || I.strStartsWith (last s) ".double"
              w s    = I.strStartsWith (last s) ".space 4" || I.strStartsWith (last s) ".word"
              o s    = I.strStartsWith (last s) ".asciiz"
              d'     = f (lines d)
              double = unlines $ I.nub $ map (I.strJoin ": ") $ filter db (map (I.strSplitAll ": ") $ tail $ d')
              four   = unlines $ I.nub $ map (I.strJoin ": ") $ filter w (map (I.strSplitAll ": ") $ tail $ d')
              one    = unlines $ I.nub $ map (I.strJoin ": ") $ filter o (map (I.strSplitAll ": ") $ tail $ d')
              d''    = "# Assemble " ++ show game' ++ "\n##\n\t.data\n" ++ double ++ four ++ one ++ "\n\t\t.text\n"
            
            I.writeFile ("./output/" ++ outputFile) d''
            I.genFinalCode (tail tac) inter color ("./output/" ++ outputFile)
            -- close outputFile
        -}    
            -- when (previewGame) print code
            -- when (showMyTokens cmdArgs) mapM_ print (I.fromRight [I.Token I.TkError BL.empty (-1,-1)] lexer)
            -- when (showshowSymTable cmdArgs) print st
            -- when (showshowAST cmdArgs) print ast
            -- when (showshowTac cmdArgs) mapM_ print (reverse $ I.copyOpt $ reverse tac)
              -- printPromises (proms state)
            -- when (showshowBlocks cmdArgs) print leaders -- putStrLn $ "\nNodes: " ++ I.printFGNodes nodes
            -- when (showshowFlowGraph cmdArgs) putStrLn $ "\nFlow Graph: " ++ show graph
            -- when (showshowLiveVars cmdArgs) putStrLn $ "\nLive Vars: " ++ printLiveVars (toList (bLiveVars regAlloc))
            -- when (showshowInterGraph cmdArgs) putStrLn $ "\nInterference Graph: " ++ I.printIGNodes igNodes
            -- when (showshowRegAssig cmdArgs) putStrLn $ "\nDSatur coloring: " ++ show color
            -- when (showshowTargetCode cmdArgs) print outputFile
            -- when (play cmdArgs) doPlay fileCode
            -- exitSuccess
