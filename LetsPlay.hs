import Data.Strings       (strEndsWith, strSplit)
import System.Process     (callCommand)
import System.Environment (getArgs)

runmars :: String -> String -> String
runmars name marsDir = "java -jar " ++ marsDir ++ " output/" ++ name ++ ".asm"

main :: IO ()
main = do
  args <- getArgs
  let 
    (name, marsDir) = case length args of
      0 -> noGameToPlay glitches
      1 -> error "TOO BAD! No path for Mars .jar file. T.T"
      -- .game + mars path
      2 -> 
        let
          mars = head args
          file = args !! 1
        in 
        if strEndsWith file ".game" then 
          (fst $ strSplit ".game" file, mars)
        else 
          error "Wait a minute, are you sure this is a .game to play?"
      -- .game + flags + mars path
      3 -> 
        let
          mars = head args
          file = args !! 1
          flag = args !! 2
        in 
        if strEndsWith file ".game" then 
          ((fst $ strSplit ".game" file) ++ flag, mars)
        else 
          error "Wait a minute, are you sure this is a .game to play?"
      -- two files/unkown args 
      _ -> 
        error "AMAZING, you can play more than just one .game at the same time, but I can not :'V"

  callCommand $ "stack run -- " ++ name ++ ".game" ++ flags
  -- callCommand $ "find Mars"
  callCommand $ runmars (snd $ strSplit "/" name) marsDir
  putStrLn $ "Thank you for playing '" ++ name ++ "' with us! Come back soon!"
