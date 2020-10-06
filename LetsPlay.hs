import Data.Strings       (strEndsWith, strSplit)
import System.Process     (callCommand)
import System.Environment (getArgs)

runmars :: String -> String
runmars name = "java -jar ~/Downloads/Mars4_5.jar output/" ++ name ++ ".asm"

main :: IO ()
main = do
    args <- getArgs
    let name = case length args of
        0 -> error "TOO BAD! No .game to play :("
        1 -> let file = head args 
            in if strEndsWith file ".game" then fst $ strSplit ".game" file
            else error "Wait a minute, are you sure this is a .game to play?"
        _ -> error "AMAZING, you can play more than just one .game at the same time, but I can not, try just one .game"
    callCommand $ "stack run " ++ name ++ ".game"
    callCommand $ runmars $ snd $ strSplit "/" name
    putStrLn $ "Thank you for playing " ++ head args ++ " with us! Come back soon!"