{- |
 *  Print promises module
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.PrintPromises where


import Playit.Types


printPromiseParam :: [(Type,Pos)] -> IO ()
printPromiseParam [] = return ()
printPromiseParam ((t,p):r) = do
    putStrLn $ "\t\tType: " ++ show t ++ " at: " ++ show p
    printPromiseParam r

printPromiseLinks :: [Id] -> IO ()
printPromiseLinks [] = return ()
printPromiseLinks (id:r) =
    putStrLn $ "\t\t\t" ++ show id


printPromiseLateChecks :: [LateCheckPromise] -> IO ()
printPromiseLateChecks [] = return ()
printPromiseLateChecks (LateCheckPromise e pos ids:r) = do
    putStrLn "\t\tExpresion: "
    putStrLn $ "\t\t\t" ++ show e
    putStrLn $ "\t\tPos: " ++ show pos
    putStrLn "\t\tEnlaces: "
    if not $ null ids then 
        printPromiseLinks ids
    else
        putStrLn "\t\t\tNone"
    printPromiseLateChecks r

printPromises :: [Promise] -> IO ()
printPromises [] = return ()
printPromises (Promise id params typer pos checks:r) = do
    putStrLn $ "PromiseFunction : " ++ id
    putStrLn "\tparams : "
    if not $ null params then 
        printPromiseParam params
    else
        putStrLn "\t\tNone"
    putStrLn $ "\ttype : " ++ show typer
    putStrLn $ "\tat : " ++ show pos
    putStrLn $ "\tchecksOnUpdate : " ++ " N: " ++ show (length checks)
    printPromiseLateChecks checks

    printPromises r