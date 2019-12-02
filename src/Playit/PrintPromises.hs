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
printPromiseLateChecks (LateCheckPromiseSubroutine e pos ids:r) = do
    putStrLn "\t\tExpresion: "
    putStrLn $ "\t\t\t" ++ show e
    putStrLn $ "\t\tPos: " ++ show pos
    putStrLn "\t\tEnlaces: "
    if not $ null ids then 
        printPromiseLinks ids
    else
        putStrLn "\t\t\tNone"
    printPromiseLateChecks r


printPromiseLateChecksCalls :: [LateCheckPromise] -> IO ()
printPromiseLateChecksCalls [] = return ()
printPromiseLateChecksCalls (LateCheckPromiseCall subr ids:r) = do
  putStrLn "\t\tSubroutine: "
  putStrLn $ "\t\t\t" ++ show subr
  putStrLn "\t\tEnlaces: "
  if not $ null ids then 
    printPromiseLinks ids
  else
    putStrLn "\t\t\tNone"
  printPromiseLateChecksCalls r


printPromiseLateChecksForEachs :: [LateCheckPromise] -> IO ()
printPromiseLateChecksForEachs [] = return ()
printPromiseLateChecksForEachs (LateCheckPromiseForEach expr idvar tvar pos ids:r) = do
  putStrLn "\t\tExpresion: "
  putStrLn $ "\t\t\t" ++ show expr
  putStrLn $ "\t\tPos: " ++ show pos
  putStrLn $ "\t\tIDVar: " ++ show idvar
  putStrLn $ "\t\tTypeVar: " ++ show tvar
  putStrLn "\t\tEnlaces: "
  if not $ null ids then 
    printPromiseLinks ids
  else
    putStrLn "\t\t\tNone"
  printPromiseLateChecksForEachs r


printPromises :: [Promise] -> IO ()
printPromises [] = return ()
printPromises (PromiseSubroutine id params typer cat pos checks checks2 checks3: r) = do
    putStrLn $ "PromiseFunction : " ++ id
    putStrLn "\tparams : "
    if not $ null params then 
        printPromiseParam params
    else
        putStrLn "\t\tNone"
    putStrLn $ "\ttype : " ++ show typer
    putStrLn $ "\tat : " ++ show pos
    putStrLn $ "\tchecksOnUpdatePromiseExpressions : N: " ++ show (length checks)
    printPromiseLateChecks checks
    putStrLn $ "\tchecksOnUpdatePromiseCalls : N: " ++ show (length checks2)
    printPromiseLateChecksCalls checks2
    putStrLn $ "\tchecksOnUpdatePromiseForEach : N: " ++ show (length checks3)
    printPromiseLateChecksForEachs checks3

    printPromises r