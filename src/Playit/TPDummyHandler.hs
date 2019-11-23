{- |
 * Maneja el tipo TPDummy que representa el tipo de retorno de una función que todavía
 no ha sido declarado,
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.TPDummyHandler where


import Control.Monad.Trans.RWS
--import qualified Data.Map as M
import Data.Maybe (isJust,fromJust)
import Playit.AuxFuncs
import Playit.Errors
import Playit.SymbolTable
import Playit.Types



-------------------------------------------------------------------------------
updatePromiseTypeFunction :: Expr -> Type -> MonadSymTab ()
updatePromiseTypeFunction exprF t = do
    (symTab, activeScopes, scope,promises) <- get
    let name = case exprF of 
                (FuncCall (Call name _) _) -> name
                _ -> error "Internal error : FunctionCall doesn't have a name"
        promise = getPromiseSubroutine name promises
    
    if isJust promise then do
        let modifyTypePromise prom@(Promise id p _ pos) = 
                if id == name then Promise id p t pos else prom

        put(symTab, activeScopes, scope , map modifyTypePromise promises)
        updateType name 1 t
    else
        error $ "Internal error : Promise for '"  ++ name ++ "' not defined!"
-------------------------------------------------------------------------------

{-
    Dada una expresion cuyo el tipo  de sus llamadas a funciones no se ha decidido. 
    Ejemplo:
        // Donde funcion1 y funcion2 no se han declarado y no se sabe si es suma de enteros
        // o flotantes
        funcion1() + funcion2()
        
    Le asigna a las promesas de funcion1 y funcion2  el tipo de retorno t
-}
updateExprPromiseType :: Expr -> Type -> MonadSymTab ()
updateExprPromiseType (Binary op e1 e2 TPDummy) t = updateExprPromiseType e1 t >>  updateExprPromiseType e2 t
updateExprPromiseType (FuncCall (Call name params) TPDummy ) t = updateSubroutinePromiseType name t
updateExprPromiseType _ _   = return ()
    

{-
    Le asigna a una promesa de funcion el tipo de retorno pasado como argumento.
-}
updateSubroutinePromiseType :: Id -> Type -> MonadSymTab ()
updateSubroutinePromiseType name t = do
    (symTab, activeScopes, scope,promises) <- get
        
    let promise = getPromiseSubroutine name promises
    if isJust promise then do

        let promise' = fromJust promise
        let typePromise = getTypePromise promise'

        if typePromise == TPDummy then do
            let 
                modifyTypePromise prom@(PromiseSubroutine id p _ pos) = 
                    if id == name then PromiseSubroutine id p t pos else prom

            put(symTab, activeScopes, scope , map modifyTypePromise promises)

            updateType name 1 t
            return ()
        else if typePromise /= t then do
            -- error $ semmErrorMsg (show t) (show typePromise) fileCode p
            error "TEST: IS THIS GONNA GET EXECUTED? LOL"
            return ()
        else return ()
    else do
        return ()
--        error $ "Internal error : Promise for '"  ++ name ++ "' is not defined!"
    return ()


{- 
updatePromiseTypeFunction exprF t = do
    (symTab, activeScopes, scope,promises) <- get
        
    let name = case exprF of 
                (FuncCall (Call name _) _) -> name
                _ -> error $ "Internal error : FunctionCall doesn't have a name"

    let promise = getPromiseSubrutine name promises
    if isJust promise then do
        let 
            modifyTypePromise prom@(Promise id p typ pos) = 
                if id == name then Promise id p t pos else prom

        put(symTab, activeScopes, scope , map modifyTypePromise promises)

        updateType name 1 t
    else do
        error $ "Internal error : Promise for '"  ++ name ++ "' not defined!"


(F1Dummy + F2Dummy) 
(F1Dummy - F2Dummy) 
(F1Dummy * F2Dummy) 
(F1Dummy / F2Dummy) 

3*2 + 1,2


-}