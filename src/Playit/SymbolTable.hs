{-
Modulo para la creacion y manejo de la tabla de simbolos

* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.SymbolTable where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import qualified Data.Map as M
-- import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust, isNothing)
import Playit.Types


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--              Creacion y manejo de la tabla de simbolos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Estado inicial con todo lo predefinido del lenguaje
initState = createInitSymTab M.empty [0]


createInitSymTab :: SymTab -> StackScopes -> MonadSymTab ()
createInitSymTab = addToSymTab ["Power"] TInt
createInitSymTab = addToSymTab ["Skill"] TFloat
createInitSymTab = addToSymTab ["Rune"] TChar
createInitSymTab = addToSymTab ["Runes"] TStr
createInitSymTab = addToSymTab ["Battle"] TBool
createInitSymTab = addToSymTab ["Inventory"] TRegistro
createInitSymTab = addToSymTab ["Items"] TUnion
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Se crea el alacance interno con su tabla y padre asociado
openInnerScope :: MonadSymTab ()
openInnerScope = do
    (actualSymTab, actualScope) <- get
    put $ (SymTab (M.empty, Just actualSymTab), actualScope + 1)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Se cierra el alcance actual devolviendo el mando al padre
closeScope :: MonadSymTab ()
closeScope = do
    (SymTab (_, father), actualScope) <- get
    case father of
        (Just st) -> put (st, actualScope - 1)
        Nothing -> {-error "\n\nNo hay tabla padre.\n"-} return ()
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Agrega a la tabla de simbolos la lista de identificadores con su informcaion:
--  Tipo, alcance
addToSymTab :: [Nombre] -> Tipo -> SymTab -> StackScopes -> MonadSymTab ()
addToSymTab ids t actualSymTab scopes@(scope:_) = 
    put (insertSymbols ids t actualSymTab scope, scopes) >> return ()
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Inserta los identificadores con su tipo en la tabla de simbolos dada
insertSymbols :: [Nombre] -> Tipo -> SymTab -> Alcance -> SymTab
insertSymbols [] _ symTab _ = symTab
insertSymbols (id:ids) t (SymTab table) scope
    | isNothing (M.lookup id table) =
        insertSymbols ids t newSymTab scope

    | otherwise = 
        error ("\n\nActualizar info de la variable: '" ++ id ++
                "', junto con su otra info.\n")
    
    where
        -- Tabla de simbolos con el identificador insertado
        newSymTab = SymTab M.insert id symbolInfo table
        symbolInfo = SymbolInfo t scope
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupInSymTab :: Nombre -> SymTab -> Maybe SymbolInfo
lookupInSymTab var (SymTab (table, Nothing)) = M.lookup var table
lookupInSymTab var (SymTab (table, Just fathe))
    | isJust varInfo = varInfo
    | otherwise = lookupInSymTab var father

    where   varInfo = M.lookup var table
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--        Manejo de la tabla de simbolos al 'correr' las instrucciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Obtiene la informacion asociada a la variable
getVarInfo :: Vars -> SymTab -> SymbolInfo
getVarInfo (Var n _) (SymTab (table, Nothing)) = fromJust $ M.lookup n table
getVarInfo var@(Var name _) (SymTab (table, Just father))
    | isJust varInfo = fromJust varInfo
    | otherwise = getVarInfo var father
    
    where   varInfo = M.lookup name table
getVarInfo (VarIndex var _ _) st = getVarInfo var st
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Determina si una variable esta inicializada o no
notEmptyValue :: Vars -> SymTab -> Bool
notEmptyValue (VarIndex vars _ _) symTab = notEmptyValue vars symTab
notEmptyValue var symTab =
    let varInfo = getVarInfo var symTab
    in  getVal varInfo /= ValorVacio
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Actualiza el valor de la variable
updateVarVal :: Vars -> Literal -> Int -> SymTab -> Alcance -> SymTab
updateVarVal var@(Var name _) value _ st@(SymTab (table, _)) scope =
    updatedTable

    where
        varInfo = getVarInfo var st
        newVal (SymbolInfo t _ scope) = SymbolInfo t value scope
        updatedTable = addVarInScope name (newVal varInfo) st scope

updateVarVal var@(VarIndex _ _ _) value index st@(SymTab (table, _)) scope
    | index > 0 && index <= tamArray = updatedTable
    | otherwise =
        error ("\n\nError: El indice: " ++ show (index - 1) ++
                " esta fuera de rango.\n")

    where
        varInfo = getVarInfo var st
        (Arreglo lst) = getVal varInfo
        (lst1, lst2) = splitAt index lst
        tamArray = length lst
        newLst = Arreglo $ init lst1 ++ [value] ++ lst2
        newVal (SymbolInfo t _ scope) = SymbolInfo t newLst scope
        -- name = head $ splitOn "[" $ showVar var
        name = takeWhile (/= '[') $ showVar var
        updatedTable = addVarInScope name (newVal varInfo) st scope
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Agrega la variable en la tabla con el alcance correspondiente
addVarInScope :: Nombre -> SymbolInfo -> SymTab -> Alcance -> SymTab
addVarInScope var val@(SymbolInfo _ _ varScope) (SymTab (table, Nothing)) scope =
    let freshTable = M.delete var table
        updatedTable = M.insert var val freshTable
    in SymTab (updatedTable, Nothing)

addVarInScope var val@(SymbolInfo _ _ varScope) st@(SymTab (table, father)) scope
    | varScope == scope =
        let freshTable = M.delete var table
            updatedTable = M.insert var val freshTable
        in SymTab (updatedTable, father)
    --  varScope > scope && father == Nothing =
    --     addVarInScope var val st (scope - 1)
    | otherwise =
        let updatedSymTab = addVarInScope var val (fromJust father) (scope - 1)
        in SymTab (table, Just updatedSymTab)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Actualiza la tabla de simbolos padre con la modificada
updateSymTab :: SymTab -> SymTab -> SymTab
updateSymTab symTab@(SymTab (table, Nothing)) _ = symTab
updateSymTab (SymTab (table, _)) updatedF = SymTab (table, Just updatedF)
--------------------------------------------------------------------------------

