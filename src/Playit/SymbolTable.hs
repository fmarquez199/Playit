{-
Modulo para la creacion y manejo de la tabla de simbolos

* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.SymbolTable where

import Control.Monad.Trans.RWS
import Control.Monad (void)
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
initState :: (SymTab,StackScopes)
initState = createInitSymTab (SymTab M.empty) [0]


createInitSymTab :: SymTab -> StackScopes -> (SymTab,StackScopes)
createInitSymTab st scopes = (insertSymbols symbols info st,scopes)
    where
        symbols = ["Power","Skill","Rune","Runes","Battle","Inventory","Items",
            "Win","Lose","free","puff"]
        info = [powerInfo,skillInfo,runeInfo,runesInfo,battleInfo,inventoryInfo,
            itemsInfo,boolsInfo,boolsInfo,aptInfo,aptInfo]
        powerInfo = SymbolInfo TInt 0 Tipos
        skillInfo = SymbolInfo TFloat 0 Tipos
        runeInfo = SymbolInfo TChar 0 Tipos
        runesInfo = SymbolInfo TStr 0 Tipos
        battleInfo = SymbolInfo TBool 0 Tipos
        inventoryInfo = SymbolInfo TRegistro 0 ConstructoresTipos
        itemsInfo = SymbolInfo TUnion 0 ConstructoresTipos
        boolsInfo = SymbolInfo TBool 0 Variable
        aptInfo = SymbolInfo (TApuntador TDummy) 0 Apuntadores
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Se empila el nuevo alcance
pushNewScope :: MonadSymTab ()
pushNewScope = do
    (actualSymTab, scopes@(actualScope:_)) <- get
    let newScope = actualScope + 1
    put (actualSymTab, newScope:scopes)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Se desempila el alcance actual
popScope :: MonadSymTab ()
popScope = do
    (actualSymTab, _:scopes) <- get
    put (actualSymTab, scopes)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Agrega a la tabla de simbolos la lista de identificadores con su informacion:
--  Tipo, alcance, categoria
addToSymTab :: [Nombre] -> [SymbolInfo] -> SymTab -> StackScopes -> MonadSymTab ()
addToSymTab ids info actualSymTab scopes = 
    put (insertSymbols ids info actualSymTab, scopes)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Inserta los identificadores con su tipo en la tabla de simbolos dada
insertSymbols :: [Nombre] -> [SymbolInfo] -> SymTab -> SymTab
insertSymbols [] _ symTab = symTab
insertSymbols (id:ids) (info:infos) (SymTab table)
    -- | isNothing (M.lookup id table) = insertSymbols ids infos newSymTab
    -- | otherwise = SymTab $ M.insert id (info : (fromJust (M.lookup id table))) table
    | isJust (M.lookup id table) = insertSymbols ids infos upd
    | otherwise = insertSymbols ids infos new
    
    where
        -- Tabla de simbolos con el identificador insertado
        new = SymTab $ M.insert id [info] table
        upd = SymTab $ M.insert id (info:fromJust (M.lookup id table)) table
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupInSymTab :: Nombre -> SymTab -> Maybe [SymbolInfo]
lookupInSymTab var (SymTab table) = M.lookup var table
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Busca los identificadores de una variable en la tabla de simbolos dada.
lookupInSymTab' :: [Nombre] -> SymTab -> [Maybe [SymbolInfo]]
lookupInSymTab' [] _ = [Nothing]
lookupInSymTab' [x] symtab = [lookupInSymTab x symtab]
lookupInSymTab' (x:xs) symtab = lookupInSymTab x symtab:lookupInSymTab' xs symtab
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Añade las variables a la tabla de simbolos
insertDeclarations :: [Nombre] -> Tipo -> MonadSymTab ()
insertDeclarations ids t = do
    (actualSymTab, scopes@(scope:_)) <- get
    let info = [SymbolInfo t scope Variable]
    addToSymTab ids info actualSymTab scopes
--------------------------------------------------------------------------------
