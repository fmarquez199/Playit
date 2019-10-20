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
        symbols = t ++ words
        t = ["Power", "Skill", "Rune", "Runes", "Battle", "Inventory", "Items"]
        words = ["Win", "Lose", "free", "puff"]
        info = ti ++ wi
        ti = [pInfo, sInfo, rInfo, rsInfo, bInfo, inventoryInfo, itemsInfo]
        wi = [boolsInfo, boolsInfo, aptInfo, aptInfo]
        pInfo = SymbolInfo TInt 0 Tipos
        sInfo = SymbolInfo TFloat 0 Tipos
        rInfo = SymbolInfo TChar 0 Tipos
        rsInfo = SymbolInfo TStr 0 Tipos
        bInfo = SymbolInfo TBool 0 Tipos
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
    | M.member id table = insertSymbols ids infos updSymTab
    | otherwise = insertSymbols ids infos newSymTab
    
    where
        -- Tabla de simbolos con el identificador insertado
        newSymTab = SymTab $ M.insert id [info] table
        updSymTab = SymTab $ M.adjust (info:) id table
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupInSymTab :: Nombre -> SymTab -> Maybe [SymbolInfo]
lookupInSymTab var (SymTab table) = M.lookup var table
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Busca el (identificador,scope) de una variable en la tabla de simbolos dada.
{-lookupNameScopeInSymTab :: Nombre -> Alcance -> SymTab -> Maybe [SymbolInfo]
lookupNameScopeInSymTab name scope (SymTab table) = fmap (elem scope) (M.lookup name table)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupNameScopeInSymTab :: [SymbolInfo] -> Alcance-> Boolean
lookupNameScopeInSymTab (info:[]) scope = if getScope info == scope then True else False
lookupNameScopeInSymTab (info:r) scope = if getScope info == scope then True else (lookupNameScopeInSymTab r scope)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupNameScopesInSymTab :: [SymbolInfo] -> Alcance-> Boolean
lookupNameScopesInSymTab (info:[]) scopes = if getScope info == scope then True else False
lookupNameScopesInSymTab (info:r)  scopes = if getScope info == scope then True else (lookupNameScopeInSymTab r scope)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupNameScopeInSymTab :: [SymbolInfo] -> Alcance-> Boolean
lookupNameScopeInSymTab info (sc:[])    = if getScope info == scope then True else False
lookupNameScopeInSymTab info (sc:scs)   = if getScope info == sc then info else (lookupNameScopeInSymTab r scope)
--------------------------------------------------------------------------------
-}
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
    let info = replicate (length ids) (SymbolInfo t scope Variable)
    addToSymTab ids info actualSymTab scopes
--------------------------------------------------------------------------------
