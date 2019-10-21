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
initState :: (SymTab,ActiveScopes,Alcance)
initState = createInitSymTab (SymTab M.empty)


createInitSymTab :: SymTab -> (SymTab,ActiveScopes,Alcance)
createInitSymTab st = (insertSymbols symbols info st,[0],0)
    where
        symbols = t ++ words
        t = ["Power", "Skill", "Rune", "Runes", "Battle", "Inventory", "Items"]
        words = ["Win", "Lose", "free", "puff"]
        info = ti ++ wi
        ti = [pInfo, sInfo, rInfo, rsInfo, bInfo, inventoryInfo, itemsInfo]
        wi = [boolsInfo, boolsInfo, aptInfo, aptInfo]
        pInfo = SymbolInfo TInt 0 Tipos [Nada]
        sInfo = SymbolInfo TFloat 0 Tipos [Nada]
        rInfo = SymbolInfo TChar 0 Tipos [Nada]
        rsInfo = SymbolInfo TStr 0 Tipos [Nada]
        bInfo = SymbolInfo TBool 0 Tipos [Nada]
        inventoryInfo = SymbolInfo TRegistro 0 ConstructoresTipos [Nada]
        itemsInfo = SymbolInfo TUnion 0 ConstructoresTipos [Nada]
        boolsInfo = SymbolInfo TBool 0 Variable [Nada]
        aptInfo = SymbolInfo (TApuntador TDummy) 0 Apuntadores [Nada]
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Se empila el nuevo alcance
pushNewScope :: MonadSymTab ()
pushNewScope = do
    (actualSymTab, activeScopes, scope) <- get
    let newScope = scope + 1
    put (actualSymTab, newScope:activeScopes, newScope)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Se desempila el alcance actual
popScope :: MonadSymTab ()
popScope = do
    (actualSymTab, _:prevScopes, scope) <- get
    put (actualSymTab, prevScopes, scope)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Agrega a la tabla de simbolos la lista de identificadores con su informacion:
--  Tipo, alcance, categoria
addToSymTab :: [Nombre] -> [SymbolInfo] -> SymTab -> ActiveScopes -> Alcance
            -> MonadSymTab ()
addToSymTab ids info actualSymTab activeScopes scope = 
    put (insertSymbols ids info actualSymTab, activeScopes, scope)
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
-- Añade las variables a la tabla de simbolos
insertDeclarations :: [Nombre] -> Tipo -> SecuenciaInstr -> MonadSymTab SecuenciaInstr
insertDeclarations ids t asigs = do
    (actualSymTab, activeScopes@(activeScope:_), scope) <- get
    let info = replicate (length ids) (SymbolInfo t activeScope Variable [Nada])
    addToSymTab ids info actualSymTab activeScopes scope
    return asigs
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

updateExtraInfo :: Nombre -> Categoria -> [ExtraInfo] -> MonadSymTab ()
updateExtraInfo name category extraInfo = do
    (symTab@(SymTab table),scopes,scope) <- get
    let byCategory i = getCategory i == category
    let (SymbolInfo t s c ei) = filter byCategory $ fromJust $ lookupInSymTab name symTab
    if ei == [Nada] then do
        let newSymbolInfo = SymbolInfo t s c extraInfo
        put(SymTab $ M.adjust (newSymbolInfo:) name table,scope,scope)
        return ()
    else do
        let newSymbolInfo = SymbolInfo t s c extraInfo:ei
        put(SymTab $ M.adjust (newSymbolInfo:) name table,scope,scope)
        return ()
