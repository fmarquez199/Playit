{-
Modulo para la creacion y manejo de la tabla de simbolos

* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.SymbolTable where

import Control.Monad.Trans.RWS
import Control.Monad (void,forM)
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
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupInSymTab :: Nombre -> SymTab -> Maybe [SymbolInfo]
lookupInSymTab var (SymTab table) = M.lookup var table
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Busca el (identificador,scope) de una variable en la tabla de simbolos dada.
--lookupNameScopeInSymTab :: Nombre -> Alcance -> SymTab -> Maybe [SymbolInfo]
--lookupNameScopeInSymTab name scope (SymTab table) = M.lookup name table
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
--lookupNamesScopeInSymTab :: [SymbolInfo] -> Alcance-> Bool
--lookupNamesScopeInSymTab (info:[]) scope = if getScope info == scope then True else False
--lookupNamesScopeInSymTab (info:r) scope = if getScope info == scope then True else (lookupNamesScopeInSymTab r scope)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupScopesInSymInfos :: [Alcance]-> Maybe [SymbolInfo] ->  Maybe SymbolInfo
lookupScopesInSymInfos scopes Nothing = Nothing
lookupScopesInSymInfos scopes (Just r) 
    | lstAlcances == []  = Nothing
    | otherwise = Just $ fst $ head lstAlcances
    where
        lstAlcances = [(s,a) | s <- r, a <- scopes,getScope s == a]
        
--------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupScopesNameInSymTab :: [Alcance] -> Nombre -> SymTab-> Maybe SymbolInfo
lookupScopesNameInSymTab scopes nombre symtab  = lookupScopesInSymInfos scopes (lookupInSymTab nombre symtab)
        
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
insertDeclarations :: [Nombre] -> Tipo -> SecuenciaInstr -> MonadSymTab SecuenciaInstr
insertDeclarations ids t asigs = do

    (actualSymTab, activeScopes@(activeScope:_), scope) <- get

    _ <- forM ids $ \id -> do
        _ <- if isJust $ lookupScopesNameInSymTab [activeScope] id actualSymTab then do 
                error $ "Error: redeclaración de \'" ++ id ++ "\'" 
            else return ()
            
        return id
    
    let info = replicate (length ids) (SymbolInfo t activeScope Variable)
    addToSymTab ids info actualSymTab activeScopes scope
    return asigs
--------------------------------------------------------------------------------

