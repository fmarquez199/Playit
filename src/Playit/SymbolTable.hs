{-
Modulo para la creacion y manejo de la tabla de simbolos

* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.SymbolTable where

import Control.Monad.Trans.RWS
import Control.Monad (void,forM,when)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)
import Playit.Types


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Creacion y manejo de la tabla de simbolos
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Estado inicial con todo lo predefinido del lenguaje
initState :: (SymTab,ActiveScopes,Alcance)
initState = createInitSymTab (SymTab M.empty)


createInitSymTab :: SymTab -> (SymTab,ActiveScopes,Alcance)
createInitSymTab st = (insertSymbols symbols info st,[0],0)
    where
        symbols = t ++ words
        t = ["Power", "Skill", "Rune", "Runes", "Battle", "Inventory", "Items",
            "Kit of"]
        words = ["Win", "Lose", "free", "puff", "DeathZone", "boss", "monster",
            "controller", "drop", "joystick", "Button", "notPressed", "kill",
            "lock", "play", "gameOver", "keepPlaying", "spawn", "summon",
            "unlock", "world", "portalRunesToRune", "portalRuneToRunes",
            "portalPowerToRunes", "portalSkillToRunes", "portalRunesToPower",
            "portalRunesToSkill"]
        info = tI ++ wI
        tI = [power, skill, rune, runes, battle, inventory, items, listOf
            ]
        wI = [bools, bools, apt, apt, apt, proc, func, for, out, input, ifElse,
            ifElse, kill, lock, play, break, continue, spawn, apt, unlock, world,
            portalSC, portalCS, portalIS, portalFS, portalSI, portalSF]
        power = SymbolInfo TInt 0 Tipos []
        skill = SymbolInfo TFloat 0 Tipos []
        rune = SymbolInfo TChar 0 Tipos []
        runes = SymbolInfo TStr 0 Tipos []
        battle = SymbolInfo TBool 0 Tipos []
        listOf = SymbolInfo (TLista TDummy) 0 Tipos [] -- Tipo?
        inventory = SymbolInfo TRegistro 0 ConstructoresTipos []
        items = SymbolInfo TUnion 0 ConstructoresTipos []
        bools = SymbolInfo TBool 0 Constantes []
        apt = SymbolInfo (TApuntador TDummy) 0 Apuntadores [] -- Tipo?
        proc = SymbolInfo TDummy 0 Procedimientos [] -- Tipo?
        func = SymbolInfo TDummy 0 Funciones [] -- Tipo?
        portalSC = SymbolInfo TChar 0 Funciones []
        portalCS = SymbolInfo TStr 0 Funciones []
        portalIS = SymbolInfo TStr 0 Funciones []
        portalFS = SymbolInfo TStr 0 Funciones []
        portalSI = SymbolInfo TInt 0 Funciones []
        portalSF = SymbolInfo TFloat 0 Funciones []
        for = SymbolInfo TDummy 0 Constantes [] -- Categoria y tipo?
        out = SymbolInfo TStr 0 Constantes [] -- Categoria?
        input = SymbolInfo TDummy 0 Constantes [] -- Categoria y tipo?
        ifElse = SymbolInfo TBool 0 Constantes [] -- Categoria?
        kill = SymbolInfo TDummy 0 Constantes [] -- Categoria y tipo?
        lock = SymbolInfo TDummy 0 Constantes [] -- Categoria y tipo?
        play = SymbolInfo TDummy 0 Constantes [] -- Categoria y tipo?
        break = SymbolInfo TDummy 0 Constantes [] -- Categoria y tipo?
        continue = SymbolInfo TDummy 0 Constantes [] -- Categoria y tipo?
        spawn = SymbolInfo TDummy 0 Constantes [] -- Categoria y tipo?
        unlock = SymbolInfo TDummy 0 Constantes [] -- Categoria y tipo?
        world = SymbolInfo TDummy 0 Constantes [] -- Categoria y tipo?
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Se empila el nuevo alcance
pushNewScope :: MonadSymTab ()
pushNewScope = do
    (actualSymTab, activeScopes, scope) <- get
    let newScope = scope + 1
    put (actualSymTab, newScope:activeScopes, newScope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Se desempila el alcance actual
popScope :: MonadSymTab ()
popScope = do
    (actualSymTab, _:prevScopes, scope) <- get
    put (actualSymTab, prevScopes, scope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Agrega a la tabla de simbolos la lista de identificadores con su informacion:
--  Tipo, alcance, categoria
addToSymTab :: [Nombre] -> [SymbolInfo] -> SymTab -> ActiveScopes -> Alcance
            -> MonadSymTab ()
addToSymTab ids info actualSymTab activeScopes scope = 
    put (insertSymbols ids info actualSymTab, activeScopes, scope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Añade las variables a la tabla de simbolos
insertDeclarations :: [(Nombre, Posicion)] -> Tipo -> SecuenciaInstr
                    -> MonadSymTab SecuenciaInstr
insertDeclarations ids t asigs = do
    (symTab, activeScopes@(activeScope:_), scope) <- get
    file <- ask

    checkedIds <- forM ids $ \(id,p) -> do
        let idScopeInfo = lookupInScopes [activeScope] id symTab
        
        when (isJust idScopeInfo) $
            let info = fromJust $ lookupInSymTab id symTab
                scopeInfo = [i | i <- info, getScope i == activeScope]
                
                idCategories = map getCategory scopeInfo
                categorias = [Variable, Parametros Valor, Parametros Referencia]
                isInAnyCategory = any (`elem` idCategories) categorias
                
                idScopes = map getScope scopeInfo
                isInActualScope = getScope (fromJust idScopeInfo) `elem` idScopes
            in
            when (isInAnyCategory && isInActualScope) $
                error $ "\n\nError: " ++ file ++ ": " ++ show p ++
                    "\n\tVariable '" ++ id ++ "' ya esta declarada.\n"
        return id
    
    let info = replicate (length ids) (SymbolInfo t activeScope Variable [])
    addToSymTab checkedIds info symTab activeScopes scope
    return asigs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupInSymTab :: Nombre -> SymTab -> Maybe [SymbolInfo]
lookupInSymTab var (SymTab table) = M.lookup var table
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Busca los identificadores de una variable en la tabla de simbolos dada.
lookupInSymTab' :: [Nombre] -> SymTab -> [Maybe [SymbolInfo]]
lookupInSymTab' [] _ = [Nothing]
lookupInSymTab' [x] symtab = [lookupInSymTab x symtab]
lookupInSymTab' (x:xs) symtab = lookupInSymTab x symtab:lookupInSymTab' xs symtab
-------------------------------------------------------------------------------

        
-------------------------------------------------------------------------------
-- Busca el identificador dentro de su cadena estatica
lookupInScopes :: [Alcance] -> Nombre -> SymTab -> Maybe SymbolInfo
lookupInScopes scopes nombre symtab =
    lookupInScopes' scopes (lookupInSymTab nombre symtab)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Busca la informacion dentro de la cadena estatica
lookupInScopes' :: [Alcance]-> Maybe [SymbolInfo] ->  Maybe SymbolInfo
lookupInScopes' scopes Nothing = Nothing
lookupInScopes' scopes (Just symInfo) 
    | null symScopes  = Nothing
    | otherwise = Just $ fst $ head symScopes
    where
        symScopes = [(s,a) | s <- symInfo, a <- scopes, getScope s == a]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
updateType :: Nombre -> Alcance -> Tipo -> MonadSymTab ()
updateType n a t = do
    (symTab@(SymTab table), scopes, scope) <- get
    let infos = lookupInSymTab n symTab
    when (isJust infos) $ do
        let isTarget sym = getScope sym == a
            updateType' = 
                fmap (\sym -> if isTarget sym then modifyType sym t else sym)
        put(SymTab $ M.adjust updateType' n table, scopes, scope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- 
modifyType :: SymbolInfo -> Tipo -> SymbolInfo
modifyType (SymbolInfo _ s c ei) newT = SymbolInfo newT s c ei
-- modifyType symbolInfo _ = symbolInfo
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Actualiza la informacion extra del simbolo con la nueva dependiendo de la
-- categoria a la que pertenece
-- NOTA: Es primeramente para agregar como informacion extra en las subrutinas
--      su AST y parametros, puede necesitar modificarse si se quiere usar para
--      algo diferente
updateExtraInfo :: Nombre -> Categoria -> [ExtraInfo] -> MonadSymTab ()
updateExtraInfo name category extraInfo = do
    (symTab@(SymTab table), scopes, scope) <- get
    let infos = lookupInSymTab name symTab
    when (isJust infos) $ do
        let isTarget sym = getCategory sym == category
            updateExtraInfo' = 
                fmap (\sym -> if isTarget sym then modifyExtraInfo sym extraInfo else sym)
        put(SymTab $ M.adjust updateExtraInfo' name table, scopes, scope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Actualiza la informacion extra del simbolo con la nueva
modifyExtraInfo :: SymbolInfo -> [ExtraInfo] -> SymbolInfo
modifyExtraInfo (SymbolInfo t s c []) extraInfo = SymbolInfo t s c extraInfo
modifyExtraInfo (SymbolInfo t s c ei) extraInfo = SymbolInfo t s c (extraInfo ++ ei)
-------------------------------------------------------------------------------
