
{-
Modulo para la creacion y manejo de la tabla de simbolos
* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.SymbolTable where

import Control.Monad.Trans.RWS
import Control.Monad (forM, when)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Playit.Types
--import Playit.ErrorM


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
        symbols = ["Power", "Skill", "Rune", "Runes", "Battle", "Inventory",
            "Items", "Kit of", "Win", "Lose", "DeathZone", "portalRunesToRune",
            "portalRuneToRunes", "portalPowerToRunes", "portalSkillToRunes",
            "portalRunesToPower", "portalRunesToSkill"]
        info = [power, skill, rune, runes, battle, inventory, items, listOf,
            bools, bools, apt, portalSC, portalCS, portalIS, portalFS,
            portalSI, portalSF]
        power = SymbolInfo TInt 0 Tipos []
        skill = SymbolInfo TFloat 0 Tipos []
        rune = SymbolInfo TChar 0 Tipos []
        runes = SymbolInfo TStr 0 Tipos []
        battle = SymbolInfo TBool 0 Tipos []
        listOf = SymbolInfo (TLista TDummy) 0 ConstructoresTipos [] -- Tipo?
        inventory = SymbolInfo TRegistro 0 ConstructoresTipos []
        items = SymbolInfo TUnion 0 ConstructoresTipos []
        bools = SymbolInfo TBool 0 Constantes []
        apt = SymbolInfo (TApuntador TDummy) 0 Apuntadores [] -- Tipo?
        portalSC = SymbolInfo TChar 0 Funciones []
        portalCS = SymbolInfo TStr 0 Funciones []
        portalIS = SymbolInfo TStr 0 Funciones []
        portalFS = SymbolInfo TStr 0 Funciones []
        portalSI = SymbolInfo TInt 0 Funciones []
        portalSF = SymbolInfo TFloat 0 Funciones []
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
addToSymTab ns info actualSymTab activeScopes scope = 
    put (insertSymbols ns info actualSymTab, activeScopes, scope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Inserta los identificadores con su tipo en la tabla de simbolos dada
insertSymbols :: [Nombre] -> [SymbolInfo] -> SymTab -> SymTab
insertSymbols [] _ symTab = symTab
insertSymbols (n:ns) (info:infos) (SymTab table)
    | M.member n table = insertSymbols ns infos updSymTab
    | otherwise = insertSymbols ns infos newSymTab
    
    where
        -- Tabla de simbolos con el identificador insertado
        newSymTab = SymTab $ M.insert n [info] table
        updSymTab = SymTab $ M.adjust (info:) n table
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Añade las variables a la tabla de simbolos
insertDeclarations :: [(Nombre,Posicion)] -> Tipo -> SecuenciaInstr -> MonadSymTab SecuenciaInstr
insertDeclarations ids t asigs = do
    (symTab, activeScopes@(activeScope:_), scope) <- get
    file <- ask

    checkedIds <- forM ids $ \(id',p) -> do
        let idScopeInfo = lookupInScopes [activeScope] id' symTab
        
        when (isJust idScopeInfo) $
            let info = fromJust $ lookupInSymTab id' symTab
                scopeInfo = [i | i <- info, getScope i == activeScope]
                
                idCategories = map getCategory scopeInfo
                categorias = [Variable, Parametros Valor, Parametros Referencia]
                isInAnyCategory = any (`elem` idCategories) categorias
                
                idScopes = map getScope scopeInfo
                isInActualScope = getScope (fromJust idScopeInfo) `elem` idScopes
            in
            when (isInAnyCategory && isInActualScope) $
                error $ "\n\nError: " ++ file ++ ": " ++ show p ++
                    "\n\tVariable '" ++ id' ++ "' ya esta declarada.\n"
        return id'
    
    let info = replicate (length ids) (SymbolInfo t activeScope Variable [Nada])
    addToSymTab ids' info actualSymTab activeScopes scope
    return asigs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupInSymTab :: Nombre -> SymTab -> Maybe [SymbolInfo]
lookupInSymTab var (SymTab table) = M.lookup var table
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------

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
lookupInScopes' _ Nothing = Nothing
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
updateExtraInfo :: Nombre -> Alcance -> [ExtraInfo] -> MonadSymTab ()
updateExtraInfo name scope extraInfo = do

    (symTab@(SymTab table), scopes, lastScope) <- get
    
    -- Obtenemos todos los simbolos asociados al nombre    
    let infos = lookupInSymTab name symTab
    
    if isJust infos then do
        let isTargetSymbol sym = getScope sym == scope
            updateExtraInfo' = fmap (\sym -> if isTargetSymbol sym then modifiExtraInfoSymbol sym extraInfo else sym)
        put(SymTab $ M.adjust updateExtraInfo' name table, scopes, lastScope)

-------------------------------------------------------------------------------
-- Actualiza la informacion extra del simbolo con la nueva
modifyExtraInfo :: SymbolInfo -> [ExtraInfo] -> SymbolInfo
modifyExtraInfo (SymbolInfo t s c []) extraInfo = SymbolInfo t s c extraInfo
modifyExtraInfo (SymbolInfo t s c ei) extraInfo = SymbolInfo t s c (ei ++ extraInfo)
-------------------------------------------------------------------------------
