
{-
Modulo para la creacion y manejo de la tabla de simbolos

* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.SymbolTable where

import Control.Monad.Trans.RWS
import Control.Monad (when)
import qualified Data.Map as M
import Data.List (findIndices)
import Data.Maybe (fromJust, isJust, isNothing)
import Playit.Errors
import Playit.Types


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Creacion y manejo de la tabla de simbolos
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Estado inicial con todo lo predefinido del lenguaje
initState :: SymTabState
initState = createInitSymTab (SymTab M.empty)


createInitSymTab :: SymTab -> SymTabState
createInitSymTab st = (insertSymbols symbols info st, [1,0], 1)
    where
        symbols = ["Power", "Skill", "Rune", "Runes", "Battle", "Inventory",
            "Items", "Kit of", "Win", "Lose", "DeathZone", "portalRunesToRune",
            "portalRuneToRunes", "portalPowerToRunes", "portalSkillToRunes",
            "portalRunesToPower", "portalRunesToSkill"]
        info = [power, skill, rune, runes, battle, inventory, items, listOf,
            bools, bools, apt, portalSC, portalCS, portalIS, portalFS,
            portalSI, portalSF]
        power     = SymbolInfo TInt 0 Types []
        skill     = SymbolInfo TFloat 0 Types []
        rune      = SymbolInfo TChar 0 Types []
        runes     = SymbolInfo TStr 0 Types []
        battle    = SymbolInfo TBool 0 Types []
        listOf    = SymbolInfo (TList TDummy) 0 TypeConstructors [] -- Tipo?
        inventory = SymbolInfo TRegister 0 TypeConstructors []
        items     = SymbolInfo TUnion 0 TypeConstructors []
        bools     = SymbolInfo TBool 0 Constants []
        apt       = SymbolInfo (TPointer TDummy) 0 Pointers [] -- Tipo?
        portalCS  = SymbolInfo TStr 0 Functions [Params ["rune"]]
        portalIS  = SymbolInfo TStr 0 Functions [Params ["power"]]
        portalFS  = SymbolInfo TStr 0 Functions [Params ["skill"]]
        portalSI  = SymbolInfo TInt 0 Functions [Params ["runes"]]
        portalSC  = SymbolInfo TChar 0 Functions [Params ["runes"]]
        portalSF  = SymbolInfo TFloat 0 Functions [Params ["runes"]]
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
--  Type, alcance, categoria
addToSymTab :: [Id] -> [SymbolInfo] -> SymTab -> ActiveScopes -> Scope
            -> MonadSymTab ()
addToSymTab ns info actualSymTab activeScopes scope = 
    put (insertSymbols ns info actualSymTab, activeScopes, scope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Inserta los identificadores con su tipo en la tabla de simbolos dada
insertSymbols :: [Id] -> [SymbolInfo] -> SymTab -> SymTab
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
insertDeclarations :: [(Id, Pos)] -> Type -> InstrSeq
                    -> MonadSymTab InstrSeq
insertDeclarations ids t asigs = do
    (symTab, activeScopes@(activeScope:_), scope) <- get
    fileCode <- ask
    let ids' = (map fst ids)
        idsInfo = lookupInSymTab' ids' symTab

    if all (==Nothing) idsInfo then
    -- Si no hay ninguno declarado los agrego
        let 
            idInfo = SymbolInfo t activeScope Variables []
            idsInfo' = replicate (length ids) idInfo

        in addToSymTab ids' idsInfo' symTab activeScopes scope
    else
    -- Sino ver cual ya esta declarado en el alcance actual, el primero
        let redefs = concatMap fromJust $ filter isJust idsInfo
            isVar si = getCategory si == Variables
            redefs' = filter isVar redefs
            redefsScopes = map getScope redefs'
            redefsIndexs = findIndices isJust idsInfo
            isInActualScope = activeScope `elem` redefsScopes
        in
        if isInActualScope then
            let p = snd $ ids !! head redefsIndexs
            in error $ errorMessage "Redefined variable" fileCode p
        else
            let idsInScope = [i | i<-ids',index<-redefsIndexs,i== ids' !! index]
                idInfo = SymbolInfo t activeScope Variables []
                idsInfo' = replicate (length ids) idInfo
            in addToSymTab idsInScope idsInfo' symTab activeScopes scope

    return asigs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupInSymTab :: Id -> SymTab -> Maybe [SymbolInfo]
lookupInSymTab var (SymTab table) = M.lookup var table
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Busca los identificadores de una variable en la tabla de simbolos dada.
lookupInSymTab' :: [Id] -> SymTab -> [Maybe [SymbolInfo]]
lookupInSymTab' [] _ = [Nothing]
lookupInSymTab' [x] symtab = [lookupInSymTab x symtab]
lookupInSymTab' (x:xs) symtab = lookupInSymTab x symtab:lookupInSymTab' xs symtab
-------------------------------------------------------------------------------

        
-------------------------------------------------------------------------------
-- Busca el identificador dentro de su cadena estatica
lookupInScopes :: [Scope] -> Id -> SymTab -> Maybe [SymbolInfo]
lookupInScopes scopes nombre symtab
    | isNothing symInfo = Nothing
    | otherwise =
        if null symInfos then Nothing
        else Just symInfos
    
    where
        symInfo = lookupInSymTab nombre symtab
        symInfos = [si | si<-fromJust symInfo,s<-scopes,getScope si `elem` [s,0]] 
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
updateType :: Id -> Scope -> Type -> MonadSymTab ()
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
modifyType :: SymbolInfo -> Type -> SymbolInfo
modifyType (SymbolInfo _ s c ei) newT = SymbolInfo newT s c ei
-- modifyType symbolInfo _ = symbolInfo
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Actualiza la informacion extra del simbolo con la nueva dependiendo de la
-- categoria a la que pertenece
-- NOTA: Es primeramente para agregar como informacion extra en las subrutinas
--      su AST y parametros, puede necesitar modificarse si se quiere usar para
--      algo diferente
updateExtraInfo :: Id -> Category -> [ExtraInfo] -> MonadSymTab ()
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
modifyExtraInfo (SymbolInfo t s c ei) extraInfo = SymbolInfo t s c (ei ++ extraInfo)
-------------------------------------------------------------------------------
