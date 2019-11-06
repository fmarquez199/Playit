
{- |
 * Creates and handle the symbol table
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.SymbolTable where

import Control.Monad.Trans.RWS
import Control.Monad (void,when)
import qualified Data.Map as M
import Data.List (findIndices)
import Data.Maybe (fromJust, isJust, isNothing)
import Playit.AuxFuncs
import Playit.Errors
import Playit.Types


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                         Symbol table handlers
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Initial state with prelude
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
        portalCS  = SymbolInfo TStr 0 Functions [Params [(TChar,"rune")]]
        portalIS  = SymbolInfo TStr 0 Functions [Params [(TInt,"power")]]
        portalFS  = SymbolInfo TStr 0 Functions [Params [(TFloat,"skill")]]
        portalSI  = SymbolInfo TInt 0 Functions [Params [(TStr,"runes")]]
        portalSC  = SymbolInfo TChar 0 Functions [Params [(TStr,"runes")]]
        portalSF  = SymbolInfo TFloat 0 Functions [Params [(TStr,"runes")]]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
pushNewScope :: MonadSymTab ()
pushNewScope = do
    (actualSymTab, activeScopes, scope) <- get
    let newScope = scope + 1
    put (actualSymTab, newScope:activeScopes, newScope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
popScope :: MonadSymTab ()
popScope = do
    (actualSymTab, _:prevScopes, scope) <- get
    put (actualSymTab, prevScopes, scope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                              Insertions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts the ids into symbol table with its symbol info
addToSymTab :: [Id] -> [SymbolInfo] -> SymTab -> ActiveScopes -> Scope -> MonadSymTab ()
addToSymTab ns info actualSymTab activeScopes scope = 
    put (insertSymbols ns info actualSymTab, activeScopes, scope)

insertSymbols :: [Id] -> [SymbolInfo] -> SymTab -> SymTab
insertSymbols [] _ symTab = symTab
insertSymbols (n:ns) (info:infos) (SymTab table)
    | M.member n table = insertSymbols ns infos updSymTab
    | otherwise = insertSymbols ns infos newSymTab
    
    where
        -- Symbol table with the id inserted
        newSymTab = SymTab $ M.insert n [info] table
        updSymTab = SymTab $ M.adjust (info:) n table
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Insert the declared variables into symbol table
insertDeclarations :: [(Id, Pos)] -> Type -> InstrSeq -> MonadSymTab InstrSeq
insertDeclarations ids t asigs = do
    (symTab, activeScopes@(activeScope:_), scope) <- get
    fileCode <- ask
    let ids' = (map fst ids)
        idsInfo = lookupInSymTab' ids' symTab

    if all (==Nothing) idsInfo then
    -- Add ids if none var declared
        let 
            idInfo = SymbolInfo t activeScope Variables []
            idsInfo' = replicate (length ids) idInfo

        in addToSymTab ids' idsInfo' symTab activeScopes scope
    else
    -- Get the first id from all declared vars in active scope
        let redefs = concatMap fromJust $ filter isJust idsInfo
            isVar si = getCategory si == Variables
            redefs' = filter isVar redefs
            redefsScopes = map getScope redefs'
            redefsIndexs = findIndices isJust idsInfo
            isInActualScope = activeScope `elem` redefsScopes
        in
        if isInActualScope then
            let p = snd $ ids !! head redefsIndexs
            in error $ errorMsg "Redefined variable" fileCode p
        else
            let idsInScope = [i | i<-ids',index<-redefsIndexs,i== ids' !! index]
                idInfo = SymbolInfo t activeScope Variables []
                idsInfo' = replicate (length ids) idInfo
            in addToSymTab idsInScope idsInfo' symTab activeScopes scope

    return asigs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts the subroutine's name into symbol table
defineSubroutine :: Id -> Category -> Pos -> MonadSymTab ()
defineSubroutine id category p = do
    (symTab, activeScopes, scope) <- get
    fileCode <- ask
    let infos = lookupInSymTab id symTab

    if isNothing infos then 
        let info = [SymbolInfo TDummy 1 category []]
        in addToSymTab [id] info symTab activeScopes scope
    else
        error $ errorMsg "Redefined subroutine" fileCode p
    return ()
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts a subroutine's parameter into symbol table
defineParameter :: Var -> Pos -> MonadSymTab (Type,Id)
defineParameter (Param name t ref) p = do
    (symTab, activeScopes@(activeScope:_), scope) <- get
    fileCode <- ask
    let infos = lookupInScopes [activeScope] name symTab

    if isJust infos then
        error $ errorMsg "Redefined parameter" fileCode p
    else do
        let info = [SymbolInfo t activeScope (Parameters ref) []]
        addToSymTab [name] info symTab activeScopes scope
        return (t,name)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts a register / union into symbol table
defineRegUnion :: Id -> Type -> Pos -> MonadSymTab ()
defineRegUnion reg regType p = do
    (symTab@(SymTab table), activeScopes@(activeScope:_), scope) <- get
    fileCode <- ask
    let regInfo = lookupInScopes [1] reg symTab

    if isJust regInfo then
        if regType == TRegister then
            error $ errorMsg "Redefined Inventory" fileCode p
        else
            error $ errorMsg "Redefined Items" fileCode p
    else
        let modifySym (SymbolInfo t s _ _) = SymbolInfo t s Fields [FromReg reg]
            updtSym = 
                map (\sym -> if getScope sym == activeScope then modifySym sym else sym)

            newSymTab = SymTab $ M.map updtSym table
            info = [SymbolInfo regType 1 TypeConstructors []]

        in void $ addToSymTab [reg] info newSymTab activeScopes scope
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                               Look ups
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Look up the symbol in the symbol table
lookupInSymTab :: Id -> SymTab -> Maybe [SymbolInfo]
lookupInSymTab sym (SymTab table) = M.lookup sym table
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Look up the symbols in the symbol table
lookupInSymTab' :: [Id] -> SymTab -> [Maybe [SymbolInfo]]
lookupInSymTab' [] _ = [Nothing]
lookupInSymTab' [x] symTab = [lookupInSymTab x symTab]
lookupInSymTab' (x:xs) symTab = lookupInSymTab x symTab:lookupInSymTab' xs symTab
-------------------------------------------------------------------------------

        
-------------------------------------------------------------------------------
-- | Look up the symbol in its static chain
lookupInScopes :: [Scope] -> Id -> SymTab -> Maybe [SymbolInfo]
lookupInScopes scopes sym symTab
    | isNothing symInfo = Nothing
    | otherwise =
        if null symInfos then Nothing
        else Just symInfos
    
    where
        symInfo = lookupInSymTab sym symTab
        symInfos = [si | si<-fromJust symInfo,s<-scopes,getScope si `elem` [s,0]] 
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                               Updates
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Updates the symbol type
updateType :: Id -> Scope -> Type -> MonadSymTab ()
updateType sym scope t = do
    (symTab@(SymTab table), scopes, scope) <- get
    let infos = lookupInSymTab sym symTab
    
    when (isJust infos) $ do
        let isTarget sym = getScope sym == scope
            updateType' = 
                fmap (\sym -> if isTarget sym then modifyType sym t else sym)
        
        put(SymTab $ M.adjust updateType' sym table, scopes, scope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Updates the symbol extra info, depends of its category
updateExtraInfo :: Id -> Category -> [ExtraInfo] -> MonadSymTab ()
updateExtraInfo sym category extraInfo = do
    (symTab@(SymTab table), scopes, scope) <- get
    let infos = lookupInSymTab sym symTab

    when (isJust infos) $ do
        let isTarget sym = getCategory sym == category
            updateExtraInfo' = 
                fmap (\sym -> if isTarget sym then modifyExtraInfo sym extraInfo else sym)
                
        put(SymTab $ M.adjust updateExtraInfo' sym table, scopes, scope)
-------------------------------------------------------------------------------
