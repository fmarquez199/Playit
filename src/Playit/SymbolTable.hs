
{- |
 * Creates and handle the symbol table
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.SymbolTable where

import Control.Monad (when,forM_)
import Control.Monad.Trans.RWS
import Data.List (findIndices)
import qualified Data.Map as M
import Data.Maybe (fromJust,isJust,isNothing)
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
createInitSymTab st = (insertSymbols symbols info st, [1,0], 1,[])
  where
    symbols = ["Power", "Skill", "Rune", "Runes", "Battle", "Inventory",
        "Items", "Kit of", "Win", "Lose", "DeathZone", "portalRunesToRune",
        "portalRuneToRunes", "portalPowerToRunes", "portalSkillToRunes",
        "portalRunesToPower", "portalRunesToSkill"]
    info = [power, skill, rune, runes, battle, inventory, items, listOf,
        win, lose, apt, portalSC, portalCS, portalIS, portalFS,
        portalSI, portalSF]
    power     = SymbolInfo "Power" TInt 0 Types []
    skill     = SymbolInfo "Skill" TFloat 0 Types []
    rune      = SymbolInfo "Rune" TChar 0 Types []
    runes     = SymbolInfo "Runes" TStr 0 Types []
    battle    = SymbolInfo "Battle" TBool 0 Types []
    listOf    = SymbolInfo "Kit of" (TList TDummy) 0 TypeConstructors [] -- Tipo?
    inventory = SymbolInfo "Inventory" TRegister 0 TypeConstructors []
    items     = SymbolInfo "Items" TUnion 0 TypeConstructors []
    win       = SymbolInfo "Win" TBool 0 Constants []
    lose      = SymbolInfo "Lose" TBool 0 Constants []
    apt       = SymbolInfo "DeathZone" (TPointer TDummy) 0 Pointers [] -- Tipo?
    portalCS  = SymbolInfo "portalRuneToRunes" TStr 0 Functions [Params [(TChar,"rune")]]
    portalIS  = SymbolInfo "portalPowerToRunes" TStr 0 Functions [Params [(TInt,"power")]]
    portalFS  = SymbolInfo "portalSkillToRunes" TStr 0 Functions [Params [(TFloat,"skill")]]
    portalSI  = SymbolInfo "portalRunesToPower" TInt 0 Functions [Params [(TStr,"runes")]]
    portalSC  = SymbolInfo "portalRunesToRune" TChar 0 Functions [Params [(TStr,"runes")]]
    portalSF  = SymbolInfo "portalRunesToSkill" TFloat 0 Functions [Params [(TStr,"runes")]]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
pushNewScope :: MonadSymTab ()
pushNewScope = do
  (actualSymTab, activeScopes, scope, promises) <- get
  let newScope = scope + 1
  put (actualSymTab, newScope:activeScopes, newScope, promises)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
popScope :: MonadSymTab ()
popScope = do
  (actualSymTab, _:prevScopes, scope, promises) <- get
  put (actualSymTab, prevScopes, scope, promises)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                              Insertions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts the ids into symbol table with its symbol info
addToSymTab :: [Id] -> [SymbolInfo] -> SymTab -> ActiveScopes -> Scope
            -> Promises -> MonadSymTab ()
addToSymTab ns info actualSymTab activeScopes scope promises = 
    put (insertSymbols ns info actualSymTab, activeScopes, scope, promises)

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
insertDeclarations :: [(Id, Pos)] -> Type -> [(Instr,Pos)] -> MonadSymTab InstrSeq
insertDeclarations ids t asigspos = do
  (symTab, activeScopes@(activeScope:_), scope, promises) <- get
  fileCode <- ask
  let
    asigs = map fst asigspos
    ids'    = (map fst ids)
    idsInfo = lookupInSymTab' ids' symTab

  if all (==Nothing) idsInfo then
  -- Add ids if none var declared
    let 
      idInfo = SymbolInfo {-TODO verga este id-} t activeScope Variables []
      idsInfo' = replicate (length ids) idInfo

    in addToSymTab ids' idsInfo' symTab activeScopes scope promises
  else
  -- Get the first id from all declared vars in active scope
    let
      redefs = concatMap fromJust $ filter isJust idsInfo
      vars = [Variables, Parameters Value, Parameters Reference]
      isVar redef = getCategory redef `elem` vars
      redefs' = filter isVar redefs
      redefsScopes = map getScope redefs'
      redefsIndexs = findIndices isJust idsInfo
      isInActualScope = activeScope `elem` redefsScopes
    in
      if isInActualScope then
        let p = snd $ ids !! head redefsIndexs
        in tell [errorMsg "Redefined variable" fileCode p]
      else
        let
          idsInScope = [i | i<-ids',index<-redefsIndexs,i== ids' !! index]
          idInfo = SymbolInfo {-TODO verga este id-} t activeScope Variables []
          idsInfo' = replicate (length ids) idInfo
        in addToSymTab idsInScope idsInfo' symTab activeScopes scope promises

  return asigs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts the subroutine's name into symbol table
defineSubroutine :: Id -> Category -> Pos -> MonadSymTab ()
defineSubroutine id category p = do
  (symTab, activeScopes, scope, promises) <- get
  fileCode <- ask
  let infos = lookupInSymTab id symTab

  if isNothing infos then 
    let info = [SymbolInfo id TDummy 1 category []]
    in addToSymTab [id] info symTab activeScopes scope promises
  else
    let promise = getPromise id promises
    in
      if isJust promise then
        let promise' = fromJust promise
        in
        if getTypePromise promise' /= TVoid && category == Procedures then
          tell [errorMsg ("'"++id++"' is not a function") fileCode (getPosPromise promise')]
        else
          when (getTypePromise promise' == TVoid && category == Functions) $
            tell [errorMsg ("'"++id++"' is not a procedure") fileCode (getPosPromise promise')]
      else
        tell [errorMsg ("Redefined subroutine"++show infos) fileCode p]

  return ()
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts a subroutine's parameter into symbol table
defineParameter :: Var -> Pos -> MonadSymTab (Type,Id)
defineParameter (Param name t ref) p = do
  (symTab, activeScopes@(activeScope:_), scope, promises) <- get
  fileCode <- ask
  let
    infos = lookupInScopes [activeScope] name symTab
    param = (t,name)

  if isJust infos then
    tell [errorMsg "Redefined parameter" fileCode p] >> return param
  else
    let info = [SymbolInfo name t activeScope (Parameters ref) []]
    in addToSymTab [name] info symTab activeScopes scope promises >> return param
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts a register / union into symbol table
defineRegUnion :: Id -> Type -> [ExtraInfo] -> Pos -> MonadSymTab (Id, Pos)
defineRegUnion reg regType extraInfo p = do
  (symTab, activeScopes, scope, promises) <- get
  fileCode <- ask
  let regInfo = lookupInScopes [1] reg symTab

  if isJust regInfo then
    if regType == TRegister then
      tell [errorMsg "Redefined Inventory" fileCode p] >> return (reg, p)
    else
      tell [errorMsg "Redefined Items" fileCode p] >> return (reg, p)
  else
    let
      info      = [SymbolInfo reg regType 1 TypeConstructors extraInfo]
      promise   =  getPromise reg promises
      npromises =
        if isJust promise then
          let promise' = fromJust promise in filter (/=promise') promises
        else promises
    in
      addToSymTab [reg] info symTab activeScopes scope npromises >> return (reg,p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
updatesDeclarationsCategory :: Id -> MonadSymTab ()
updatesDeclarationsCategory reg = do
  (SymTab table, activeScopes@(activeScope:_), scope, promises) <- get
  let
    modifySym (SymbolInfo reg t s _ _) = SymbolInfo reg t s Fields [FromReg reg]
    updtSym = 
      map (\sym -> if getScope sym == activeScope then modifySym sym else sym)

  put(SymTab $ M.map updtSym table, activeScopes, scope, promises)
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
updateType symbol scope t = do
  (symTab@(SymTab table), activeScopes, scopes, promises) <- get
  fileCode <- ask
  let infos = lookupInSymTab symbol symTab
  when (isJust infos) $ do
    let
      isTarget symbol' = getScope symbol' == scope
      updateType' = fmap (\sym -> if isTarget sym then modifyType sym t else sym)
      updatedSymTab = SymTab $ M.adjust updateType' symbol table

    put(updatedSymTab, activeScopes, scopes, promises)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Updates the symbol category
updateCategory :: Id -> Scope -> Category -> MonadSymTab ()
updateCategory symbol scope cat = do
  (symTab@(SymTab table), activeScopes, scopes, promises) <- get
  fileCode <- ask
  let
    infos = lookupInSymTab symbol symTab

  when (isJust infos) $ do
    let
      isTarget symbol' = getScope symbol' == scope
      updateCategory' = fmap (\sym -> if isTarget sym then modifyCategory sym cat else sym)
      updatedSymTab = SymTab $ M.adjust updateCategory' symbol table

    put(updatedSymTab, activeScopes, scopes, promises)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Updates the symbol extra info, depends of its category
updateExtraInfo :: Id -> Category -> [ExtraInfo] -> MonadSymTab ()
updateExtraInfo sym category extraInfo = do
  (symTab@(SymTab table), scopes, scope, promises) <- get
  let infos = lookupInSymTab sym symTab

  when (isJust infos) $ do
    let
      isTarget sym = getCategory sym == category
      updateExtraInfo' = 
        fmap (\sym -> if isTarget sym then modifyExtraInfo sym extraInfo else sym)
            
    put(SymTab $ M.adjust updateExtraInfo' sym table, scopes, scope, promises)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                                Checks
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
checkPromises ::  MonadSymTab ()
checkPromises = do
  (symTab, activeScopes, scopes , promises) <- get
  fileCode <- ask

  forM_ promises $ \t ->
    case t of
      PromiseSubroutine {} -> printErrorPromiseFunction t fileCode
      (PromiseUserDefinedType name pos) -> 
        tell [errorMsg ("Type '" ++ name ++ "' wasn't defined") fileCode pos]
-------------------------------------------------------------------------------


showParamsPF:: [(Type,Pos)] -> String
showParamsPF [] = ""
showParamsPF [(t,p)] = show t
showParamsPF ((t,p):r)  = show t ++ "," ++ showParamsPF r


printErrorPromiseFunction :: Promise -> FileCodeReader -> MonadSymTab ()
printErrorPromiseFunction (PromiseSubroutine name args t cat pc _ _ _)  fileCode = 
  if cat == Functions then
    tell [errorMsg ("Function '" ++ name ++ "(" ++ showParamsPF args ++ ") ->" ++ show t ++ "'  wasn't defined") fileCode pc]
  else 
    tell [errorMsg ("Procedure '" ++ name ++ "(" ++ showParamsPF args ++ ")' wasn't defined") fileCode pc]

