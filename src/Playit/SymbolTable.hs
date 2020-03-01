
{- |
 * Creates and handle the symbol table
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.SymbolTable where

import Control.Monad           (when,forM_)
import Control.Monad.Trans.RWS
import Data.List               (findIndices)
import Data.Maybe              (fromJust,isJust,isNothing)
import Playit.AuxFuncs
import Playit.Errors
import Playit.Types
import Control.Monad.IO.Class  (liftIO)
import qualified Data.Map as M


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                         Symbol table handlers
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Initial state with prelude
stInitState :: SymTabState
stInitState = createInitSymTab (SymTab M.empty)

createInitSymTab :: SymTab -> SymTabState
createInitSymTab st = SymTabState {
    symTab = st',
    actS   = [1,0],
    currS  = 1,
    proms  = []
  }
  where
    symbols   = ["Power", "Skill", "Rune", "Runes", "Battle", "Inventory",
        "Items", "Kit of", "Win", "Lose", "DeathZone", "portalRunesToRune",
        "portalRuneToRunes", "portalPowerToRunes", "portalSkillToRunes",
        "portalRunesToPower", "portalRunesToSkill"]
    info      = [power, skill, rune, runes, battle, inventory, items, listOf,
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
    st'       = insertSymbols symbols info st
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
pushNewScope :: MonadSymTab ()
pushNewScope = do
  state@SymTabState{actS = oldS, currS = s} <- get
  let newS = s + 1
  put state{actS = newS:oldS, currS = newS}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
popScope :: MonadSymTab ()
popScope = do
  state@SymTabState{actS = _:prevScopes} <- get
  put state{actS = prevScopes}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                              Insertions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts the ids into symbol table with its symbol info
addToSymTab :: [Id] -> [SymbolInfo] -> MonadSymTab ()
addToSymTab ns info = do
  state@SymTabState{symTab = actualSymTab} <- get
  put state{symTab = insertSymbols ns info actualSymTab}

insertSymbols :: [Id] -> [SymbolInfo] -> SymTab -> SymTab
insertSymbols [] _ symTab = symTab
insertSymbols (n:ns) (info:infos) (SymTab table)
  | M.member n table = insertSymbols ns infos updSymTab
  | otherwise        = insertSymbols ns infos newSymTab
  
  where
    -- Symbol table with the id inserted
    newSymTab = SymTab $ M.insert n [info] table
    updSymTab = SymTab $ M.adjust (info:) n table
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Insert the declared variables into symbol table
insertDeclarations :: [(Id, Pos)] -> Type -> [(Instr,Pos)] -> MonadSymTab InstrSeq
insertDeclarations ids t asigsPos = do
  state@SymTabState{symTab = st, actS = activeS:_} <- get
  fileCode <- ask
  let
    asigs       = map fst asigsPos
    ids'        = map fst ids
    idsInfo     = lookupInSymTab' ids' st
    info (x:xs) = SymbolInfo x t activeS Variables [] : info xs
  
-- Add ids if none var declared
  if all (==Nothing) idsInfo then addToSymTab ids' (info ids')
  else
-- Get the first id from all declared vars in active scope
    let
      redefs          = concatMap fromJust $ filter isJust idsInfo
      vars            = [Variables, Parameters Value, Parameters Reference]
      isVar redef     = category redef `elem` vars
      redefs'         = filter isVar redefs
      redefsScopes    = map scope redefs'
      redefsIndexs    = findIndices isJust idsInfo
      isInActualScope = activeS `elem` redefsScopes
    in
      if isInActualScope then
        let p = snd $ ids !! head redefsIndexs
        in tell [errorMsg "Redefined variable" fileCode p]
      else
        let idsInScope = [i | i <- ids', index <- redefsIndexs, i == ids' !! index]
        in addToSymTab idsInScope (info ids')
--
  return $ reverse $ initialize (zip ids' asigs) t


initialize :: [(Id,Instr)] -> Type -> InstrSeq
initialize [] _               = []
initialize ((n, assig):ids) t =
  if emptyAssig assig then
    case t of
      TInt        -> assign (defaultBaseVal t) : initialize ids t
      TFloat      -> assign (defaultBaseVal t) : initialize ids t
      TBool       -> assign (defaultBaseVal t) : initialize ids t
      TChar       -> assign (defaultBaseVal t) : initialize ids t
      TArray e t' -> assign (ArrayList (lits e t') t) : initialize ids t
      tipo -> error $ "No se como inicializar esto todavia: " ++ show tipo
  else
    assig : initialize ids t

  where
    assign expr      = Assig (Var n t) expr TVoid
    lits eSize tArr = replicate (getSize eSize) $ defaultBaseVal tArr


defaultBaseVal :: Type -> Expr
defaultBaseVal TInt   = Literal (Integer 0)  TInt
defaultBaseVal TFloat = Literal (Floatt 0.0) TFloat
defaultBaseVal TBool  = Literal (Boolean True) TBool
defaultBaseVal TChar  = Literal (Character '\0') TChar
defaultBaseVal t      = error $ "Esto no es un tipo basico: " ++ show t

emptyAssig :: Instr -> Bool
emptyAssig (Assig _ (Literal EmptyVal _) _) = True
emptyAssig _                                = False

getSize :: Expr -> Int
getSize (Literal (Integer n) _) = n
getSize e = error $ "Expresion para tamaño de arreglo incorrecta: " ++ show e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts the subroutine's name into symbol table
defineSubroutine :: Id -> Category -> Pos -> MonadSymTab ()
defineSubroutine n category p = do
  SymTabState{symTab = st, proms = promises, actS = scopes, currS = current} <- get
  fileCode <- ask
  let infos = lookupInScopes scopes n st

  if isNothing infos then 
    let info = [SymbolInfo n TDummy 1 category []]
    in addToSymTab [n] info
  else
    let promise = getPromise n promises
    in
      if isJust promise then
        let prom = fromJust promise
        in
        if promiseType prom /= TVoid && category == Procedures then
          tell [errorMsg ("'"++n++"' is not a function") fileCode (promisePos prom)]
        else
          when (promiseType prom == TVoid && category == Functions) $
            tell [errorMsg ("'"++n++"' is not a procedure") fileCode (promisePos prom)]
      else
        tell [errorMsg ("Redefined subroutine"++show infos) fileCode p]

  return ()
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts a subroutine's parameter into symbol table
defineParameter :: Var -> Pos -> MonadSymTab (Type,Id)
defineParameter (Param name t ref) p = do
  state@SymTabState{symTab = st, actS = activeS:_}  <- get
  fileCode <- ask
  let
    infos = lookupInScopes [activeS] name st
    param = (t,name)

  if isJust infos then
    tell [errorMsg "Redefined parameter" fileCode p] >> return param
  else
    let info = [SymbolInfo name t activeS (Parameters ref) []]
    in addToSymTab [name] info >> return param
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Inserts a register / union into symbol table
defineRegUnion :: Id -> Type -> [ExtraInfo] -> Pos -> MonadSymTab (Id, Pos)
defineRegUnion reg regType extra p = do
  SymTabState{symTab = st, proms = promises, currS = current} <- get
  fileCode <- ask
  let regInfo = lookupInScopes [1] reg st

  if isJust regInfo then
    if regType == TRegister then
      tell [errorMsg "Redefined Inventory" fileCode p] >> return (reg, p)
    else
      tell [errorMsg "Redefined Items" fileCode p] >> return (reg, p)
  else
    let
      info      = [SymbolInfo reg regType 1 TypeConstructors extra]
      promise   =  getPromise reg promises
      npromises =
        if isJust promise then
          let promise' = fromJust promise in filter (/=promise') promises
        else promises
    in
      addToSymTab [reg] info >> return (reg,p)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
updatesDeclarationsCategory :: Id -> MonadSymTab ()
updatesDeclarationsCategory reg = do
  state@SymTabState{symTab = SymTab table, actS = activeScope:_} <- get
  let
    modifySym symInfo = symInfo{category = Fields, extraInfo = [FromReg reg]}
    updtSym = 
      map (\sym -> if scope sym == activeScope then modifySym sym else sym)

  put state{symTab = SymTab $ M.map updtSym table}
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
lookupInSymTab' [] _          = [Nothing]
lookupInSymTab' [x] symTab    = [lookupInSymTab x symTab]
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
    symInfo  = lookupInSymTab sym symTab
    symInfos = [si | si<-fromJust symInfo,s<-scopes,scope si `elem` [s,0]] 
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                               Updates
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Updates the symbol type
updateType :: Id -> Scope -> Type -> MonadSymTab ()
updateType symbol symScope t = do
  state@SymTabState{symTab = st@(SymTab table)} <- get
  fileCode <- ask
  let infos = lookupInSymTab symbol st
  when (isJust infos) $ do
    let
      isTarget s = scope s == symScope
      updateT'   = fmap (\sym -> if isTarget sym then modifyType sym t else sym)
      updatedST  = SymTab $ M.adjust updateT' symbol table

    put state{symTab = updatedST}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Updates the symbol category
updateCategory :: Id -> Scope -> Category -> MonadSymTab ()
updateCategory symbol symScope cat = do
  state@SymTabState{symTab = st@(SymTab table)} <- get
  let
    infos = lookupInSymTab symbol st

  when (isJust infos) $ do
    let
      target s  = scope s == symScope
      updateC   = fmap (\sym -> if target sym then modifyCategory sym cat else sym)
      updatedST = SymTab $ M.adjust updateC symbol table

    put state{symTab = updatedST}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- TODO: Merge this
-- | Updates the symbol extra info, depends of its category
updateExtraInfo :: Id -> Category -> [ExtraInfo] -> MonadSymTab ()
updateExtraInfo sym symCategory extra = do
  state@SymTabState{symTab = st@(SymTab table)} <- get
  let infos = lookupInSymTab sym st
  
  when (isJust infos) $ do
    let
      isTarget sym' = category sym' == symCategory
      updateExtraInfo' = 
        fmap (\s -> if isTarget s then modifyExtraInfo s extra else s)
            
    put state{symTab = SymTab $ M.adjust updateExtraInfo' sym table}

updateExtraInfoProm :: Id -> Category -> [ExtraInfo] -> MonadSymTab ()
updateExtraInfoProm sym symCategory extra = do
  state@SymTabState{symTab = st@(SymTab table)} <- get
  let infos = lookupInSymTab sym st
  
  when (isJust infos) $ do
    let
      isTarget sym' = category sym' == symCategory
      updateExtraInfo' = 
        fmap (\s -> if isTarget s then modifyExtraInfoProm s extra else s)
            
    -- liftIO $ print $ SymTab $ M.adjust updateExtraInfo' sym table
    put state{symTab = SymTab $ M.adjust updateExtraInfo' sym table}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                                Checks
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
checkPromises ::  MonadSymTab ()
checkPromises = do
  SymTabState{proms = promises} <- get
  fileCode <- ask

  forM_ promises $ \t ->
    case t of
      PromiseS {}         -> printErrorPromiseFunction t fileCode
      (PromiseT name pos) -> 
        tell [errorMsg ("Type '" ++ name ++ "' wasn't defined") fileCode pos]
-------------------------------------------------------------------------------


showParamsPF:: [(Type,Pos)] -> String
showParamsPF []        = ""
showParamsPF [(t,p)]   = show t
showParamsPF ((t,p):r) = show t ++ "," ++ showParamsPF r


printErrorPromiseFunction :: Promise -> FileCodeReader -> MonadSymTab ()
printErrorPromiseFunction (PromiseS name args t cat pc _ _ _)  fileCode = 
  if cat == Functions then
    tell [errorMsg ("Function '" ++ name ++ "(" ++ showParamsPF args ++ ") ->" ++ show t ++ "'  wasn't defined") fileCode pc]
  else 
    tell [errorMsg ("Procedure '" ++ name ++ "(" ++ showParamsPF args ++ ")' wasn't defined") fileCode pc]

