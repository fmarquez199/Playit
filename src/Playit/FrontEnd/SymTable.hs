{- |
 * Creates and handle the symbol table
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.FrontEnd.SymTable where

import Control.Monad (when, unless)

import qualified Control.Monad.Trans.RWS    as RWS
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map                   as Map

import qualified Playit.Utils               as U
import qualified Playit.Errors              as E

import qualified Playit.FrontEnd.Lexer      as Lex
import qualified Playit.FrontEnd.Syntax     as S
import qualified Playit.FrontEnd.ParserM    as PM


-- -----------------------------------------------------------------------------
-- | Initial state with prelude
-- stInitState :: PM.ParserS
-- stInitState = createInitSymTab (PM.SymTab Map.empty)

-- createInitSymTab :: PM.SymTab -> PM.ParserS
-- createInitSymTab st = PM.ParserS
--   { PM.psSymTab = st
--   , PM.psStaticChain   = [1,0]
--   , PM.psScope  = 1
--   , PM.psPromises  = []
--   , PM.psError = False
--   }
--   where
{-     win       = PM.Symbol (S.Id (Lex.Token Lex.TkWIN (BLC.pack "Win") (-1,-1))) S.TBool 0 PM.Constants []
    lose      = PM.Symbol (S.Id (Lex.TkLOSE (BLC.pack "Lose") (-1,-1)))      S.TBool 0 PM.Constants []
    power     = PM.Symbol (S.Id (Lex.TkPOWER (BLC.pack "Power") (-1,-1)))     S.TInt 0 PM.Types []
    skill     = PM.Symbol (S.Id (Lex.TkSKILL (BLC.pack "Skill") (-1,-1)))     S.TFloat 0 PM.Types []
    battle    = PM.Symbol (S.Id (Lex.TkBATTLE (BLC.pack "Battle") (-1,-1)))    S.TBool 0 PM.Types []
    rune      = PM.Symbol (S.Id (Lex.TkRUNE (BLC.pack "Rune") (-1,-1)))      S.TChar 0 PM.Types []
    runes     = PM.Symbol (S.Id (Lex.TkRUNES (BLC.pack "Runes") (-1,-1)))     S.TStr 0 PM.Types []
    listOf    = PM.Symbol (S.Id (Lex.TkKitOf (BLC.pack "Kit of") (-1,-1)))    (S.TList S.TDummy) 0 PM.TypeConstructors [] -- Tipo TEmptyList?
    inventory = PM.Symbol (S.Id (Lex.TkINVENTORY (BLC.pack "Inventory") (-1,-1))) S.TRecord 0 PM.TypeConstructors []
    items     = PM.Symbol (S.Id (Lex.TkITEMS (BLC.pack "Items") (-1,-1)))     S.TUnion 0 PM.TypeConstructors []
    apt       = PM.Symbol (S.Id (Lex.TkDeathZone (BLC.pack "DeathZone") (-1,-1))) (S.TPointer S.TDummy) 0 PM.Pointers [] -- Tipo TNull?
    portalCS  = PM.Symbol (S.Id (Lex.TkID (BLC.pack "portalRuneToRunes") (-1,-1)))  S.TStr 0 PM.Functions   [] -- [S.Params [(S.TChar,"rune")]  ]
    portalIS  = PM.Symbol (S.Id (Lex.TkID (BLC.pack "portalPowerToRunes") (-1,-1))) S.TStr 0 PM.Functions   [] -- [S.Params [(S.TInt,"power")]  ]
    portalFS  = PM.Symbol (S.Id (Lex.TkID (BLC.pack "portalSkillToRunes") (-1,-1))) S.TStr 0 PM.Functions   [] -- [S.Params [(S.TFloat,"skill")]]
    portalSI  = PM.Symbol (S.Id (Lex.TkID (BLC.pack "portalRunesToPower") (-1,-1))) S.TInt 0 PM.Functions   [] -- [S.Params [(S.TStr,"runes")]  ]
    portalSF  = PM.Symbol (S.Id (Lex.TkID (BLC.pack "portalRunesToSkill") (-1,-1))) S.TFloat 0 PM.Functions [] -- [S.Params [(S.TStr,"runes")]  ]
    portalSC  = PM.Symbol (S.Id (Lex.TkID (BLC.pack "portalRunesToRune") (-1,-1)))  S.TChar 0 PM.Functions  [] -- [S.Params [(S.TStr,"runes")]  ]
    askl      = PM.Symbol (S.Id (Lex.TkID (BLC.pack "absSkill") (-1,-1))) S.TFloat 0 PM.Functions [] -- [S.Params [(S.TFloat,"skill")]]
    abspow    = PM.Symbol (S.Id (Lex.TkID (BLC.pack "absPower") (-1,-1))) S.TInt 0 PM.Functions   [] -- [S.Params [(S.TInt,"power")]  ]
    symbols   = ["Power", "Skill", "Rune", "Runes", "Battle", "Inventory", "Items",
                "Kit of", "Win", "Lose", "DeathZone", "absSkill", "absPower",
                "portalRunesToRune", "portalRuneToRunes", "portalPowerToRunes",
                "portalSkillToRunes","portalRunesToPower", "portalRunesToSkill"]
    info      = [power, skill, rune, runes, battle, inventory, items, listOf, win, lose, apt,
                portalSC, portalCS, portalIS, portalFS, askl, abspow, portalSI, portalSF]
    st'       = insertSymbols symbols info st
 -}

-- -----------------------------------------------------------------------------
-- actualizar InLoop si es necesario (entre en uno?)
pushNewScope :: PM.ParserM ()
pushNewScope = do
  state@PM.ParserS{PM.psStaticChain = oldS, PM.psScope = s} <- RWS.get
  let newS = s + 1
  RWS.put state{PM.psStaticChain = newS:oldS, PM.psScope = newS}


-- -----------------------------------------------------------------------------
-- actualizar InLoop si es necesario (InLoop || not InLoop)
popScope :: PM.ParserM ()
popScope = do
  state@PM.ParserS{PM.psStaticChain = _:prevScopes} <- RWS.get
  RWS.put state{PM.psStaticChain = prevScopes}


-- -----------------------------------------------------------------------------
getCurrentScope :: PM.ParserM U.Scope
getCurrentScope = RWS.get >>= \PM.ParserS {PM.psScope=_psScope} -> return _psScope


{-
 * -----------------------------------------------------------------------------
 *                                   Inserts
 * -----------------------------------------------------------------------------
-}

insert :: Lex.Token -> S.Type -> PM.Category -> U.Scope -> PM.ParserM S.Id
insert tk t category scope = do
  state@PM.ParserS {PM.psSymTab = PM.SymTab oldSt, PM.psScope = _scope} <- RWS.get
  let
    id'    = S.Id (Lex.tkInput tk) (Lex.tkPosn tk)
    symbol = PM.Symbol id' t scope category PM.NoDef
    newSt  = PM.SymTab $ Map.insert id' symbol oldSt
    isDef  = scope == _scope && Map.member id' oldSt
    errMsg = "Already defined " ++ show category

  when isDef $ do
    PM.ParserR {PM.prFilename = file, PM.prCode = context} <- RWS.ask
    RWS.tell [E.Error errMsg context file (Lex.tkPosn tk)]
    RWS.put state {PM.psError = True}
  
  unless isDef $ 
    RWS.put state {PM.psSymTab = newSt}

  return id'


{-
 * -----------------------------------------------------------------------------
 *                                   Lookups
 * -----------------------------------------------------------------------------
-}




{-
 * -----------------------------------------------------------------------------
 *                                  Adjusts
 * -----------------------------------------------------------------------------
-}

-- NOTA: si no se usa en otra funcion ademas de AST.nodeDeclaration, poner S.Initialization el lugar de S.Id, se quita un 'map'
adjustType :: S.Type -> S.Id -> PM.ParserM ()
adjustType t id' = do
  state@PM.ParserS {PM.psSymTab = PM.SymTab oldSt, PM.psScope = scope} <- RWS.get
  let
    updt sym = if PM.symScope sym == scope then sym {PM.symType = t} else sym
    newSt    = PM.SymTab $ Map.adjust updt id' oldSt
  
  RWS.put state {PM.psSymTab = newSt}


adjustDefInstr :: S.Id -> S.InstrSeq -> PM.ParserM ()
adjustDefInstr id' is = do
  state@PM.ParserS {PM.psSymTab = PM.SymTab oldSt, PM.psScope = scope} <- RWS.get
  let
    updt sym@PM.Symbol {PM.symDefInfo = di} = {- if PM.symScope sym == scope then -} sym {PM.symDefInfo = di {PM.instr = is}} {- else sym -}
    newSt    = PM.SymTab $ Map.adjust updt id' oldSt
  
  RWS.put state {PM.psSymTab = newSt}


adjustDefVars :: S.Id -> [S.Id] -> PM.ParserM ()
adjustDefVars id' vars = do
  state@PM.ParserS {PM.psSymTab = PM.SymTab oldSt, PM.psScope = scope} <- RWS.get
  let
    updt sym = {- if PM.symScope sym == scope then -} sym {PM.symDefInfo = PM.DefInfo [] vars} {- else sym -}
    newSt    = PM.SymTab $ Map.adjust updt id' oldSt
  
  RWS.put state {PM.psSymTab = newSt}
