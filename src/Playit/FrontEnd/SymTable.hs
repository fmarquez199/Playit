{- |
 * Creates and handle the symbol table
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.FrontEnd.SymTable where

import qualified Control.Monad.Trans.RWS    as RWS
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map                   as Map

import qualified Playit.Utils               as U

import qualified Playit.FrontEnd.Lexer      as Lex
import qualified Playit.FrontEnd.Syntax     as S
import qualified Playit.FrontEnd.ParserM    as Pars


-- -----------------------------------------------------------------------------
-- | Initial state with prelude
-- stInitState :: Pars.ParserS
-- stInitState = createInitSymTab (Pars.SymTab Map.empty)

-- createInitSymTab :: Pars.SymTab -> Pars.ParserS
-- createInitSymTab st = Pars.ParserS
--   { Pars.psSymTab = st
--   , Pars.psStaticChain   = [1,0]
--   , Pars.psScope  = 1
--   , Pars.psPromises  = []
--   , Pars.psError = False
--   }
--   where
{-     win       = Pars.Symbol (S.Id (Lex.Token Lex.TkWIN (BLC.pack "Win") (-1,-1))) S.TBool 0 Pars.Constants []
    lose      = Pars.Symbol (S.Id (Lex.TkLOSE (BLC.pack "Lose") (-1,-1)))      S.TBool 0 Pars.Constants []
    power     = Pars.Symbol (S.Id (Lex.TkPOWER (BLC.pack "Power") (-1,-1)))     S.TInt 0 Pars.Types []
    skill     = Pars.Symbol (S.Id (Lex.TkSKILL (BLC.pack "Skill") (-1,-1)))     S.TFloat 0 Pars.Types []
    battle    = Pars.Symbol (S.Id (Lex.TkBATTLE (BLC.pack "Battle") (-1,-1)))    S.TBool 0 Pars.Types []
    rune      = Pars.Symbol (S.Id (Lex.TkRUNE (BLC.pack "Rune") (-1,-1)))      S.TChar 0 Pars.Types []
    runes     = Pars.Symbol (S.Id (Lex.TkRUNES (BLC.pack "Runes") (-1,-1)))     S.TStr 0 Pars.Types []
    listOf    = Pars.Symbol (S.Id (Lex.TkKitOf (BLC.pack "Kit of") (-1,-1)))    (S.TList S.TDummy) 0 Pars.TypeConstructors [] -- Tipo TEmptyList?
    inventory = Pars.Symbol (S.Id (Lex.TkINVENTORY (BLC.pack "Inventory") (-1,-1))) S.TRecord 0 Pars.TypeConstructors []
    items     = Pars.Symbol (S.Id (Lex.TkITEMS (BLC.pack "Items") (-1,-1)))     S.TUnion 0 Pars.TypeConstructors []
    apt       = Pars.Symbol (S.Id (Lex.TkDeathZone (BLC.pack "DeathZone") (-1,-1))) (S.TPointer S.TDummy) 0 Pars.Pointers [] -- Tipo TNull?
    portalCS  = Pars.Symbol (S.Id (Lex.TkID (BLC.pack "portalRuneToRunes") (-1,-1)))  S.TStr 0 Pars.Functions   [] -- [S.Params [(S.TChar,"rune")]  ]
    portalIS  = Pars.Symbol (S.Id (Lex.TkID (BLC.pack "portalPowerToRunes") (-1,-1))) S.TStr 0 Pars.Functions   [] -- [S.Params [(S.TInt,"power")]  ]
    portalFS  = Pars.Symbol (S.Id (Lex.TkID (BLC.pack "portalSkillToRunes") (-1,-1))) S.TStr 0 Pars.Functions   [] -- [S.Params [(S.TFloat,"skill")]]
    portalSI  = Pars.Symbol (S.Id (Lex.TkID (BLC.pack "portalRunesToPower") (-1,-1))) S.TInt 0 Pars.Functions   [] -- [S.Params [(S.TStr,"runes")]  ]
    portalSF  = Pars.Symbol (S.Id (Lex.TkID (BLC.pack "portalRunesToSkill") (-1,-1))) S.TFloat 0 Pars.Functions [] -- [S.Params [(S.TStr,"runes")]  ]
    portalSC  = Pars.Symbol (S.Id (Lex.TkID (BLC.pack "portalRunesToRune") (-1,-1)))  S.TChar 0 Pars.Functions  [] -- [S.Params [(S.TStr,"runes")]  ]
    askl      = Pars.Symbol (S.Id (Lex.TkID (BLC.pack "absSkill") (-1,-1))) S.TFloat 0 Pars.Functions [] -- [S.Params [(S.TFloat,"skill")]]
    abspow    = Pars.Symbol (S.Id (Lex.TkID (BLC.pack "absPower") (-1,-1))) S.TInt 0 Pars.Functions   [] -- [S.Params [(S.TInt,"power")]  ]
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
pushNewScope :: Pars.ParserM ()
pushNewScope = do
  state@Pars.ParserS{Pars.psStaticChain = oldS, Pars.psScope = s} <- RWS.get
  let newS = s + 1
  RWS.put state{Pars.psStaticChain = newS:oldS, Pars.psScope = newS}


-- -----------------------------------------------------------------------------
-- actualizar InLoop si es necesario (InLoop || not InLoop)
popScope :: Pars.ParserM ()
popScope = do
  state@Pars.ParserS{Pars.psStaticChain = _:prevScopes} <- RWS.get
  RWS.put state{Pars.psStaticChain = prevScopes}


-- -----------------------------------------------------------------------------
getCurrentScope :: Pars.ParserM U.Scope
getCurrentScope = RWS.get >>= \Pars.ParserS {Pars.psScope=_psScope} -> return _psScope


{-
 * -----------------------------------------------------------------------------
 *                                   Inserts
 * -----------------------------------------------------------------------------
-}

insert :: S.Id -> U.Scope -> Pars.ParserM S.Id
insert id scope = do
  state@Pars.ParserS {Pars.psSymTab = Pars.SymTab oldSt} <- RWS.get
  let
    symbol = Pars.Symbol id S.TDummy scope Pars.Variables []
    newSt  = Pars.SymTab $ Map.insert id symbol oldSt
  RWS.put state {Pars.psSymTab = newSt}
  return id


{-
 * -----------------------------------------------------------------------------
 *                                   Lookups
 * -----------------------------------------------------------------------------
-}




{-
 * -----------------------------------------------------------------------------
 *                                  Updates
 * -----------------------------------------------------------------------------
-}


