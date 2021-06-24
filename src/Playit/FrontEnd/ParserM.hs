module Playit.FrontEnd.ParserM
  ( ParserM(..)
  , ParserS(..)
  , SymTab(..)
  , Symbol(..)
  , Category(..)
  , parseError
  )where

import Control.Monad.Trans.RWS (RWST(..), ask)

-- import qualified Control.Monad.Trans.RWS    as RWS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map               as Map

import qualified Playit.Errors          as E
import qualified Playit.Utils           as U

import qualified Playit.FrontEnd.Lexer     as Lex
import qualified Playit.FrontEnd.Syntax as S


-- Parser State
data ParserS = ParserS 
  { psSymTab      :: SymTab
  , psStaticChain :: [U.Scope] -- ^ Active scopes
  , psScope       :: U.Scope   -- ^ Current scope
  , psPromises    :: [Promise] -- ^ Subroutines promises
  , psError       :: Bool      -- ^ Program has errors
  }

-- Parser Reader
-- (filename, Source code in lines/rows)
type ParserR = (BL.ByteString, [BL.ByteString])

type ParserM a = RWST ParserR [E.Error] ParserS IO a


-- -----------------------------------------------------------------------------
{- | Symbol table
 * Hash table:
 *   Key: Id
 *   Value: Symbol
-}
newtype SymTab  = SymTab { unSymTab :: Map.Map S.Id [Symbol] } deriving (Eq, Ord)

instance Show SymTab where
  show (SymTab st) = header ++ info ++ symbols
    where
      header       = "\n------------\n Symbol table \n------------\n"
      info         = "- Symbol | Related information \n------------\n"
      table        = Map.toList st
      stWithScopes = Map.map (map symScope) st
      symbols'     = map fst $ Map.toList $ Map.filter (any (>0)) stWithScopes
      showInfo i   = if symScope i > 0 then show i else ""
      showSt (k,v) =
        if k `elem` symbols' then show k ++ " -> " ++ concatMap showInfo (reverse v) ++ "\n"
        else ""
      symbols      = concatMap showSt table


-- -----------------------------------------------------------------------------
-- Symbol category
data Category
  = Constants
  | Fields
  | Functions
  | IterationVariable
  | Parameters S.Ref
  | Pointers
  | Procedures
  | TypeConstructors
  | Types
  | Variables
  deriving (Eq, Ord, Show)

-- Symbol extra information
data ExtraInfo = ExtraInfo
  { ast     :: S.InstrSeq
  , params  :: [(S.Type, S.Id)]
  , fromReg :: S.Id {- Bool que indica si esta activo(uniones) -} -- Reg / union al que pertenece el campo/variable
  -- , ActiveField
  } deriving (Eq, Ord)

instance Show ExtraInfo where
  show (ExtraInfo ast p fr) =
    "    AST:\n      " ++ U.joinWith "\t  " ast ++
    "    Parameters: " ++ show p ++ "\n" ++
    "    From struct: " ++ show fr

-- Representation of the symbol for symbol table
data Symbol = Symbol
  { symId        :: S.Id
  , symType      :: S.Type
  , symScope     :: U.Scope
  , symCategory  :: Category
  , symExtraInfo :: [ExtraInfo]
  }
  deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol _ t s c ei) = 
    "\n  Type: " ++ show t ++ " | Scope: " ++ show s ++ " | Category: "++ show c ++
    if null ei then ""
    else "\n    Extra:\n  " ++ U.joinWith "  " ei ++ "\n"


-- -----------------------------------------------------------------------------
-- Promise for co-recursive subroutines and structs
data Promise
  = PromiseS
    { promiseId           :: S.Id
    , promiseParams       :: [(S.Type, U.Position)]
    , promiseType         :: S.Type
    , promiseCat          :: Category
    , promisePos          :: U.Position
    , promiseLateCheck    :: [LateCheckPromise]
     -- Llamadas a funciones que se deben chequear cuando se actualiza el tipo de retorno de
     -- esta promesa, esta promesa aparece en las expresiones de llamadas a funciones
    , otherCallsLateCheck :: [LateCheckPromise]
    , forEachLateCheck    :: [LateCheckPromise]
    }
  | PromiseT
    { promiseId  :: S.Id
    , promisePos :: U.Position
    }
  deriving (Eq, Ord,Show)

-- 
-- Power a = a() > b()? 1:2
-- Power a = #(a() :: b())==10 ? 1:2
-- 
data LateCheckPromise
  = LateCheckPromS
    { expr        :: S.Expression -- Expresion que debe ser evaluada cuando se actualiza el tipo de la promesa
    , argsPos     :: [U.Position] -- Posiciones (linea,columna) de los argumentos necesarios para el check
    , linkedProms :: [S.Id]       -- Otras promesas enlazadas a este check (su relacionado)
    }
  | LateCheckPromCall
    { promCall    :: S.Subroutine -- Llamada que se debe evaluar
    , linkedProms :: [S.Id]       -- Promesas enlazadas
    }
  | LateCheckPromForE
    { expr        :: S.Expression -- Llamada que se debe evaluar
    , varId       :: S.Id         -- Promesas enlazadas
    , varType     :: S.Type       -- Promesas enlazadas
    , exprPos     :: U.Position
    , linkedProms :: [S.Id]       -- Otras promesas enlazadas a este check (su relacionado)
    }
  deriving (Eq, Ord,Show)

-- data PromiseExtraI = PromiseExtraI{}

-------------------------------------------------------------------------------
-- | Show the first parser error
parseError :: [Lex.Token] -> ParserM a
parseError [] =  error "\n\n\x1b[1;91mInvalid Program\n\n"
parseError (tk:tks) =  do
  fileCode <- ask
  -- error . show $ E.errorMsg' (BLC.pack "Parse error") fileCode (Lex.tkPosn tk)
  error $ "Parse error: " ++ show tk ++ ". Pos: " ++ show (Lex.tkPosn tk)