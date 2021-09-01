module Playit.FrontEnd.ParserM
  ( ParserM(..)
  , ParserR(..)
  , ParserS(..)
  , SymTab(..)
  , Symbol(..)
  , Category(..)
  , DefInfo(..)
  , pInitState
  , parseError
  )where

import Control.Monad.Trans.RWS (RWST(..), tell, ask)

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Map                   as Map

import qualified Playit.Errors              as E
import qualified Playit.Utils               as U

import qualified Playit.FrontEnd.Lexer      as Lex
import qualified Playit.FrontEnd.Syntax     as S


-- Parser State
data ParserS = ParserS 
  { psSymTab      :: SymTab
  , psStaticChain :: [U.Scope] -- ^ Active scopes
  , psScope       :: U.Scope   -- ^ Current scope
  , psPromises    :: [Promise] -- ^ Subroutines promises
  , psInLoop      :: Bool      -- ^ To know if break/continue its used inside a loop
  , psError       :: Bool      -- ^ Program has errors
  }

-- Parser Reader
-- (filename, Source code in lines/rows)
data ParserR = ParserR
  { prFilename :: String
  , prCode     :: ![BL.ByteString]
  }

type ParserM a = RWST ParserR [E.Error] ParserS IO a


-- -----------------------------------------------------------------------------
{- | Symbol table
 * Hash table:
 *   Key: Id
 *   Value: Symbol
-}
newtype SymTab  = SymTab { unSymTab :: Map.Map S.Id Symbol } deriving (Eq, Ord)

instance Show SymTab where
  show (SymTab st) = header ++ info ++ symbols
    where
      header       = "\n------------\n Symbol table \n------------\n"
      info         = "- Symbol | Related information \n------------\n"
      table        = Map.toList st
      showSt (k,v) = if symScope v > 0 then show k ++ " -> " ++ show v ++ "\n" else ""
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

-- Information related to subroutines and structs definitions
data DefInfo = DefInfo
  { instr :: S.InstrSeq       -- ^ Subroutines instruction sequence
  , vars  :: [S.Id] -- S.Vars? -- ^ Subroutines parameters and? Structs fields?
  -- , struct  :: S.Id {- Bool que indica si esta activo(uniones) -} -- Struct  al que pertenece el campo/variable
  -- , fields  :: [S.Declaration]
  -- , ActiveField
  }
  | NoDef
  deriving (Eq, Ord)

instance Show DefInfo where
  show NoDef         = ""
  show (DefInfo i v) =
    "\n    DefInfo:" ++
    "\n    Vars: " ++ show v ++
    "\n    Instr:\n      " ++ U.joinWith "\t  " i

-- Representation of the symbol for symbol table
data Symbol = Symbol
  { symId       :: S.Id -- es realmente necesario???
  , symType     :: S.Type
  , symScope    :: U.Scope
  , symCategory :: Category
  , symDefInfo  :: DefInfo
  }
  deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol _ t s c ei) = 
    "\n  Type: " ++ show t ++ " | Scope: " ++ show s ++ " | Category: "++ show c ++ show ei


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


-- -----------------------------------------------------------------------------
-- | Initial state with prelude
pInitState :: ParserS
pInitState = createInitState (SymTab Map.empty)

createInitState :: SymTab -> ParserS
createInitState st = ParserS
  { psSymTab      = st
  , psStaticChain = [1,0]
  , psScope       = 1
  , psPromises    = []
  , psInLoop      = False
  , psError       = False
  }
  

-------------------------------------------------------------------------------
-- | Show the first parser error
parseError :: [Lex.Token] -> ParserM a
parseError []       =  fail "\n\n\x1b[1;91mInvalid Program\n\n"
parseError (tk:tks) =  do
  fileCode <- ask
  -- error . show $ E.errorMsg' (BLC.pack "Parse error") fileCode (Lex.tkPosn tk)
  -- error $ "Parse error: " ++ str ++ ". Pos: " ++ show (Lex.tkPosn tk)
  let err = E.Error "Parse error" [Lex.tkInput tk] (prFilename fileCode) (Lex.tkPosn tk)
      -- tkE = filter (\x -> not $ elem x (prCode fileCode)) (map Lex.tkInput tks)
  tell [err]
  -- parseError tks
  fail (show err)
  -- fail (show tks ++ "\n" ++ show tkE) -- tiene los tokens sin err
