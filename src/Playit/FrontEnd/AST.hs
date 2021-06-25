{- |
 * Creates de abstract syntax tree with type checks
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.FrontEnd.AST
  ( -- * Program
    nodeProgram
    -- * Variables
    , nodeDeclaration , nodeIdInit , nodeVar , nodeField , nodeIndex , nodeDeref
    -- * Instructions
    -- ** Assignments
    , nodeAssign , nodeIncrement , nodeDecrement
    -- ** If
    , nodeSelection , nodeGuard
    -- ** Loops
    , nodeWhile , nodeFor , nodeForWhile , nodeForEach, nodeControlLoop
    -- ** Pointers
    , nodeFree
    -- * Subroutines
    , nodeParameter , nodeProcCall , nodeFuncCall , nodeCall, nodeReturn
    -- * I/O
    , nodeRead , nodePrint
    -- * Expressions
    -- ** Operations
    , nodeBinary , nodeUnary , nodeAnexo , nodeConcatLists , nodeIfSimple
    -- ** Sequence of items
    , nodeArray , nodeList , nodeStruct
    -- ** Variables types
    , nodeTArray , nodeTStruct
    -- * 
    , defineTStruct
  ) where

import Control.Monad.Trans.RWS (get, put, tell)

import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Playit.Utils               as U

import qualified Playit.FrontEnd.Errors     as E
import qualified Playit.FrontEnd.Lexer      as Lex
import qualified Playit.FrontEnd.ParserM    as Pars
-- import qualified Playit.FrontEnd.Promises  as Prom
import qualified Playit.FrontEnd.SymTable   as ST
import qualified Playit.FrontEnd.Syntax     as S
import qualified Playit.FrontEnd.TypeCheck  as TC


-------------------------------------------------------------------------------
-- | Creates whole statements block
nodeProgram :: S.InstrSeq -> Pars.ParserM S.Instruction
nodeProgram is = {- Prom.checkPromises >> -} return $ S.Program (reverse is)

{-
 * -----------------------------------------------------------------------------
 *                                Variables
 * -----------------------------------------------------------------------------
-}

-- -----------------------------------------------------------------------------
-- Check initializations types and insert the ids in the symbol table
-- * si una expr en inits es empty value puedo ponerle su valor por defecto de una, 
-- * no tendria que luego inicializar variables
-- * en las funcs veo su tipo
nodeDeclaration :: S.Type -> [S.Initialization] -> Pars.ParserM S.Declaration
nodeDeclaration t inits = return $ S.Declaration t inits

nodeIdInit :: S.Id -> S.Expression -> S.Initialization
nodeIdInit id' e = S.Initialization id' e

{- {- #LANGUAGE -XLambdaCase -}
typeDefaultValue :: S.Type -> S.ExprKind
typeDefaultValue = \case
  S.TInt           -> Integer 0
  S.TFloat         -> Floatt 0.0
  S.TBool          -> Boolean False
  S.TChar          -> Chr ''
  S.TStr           -> Str ""
  S.TRecord        -> -- se inicializan son los campos
  S.TUnion         -> -- se inicializan son los campos
  S.TStruct struct -> -- la idea: map nodeIdInit $ S.sFields struct
  S.TArray size t  -> S.ArrayList (replicate size $   | t)
  S.TList t        -> S.ArrayList []
  S.TPointer t     -> S.Null
  S.TEmptyList     -> S.EmptyVal
  S.TNull          -> S.Null
  S.TVoid          -> -- 
  S.TRead          -> -- 
  S.TDummy         -> -- 
  S.TPDummy        -> -- 
  S.TError         -> -- 
-}


-- -----------------------------------------------------------------------------
nodeVar :: Lex.Token -> Pars.ParserM S.Var
nodeVar name = return $ S.Var (S.Id name) S.Variable S.TDummy


-- -----------------------------------------------------------------------------
nodeField :: S.Var -> Lex.Token -> Pars.ParserM S.Var
nodeField var field = return $ S.Var (S.Id field) (S.Field (S.var var)) S.TDummy


-- -----------------------------------------------------------------------------
nodeIndex :: S.Var -> S.Expression -> Pars.ParserM S.Var
nodeIndex var index = return $ S.Var (S.varId var) (S.Index (S.var var) index) S.TDummy


-- -----------------------------------------------------------------------------
nodeDeref :: S.Var -> Pars.ParserM S.Var
nodeDeref var = return $ S.Var (S.varId var) (S.Deref (S.var var)) S.TDummy


{-
 * -----------------------------------------------------------------------------
 *                                Instructions
 * -----------------------------------------------------------------------------
-}

nodeAssign :: S.Var -> S.Expression -> Pars.ParserM S.Instruction
nodeAssign lval e = return $ S.Assig lval e

nodeIncrement :: S.Var -> Pars.ParserM S.Instruction
nodeIncrement lval = return $
  S.Assig lval (S.Expression (S.Binary S.Add (S.Rval lval) (S.Integer $ BLC.pack "1")) S.TInt (Lex.tkPosn . S.idTk $ S.varId lval) )

nodeDecrement :: S.Var -> Pars.ParserM S.Instruction
nodeDecrement lval = return $
  S.Assig lval (S.Expression (S.Binary S.Minus (S.Rval lval) (S.Integer $ BLC.pack "1")) S.TInt (Lex.tkPosn . S.idTk $ S.varId lval) )


-- -----------------------------------------------------------------------------
nodeSelection :: [S.Guard] -> U.Position -> S.Instruction
nodeSelection guards p = S.Selection guards

nodeGuard :: S.Expression -> S.InstrSeq -> Pars.ParserM S.Guard
nodeGuard cond instrs = return $ S.Guard cond instrs


-- -----------------------------------------------------------------------------
nodeWhile :: S.Expression -> S.InstrSeq -> Pars.ParserM S.Instruction
nodeWhile cond instrs = return $ S.While cond instrs


-- -----------------------------------------------------------------------------
nodeFor :: Pars.ParserM S.Instruction
nodeFor = undefined


nodeForWhile :: Pars.ParserM S.Instruction
nodeForWhile = undefined


nodeForEach :: Pars.ParserM S.Instruction
nodeForEach = undefined


-- -----------------------------------------------------------------------------
nodeControlLoop :: S.Instruction -> U.Position -> Pars.ParserM S.Instruction
nodeControlLoop i posn =
  get >>= (\s@Pars.ParserS{Pars.psInLoop = inLoop, Pars.psError = err} ->
    if inLoop && not err
      then put s{ Pars.psError = err && inLoop } >> return i
      else put s{ Pars.psError = err || not inLoop } >> E.notInLoop posn >> return i
  )


-- -----------------------------------------------------------------------------
nodeFree :: Lex.Token -> Pars.ParserM S.Instruction
nodeFree id' = return $ S.Free $ S.Id id'


{-
 * -----------------------------------------------------------------------------
 *                                Subroutines
 * -----------------------------------------------------------------------------
-}

nodeParameter :: Lex.Token -> S.Type -> S.Ref -> Pars.ParserM S.Var
nodeParameter name t ref = return $ S.Var (S.Id name) (S.Param ref) t


-- -----------------------------------------------------------------------------
nodeProcCall :: S.Subroutine -> Pars.ParserM S.Instruction
nodeProcCall s = return $ S.ProcCall s


-- -----------------------------------------------------------------------------
nodeFuncCall :: S.Subroutine -> Pars.ParserM S.Expression
nodeFuncCall s@(S.Call name _) = return $ S.Expression (S.FuncCall s) S.TVoid (Lex.tkPosn $ S.idTk name)


-- -----------------------------------------------------------------------------
nodeCall :: Lex.Token -> S.Params -> Pars.ParserM S.Subroutine
nodeCall name args = return $ S.Call (S.Id name) args


-- -----------------------------------------------------------------------------
-- TODO: check tipo para regreso de funcion
nodeReturn :: S.Expression -> S.Instruction
nodeReturn val = S.Return val


{-
 * -----------------------------------------------------------------------------
 *                                  I/O
 * -----------------------------------------------------------------------------
-}

nodePrint :: [S.Expression] -> Lex.Token -> Pars.ParserM S.Instruction
nodePrint es tk = return $ S.Print es


-- -----------------------------------------------------------------------------
nodeRead :: S.Expression -> Lex.Token -> Pars.ParserM S.Expression
nodeRead e tk = return $ S.Expression (S.Read $ S.expr e) S.TRead (Lex.tkPosn tk)


{-
 * -----------------------------------------------------------------------------
 *                                Expressions
 * -----------------------------------------------------------------------------
-}

nodeTArray :: S.Expression -> S.Type -> Pars.ParserM S.Type
nodeTArray size t = return $ S.TArray size t


-- -----------------------------------------------------------------------------
nodeTStruct :: Lex.Token -> Pars.ParserM S.Type
nodeTStruct tk = return $ S.TStruct (S.Struct (S.Id tk) S.Record [] 0)


-- -----------------------------------------------------------------------------
nodeBinary :: S.BinOp -> S.Expression -> S.Expression -> Lex.Token -> Pars.ParserM S.Expression
nodeBinary op le re tk = return $ S.Expression (S.Binary op (S.expr le) (S.expr re)) S.TVoid (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeAnexo :: S.Expression -> S.Expression -> Lex.Token -> Pars.ParserM S.Expression
nodeAnexo h list tk = return $ S.Expression (S.ArrayList $ h : [list]) (S.TList S.TVoid) (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeConcatLists :: S.Expression -> S.Expression -> Lex.Token -> Pars.ParserM S.Expression
nodeConcatLists l1 l2 tk = return $ S.Expression (S.ArrayList $ l1 : [l2]) (S.TList S.TVoid) (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeIfSimple :: S.Expression -> S.Expression -> S.Expression -> Lex.Token -> Pars.ParserM S.Expression
nodeIfSimple cond true false tk = return $ S.Expression (S.IfSimple (S.expr cond) (S.expr true) (S.expr false)) S.TVoid (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeStruct :: Lex.Token -> [S.Expression] -> Pars.ParserM S.Expression
nodeStruct tk es = return $ S.Expression (S.StructE es) S.TVoid (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeArray :: [S.Expression] -> Lex.Token -> Pars.ParserM S.Expression
nodeArray es tk = return $ S.Expression (S.ArrayList es) (S.TArray (S.Expression S.EmptyVal S.TInt (0,0)) S.TVoid) (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeList :: [S.Expression] -> Lex.Token -> Pars.ParserM S.Expression
nodeList es tk = return $ S.Expression (S.ArrayList es) (S.TList S.TVoid) (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeUnary :: S.UnOp -> S.Expression -> S.Type -> Lex.Token -> Pars.ParserM S.Expression
nodeUnary op e t tk = return $ S.Expression (S.Unary op $ S.expr e) t (Lex.tkPosn tk)


{-
 * -----------------------------------------------------------------------------
 *                                -
 * -----------------------------------------------------------------------------
-}

-- -----------------------------------------------------------------------------
defineTStruct :: Lex.Token -> S.Type -> Pars.ParserM S.Id
defineTStruct tk t = return $ S.Id tk
