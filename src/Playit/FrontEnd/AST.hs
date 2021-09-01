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
    , nodeDeclaration , nodeIdInit , nodeLVal , nodeField , nodeIndex , nodeDeref
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
    , nodeProcCall , nodeFuncCall , nodeCall, nodeReturn
    -- * I/O
    , nodeRead , nodePrint
    -- * Expressions
    -- ** Operations
    , nodeBinary , nodeUnary , nodeAnexo , nodeConcatLists , nodeIfSimple, nodeNew
    -- ** Sequence of items
    , nodeArray , nodeList , nodeStruct
    -- ** Variables types
    , nodeTArray , nodeTStruct
    -- * 
    , defineTStruct
  ) where

import Control.Monad.Trans.RWS (get, put, tell)
import Control.Monad           (mapM_)

import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Playit.Utils               as U

import qualified Playit.FrontEnd.Errors     as E
import qualified Playit.FrontEnd.Lexer      as Lex
import qualified Playit.FrontEnd.ParserM    as PM
-- import qualified Playit.FrontEnd.Promises  as Prom
import qualified Playit.FrontEnd.SymTable   as ST
import qualified Playit.FrontEnd.Syntax     as S
import qualified Playit.FrontEnd.TypeCheck  as TC


-------------------------------------------------------------------------------
-- | Creates whole statements block
nodeProgram :: S.InstrSeq -> PM.ParserM S.Instruction
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
nodeDeclaration :: S.Type -> [S.Initialization] -> PM.ParserM S.Declaration
nodeDeclaration t inits = mapM_ (ST.adjustType t . S.initId) inits >> return (S.Declaration t inits)


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
nodeLVal :: Lex.Token -> PM.ParserM S.Var
nodeLVal name = return $ S.Var (S.Id (Lex.tkInput name) (Lex.tkPosn name)) S.Variable S.TDummy (Lex.tkPosn name)


-- -----------------------------------------------------------------------------
nodeField :: S.Var -> Lex.Token -> PM.ParserM S.Var
nodeField var field = return $ S.Var (S.Id (Lex.tkInput field) (Lex.tkPosn field)) (S.Field (S.var var)) S.TDummy (Lex.tkPosn field)


-- -----------------------------------------------------------------------------
nodeIndex :: S.Var -> S.Expression -> PM.ParserM S.Var
nodeIndex var index = return $ S.Var (S.varId var) (S.Index (S.var var) index) S.TDummy (S.varPosn var)


-- -----------------------------------------------------------------------------
nodeDeref :: S.Var -> PM.ParserM S.Var
nodeDeref var = return $ S.Var (S.varId var) (S.Deref (S.var var)) S.TDummy (S.varPosn var)


{-
 * -----------------------------------------------------------------------------
 *                                Instructions
 * -----------------------------------------------------------------------------
-}

nodeAssign :: S.Var -> S.Expression -> PM.ParserM S.Instruction
nodeAssign lval e = return $ S.Assig lval e

nodeIncrement :: S.Var -> PM.ParserM S.Instruction
nodeIncrement lval = return $
  S.Assig lval (S.Expression (S.Binary S.Add (S.Rval lval) (S.Integer $ BLC.pack "1")) S.TInt (S.varPosn lval) )

nodeDecrement :: S.Var -> PM.ParserM S.Instruction
nodeDecrement lval = return $
  S.Assig lval (S.Expression (S.Binary S.Minus (S.Rval lval) (S.Integer $ BLC.pack "1")) S.TInt (S.varPosn lval) )


-- -----------------------------------------------------------------------------
nodeSelection :: [S.Guard] -> U.Position -> S.Instruction
nodeSelection guards p = S.Selection guards

nodeGuard :: S.Expression -> S.InstrSeq -> PM.ParserM S.Guard
nodeGuard cond instrs = return $ S.Guard cond instrs


-- -----------------------------------------------------------------------------
nodeWhile :: S.Expression -> S.InstrSeq -> PM.ParserM S.Instruction
nodeWhile cond instrs = return $ S.While cond instrs


-- -----------------------------------------------------------------------------
nodeFor :: PM.ParserM S.Instruction
nodeFor = undefined


nodeForWhile :: PM.ParserM S.Instruction
nodeForWhile = undefined


nodeForEach :: PM.ParserM S.Instruction
nodeForEach = undefined


-- -----------------------------------------------------------------------------
nodeControlLoop :: S.Instruction -> U.Position -> PM.ParserM S.Instruction
nodeControlLoop i posn =
  get >>= (\s@PM.ParserS{PM.psInLoop = inLoop, PM.psError = err} ->
    if inLoop && not err
      then put s{ PM.psError = err && inLoop } >> return i
      else put s{ PM.psError = err || not inLoop } >> E.notInLoop posn >> return i
  )


-- -----------------------------------------------------------------------------
nodeFree :: Lex.Token -> PM.ParserM S.Instruction
nodeFree id' = return $ S.Free $ S.Id (Lex.tkInput id') (Lex.tkPosn id')


{-
 * -----------------------------------------------------------------------------
 *                                Subroutines
 * -----------------------------------------------------------------------------
-}

nodeProcCall :: S.Subroutine -> PM.ParserM S.Instruction
nodeProcCall s = return $ S.ProcCall s


-- -----------------------------------------------------------------------------
nodeFuncCall :: S.Subroutine -> PM.ParserM S.Expression
nodeFuncCall s@(S.Call name _) = return $ S.Expression (S.FuncCall s) S.TVoid (S.idPosn name)


-- -----------------------------------------------------------------------------
nodeCall :: Lex.Token -> S.Args -> PM.ParserM S.Subroutine
nodeCall name args = return $ S.Call (S.Id (Lex.tkInput name) (Lex.tkPosn name)) args


-- -----------------------------------------------------------------------------
-- TODO: check tipo para regreso de funcion
nodeReturn :: S.Expression -> S.Instruction
nodeReturn val = S.Return val


{-
 * -----------------------------------------------------------------------------
 *                                  I/O
 * -----------------------------------------------------------------------------
-}

nodePrint :: [S.Expression] -> Lex.Token -> PM.ParserM S.Instruction
nodePrint es tk = return $ S.Print es


-- -----------------------------------------------------------------------------
nodeRead :: S.Expression -> U.Position -> PM.ParserM S.Expression
nodeRead e p = return $ S.Expression (S.Read $ S.expr e) S.TRead p


{-
 * -----------------------------------------------------------------------------
 *                                Expressions
 * -----------------------------------------------------------------------------
-}

nodeTArray :: S.Expression -> S.Type -> PM.ParserM S.Type
nodeTArray size t = return $ S.TArray size t


-- -----------------------------------------------------------------------------
nodeTStruct :: Lex.Token -> PM.ParserM S.Type
nodeTStruct tk = return $ S.TStruct (S.Struct (S.Id (Lex.tkInput tk) (Lex.tkPosn tk)) S.Record [] 0)


-- -----------------------------------------------------------------------------
nodeBinary :: S.BinOp -> S.Expression -> S.Expression -> Lex.Token -> PM.ParserM S.Expression
nodeBinary op le re tk = return $ S.Expression (S.Binary op (S.expr le) (S.expr re)) S.TVoid (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeAnexo :: S.Expression -> S.Expression -> Lex.Token -> PM.ParserM S.Expression
nodeAnexo h list tk = return $ S.Expression (S.ArrayList $ h : [list]) (S.TList S.TVoid) (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeConcatLists :: S.Expression -> S.Expression -> Lex.Token -> PM.ParserM S.Expression
nodeConcatLists l1 l2 tk = return $ S.Expression (S.ArrayList $ l1 : [l2]) (S.TList S.TVoid) (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeIfSimple :: S.Expression -> S.Expression -> S.Expression -> Lex.Token -> PM.ParserM S.Expression
nodeIfSimple cond true false tk = return $ S.Expression (S.IfSimple (S.expr cond) (S.expr true) (S.expr false)) S.TVoid (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
-- Check tipo esta definido
nodeNew :: S.Type -> U.Position -> PM.ParserM S.Expression
nodeNew t p = return $ S.Expression (S.Unary S.New S.PtrInit) (S.TPointer t) p


-- -----------------------------------------------------------------------------
nodeStruct :: Lex.Token -> [S.Expression] -> PM.ParserM S.Expression
nodeStruct tk es = return $ S.Expression (S.StructE es) S.TVoid (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeArray :: [S.Expression] -> Lex.Token -> PM.ParserM S.Expression
nodeArray es tk = return $ S.Expression (S.ArrayList es) (S.TArray (S.Expression S.EmptyVal S.TInt (0,0)) S.TVoid) (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeList :: [S.Expression] -> Lex.Token -> PM.ParserM S.Expression
nodeList es tk = return $ S.Expression (S.ArrayList es) (S.TList S.TVoid) (Lex.tkPosn tk)


-- -----------------------------------------------------------------------------
nodeUnary :: S.UnOp -> S.Expression -> S.Type -> Lex.Token -> PM.ParserM S.Expression
nodeUnary op e t tk = return $ S.Expression (S.Unary op $ S.expr e) t (Lex.tkPosn tk)


{-
 * -----------------------------------------------------------------------------
 *                                -
 * -----------------------------------------------------------------------------
-}

-- -----------------------------------------------------------------------------
defineTStruct :: Lex.Token -> S.Type -> PM.ParserM S.Id
defineTStruct tk t = return $ S.Id (Lex.tkInput tk) (Lex.tkPosn tk)
