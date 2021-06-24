{- |
 * Types that represents the abstract syntax
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.FrontEnd.Syntax where

import Data.List                       (intercalate)

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Playit.Utils               as U

import qualified Playit.FrontEnd.Lexer      as Lex (Token(..))


-- -----------------------------------------------------------------------------
type Fields = [Declaration]

-- Kind of structs
data StructKind = Record | Union  deriving (Eq, Show, Ord)

data Struct = Struct
  { sName   :: Id
  , sKind   :: StructKind
  , sFields :: Fields
  , sSize   :: Int
  } deriving(Eq, Ord)

instance Show Struct where
  show (Struct name kind fields size) = show kind ++ " " ++ show name

-- Expression's data types
data Type
  -- Basics
  = TInt
  | TFloat
  | TBool
  | TChar
  -- Composed
  | TStr
  | TRecord -- puede ser redundante/ innecesario?
  | TUnion  -- puede ser redundante/ innecesario?
  | TStruct    Struct
    -- podria ver si colocar un campo para el tipo base (Inr, Floar, Bool, Char)
  | TArray     Expression Type
  | TList      Type
  | TPointer   Type
  | TEmptyList
  | TNull
  --
  | TVoid   -- Procedures type by default
  | TRead   -- TODO: Cableado para que el input corra
  | TDummy  -- Temp for when the type its still unknown
  | TPDummy -- Temp for when the subroutine is promised to be defined later
  | TError  -- Error type, type checks fail
  -- | FunctionT [Type] Type
  -- | ProcedureT [Type]
  -- | TypeList [Type]
  deriving(Ord)

instance Eq Type where
  TInt        == TInt        = True
  TFloat      == TFloat      = True
  TBool       == TBool       = True
  TChar       == TChar       = True
  TStr        == TStr        = True
  TRecord     == TRecord     = True
  TUnion      == TUnion      = True
  TEmptyList  == TEmptyList  = True
  TNull       == TNull       = True
  TVoid       == TVoid       = True
  TRead       == TRead       = True
  TStruct s1  == TStruct s2  = sName s1 == sName s2 && sKind s1 == sKind s2
  TArray _ t1 == TArray _ t2 = t1 == t2
  TList t1    == TList t2    = t1 == t2
  TPointer t1 == TPointer t2 = t1 == t2 
  TDummy      == TDummy      = True -- ??
  TPDummy     == TPDummy     = True -- ??
  TError      == TError      = True
  _           == _             = False

instance Show Type where
  show TInt             = "Power "
  show TFloat           = "Skill "
  show TBool            = "Battle "
  show TChar            = "Rune "
  show TStr             = "Runes "
  show TRecord          = "Inventory "
  show TUnion           = "Items "
  show (TArray size t)  = show t ++ "|}" ++ show size ++ "{| "
  show (TList t)        = "Kit of (" ++ show t ++ ") "
  show (TStruct struct) = show . Lex.tkInput . idTk $ sName struct
  show (TPointer t)     = show t ++ "* "
  show TEmptyList       = "Empty Kit "
  show TNull            = "TNull*  "
  show TVoid            = "Void "
  show TRead            = ""
  show TDummy           = "Type unkown "
  show TPDummy          = "Unknown return type "
  show TError           = "Type error "   


-- -----------------------------------------------------------------------------
-- | Program variables, records, unions and subroutines ids
data Id = Id
  { idTk :: Lex.Token
  -- , idScope :: U.Scope
  -- , isOffset :: Int
  } deriving(Ord)

instance Show Id where
  show i = BLC.unpack $ Lex.tkInput (idTk i)

instance Eq Id where
  Id tk1 == Id tk2 = tk1 == tk2

data Var = Var
  { varId   :: Id
  , var     :: VarKind
  , varType :: Type
  } deriving(Eq, Ord)

instance Show Var where
  show v = show (var v) ++ (BLC.unpack . Lex.tkInput . idTk $ varId v)

-- instance Eq Var where
--   Var var1 == Var var2 = varId var1 == varId var2 &&
--                          var var1 == var var2 &&
--                          varType var1 == varType var2

-- For subroutines parameters
data Ref = Reference | Value  deriving (Eq, Show, Ord)

-- Kind of variables
data VarKind
  = Variable
  | Param Ref
  | Deref VarKind            -- Dereferenced variable
  | Index VarKind Expression       -- Indexed variable
  | Field VarKind -- StructKind -- Registers / unions field
  deriving (Eq, Ord)

instance Show VarKind where
  show Variable      = "Variable: "
  show (Param Value) = "Parameter: "
  show (Param _)     = "Parameter: ?"
  show (Deref v)     = "puff (" ++ show v ++ ")"
  show (Index v e)   = show v ++ "[" ++ show e ++ "]"
  show (Field v)     = show v ++ " spawn "


-- -----------------------------------------------------------------------------
-- Subroutine's parameters / arguments
type Params = [Expression]

-- Functions and Procedures
data Subroutine = Call Id Params  deriving (Eq, Ord)

instance Show Subroutine where
  show (Call n p) = show n ++ "(" ++ intercalate "," (map show p) ++ ")"


-- -----------------------------------------------------------------------------
-- Instruction sequence
type InstrSeq = [Instruction]

data Instruction = Instruction
  { instr     :: InstrKind
  , instrType :: Type
  } deriving (Eq, Ord)

instance Show Instruction where
  show is = show (instr is)

data Initialization = Initialization -- tal ves cambiar 'id' por 'var', seria una asignacion?
  { initId   :: Id
  , initExpr :: Expression
  } deriving(Eq, Ord)

instance Show Initialization where
  show (Initialization name val) = show name ++ " equip " ++ show val ++ ", "

data Declaration = Declaration
  { declType  :: Type
  , declInits :: [Initialization]
  } deriving(Eq, Ord)

instance Show Declaration where
  show (Declaration t inits) = show t ++ concatMap show inits

data Guard = Guard
  { guardCond   :: Expression
  , guardInstrs :: InstrSeq
  } deriving(Eq, Ord)

instance Show Guard where
  show (Guard e is) = "\n  | " ++ show e ++ " }\n  " ++ concatMap show is

data InstrKind
  = Program   InstrSeq
  | Decl      Declaration
  | Assig     Var Expression
  | Break
  | Continue
  | ProcCall  Subroutine
  | Selection [Guard]
  | Return    Expression
  | Free      Id -- TODO: Cambiar Id por Expression(Var?), ayuda en TAC
  | Print     [Expression]
  | While     Expression InstrSeq
  | ForEach   Var Expression InstrSeq
  | For       Var Expression Expression InstrSeq
  | ForWhile  Var Expression Expression Expression InstrSeq
  deriving (Eq, Ord)

tabs tab i = U.joinWith tab i
endB = "\n  .~\n"

instance Show InstrKind where
  show (Program p)            = "\nworld:\n" ++ concatMap show p ++ ".~\n"
  show (Decl d)               = "  " ++ show d ++ "\n"
  show (Assig v e)            = "  " ++ show v ++ " equip " ++ show e ++ "\n"
  show Break                  = "  GameOver\n"
  show Continue               = "  KeepPlaying\n"
  show (ProcCall s)           = "  kill " ++ show s ++ "\n"
  show (Selection guards)     = "  Button:" ++ concatMap show guards ++ "\n.~\n"
  show (Return val)           = "  unlock " ++ show val
  show (Free var)             = "  free " ++ show var ++ "\n"
  show (Print es)             = "  drop " ++ show es ++ "\n"
  show (While c i)            = "  dungeon:\n    " ++ tabs "    " i ++ "  cleared " ++ show c ++ endB
  show (ForEach v e i)        = "  farm " ++ show v ++ " in " ++ show e ++ ":\n  " ++ tabs "  " i ++ endB
  show (For v e1 e2 i)        = 
    "  farm " ++ show v ++ " equip " ++ show e1 ++ " until " ++ show e2++ ":\n  " ++ tabs "  " i ++ endB
  show (ForWhile v e1 e2 c i) =
    "  farm " ++ show v ++ " equip " ++ show e1 ++ " until " ++ show e2 ++ " lock " ++ show c
    ++ ":\n  " ++ tabs "  " i ++ endB


-- -----------------------------------------------------------------------------
data Expression = Expression
  { expr     :: ExprKind
  , exprType :: Type
  , exprPosn :: U.Position
  } deriving (Ord)

instance Show Expression where
  show expression = show (expr expression)

instance Eq Expression where
  Expression e1 t1 _ == Expression e2 t2 _ = e1 == e2 && t1 == t2

data ExprKind
  = Rval      Var
  | Read      ExprKind
  | ArrayList [Expression] -- no expr porque no sumas arreglos, solo los indexas
  | StructE   [Expression] -- Struct
  | FuncCall  Subroutine
  | Unary     UnOp ExprKind
  | Binary    BinOp ExprKind ExprKind
  | IfSimple  ExprKind ExprKind ExprKind
  | Boolean   BL.ByteString
  | Integer   BL.ByteString
  | Floatt    BL.ByteString
  | Chr       BL.ByteString
  | Str       BL.ByteString
  | PtrInit   -- tipo al que se le hace malloc, 'new' de C
  | Null      -- tipo: compatible con apt de lo que sea (o hacer que el contexto lo diga)
  | EmptyVal         
  deriving (Eq, Ord)

instance Show ExprKind where
  show (Rval var)       = show var
  show (Read e)         = "joystick " ++ show e
  show (ArrayList lst)  = "[" ++ U.joinWith "," lst ++ "]"
  show (StructE inits)  = "{" ++ U.joinWith "," inits ++ "}"
  show (FuncCall s)     = "kill " ++ show s
  show (Unary op e)     = show op ++ show e
  show (Binary op l r)  = "(" ++ show l ++ show op ++ show l ++ ")"
  show (IfSimple c t f) = show c ++ " quest " ++ show t ++ " loot " ++ show f
  show (Boolean val)    = show val
  show (Integer val)    = show val
  show (Floatt val)     = show val
  show (Chr val)        = show val
  show (Str val)        = show val
  show PtrInit          = "PtrInit: "
  show Null             = "DeathZone"
  show EmptyVal         = "Empty Value"


-- -----------------------------------------------------------------------------
-- Binary operators
data BinOp =
   -- Numerics
  Add   | Minus  | Mult | Div | IntDiv | Mod |
   -- Lists
  Anexo | Concat |
   -- Booleans
  And   | Or | Eq | Neq | Gt | Gte | Lt | Lte
  deriving (Eq, Ord)

instance Show BinOp where
  show Add    = " + "
  show Minus  = " - "
  show Mult   = " * "
  show Div    = " / "
  show IntDiv = " // "
  show Mod    = " % "
  show Anexo  = " : "
  show Concat = " :: "
  show And    = " && "
  show Or     = " || "
  show Eq     = " == "
  show Neq    = " != "
  show Gt     = " > "
  show Gte    = " >= "
  show Lt     = " < "
  show Lte    = " <= "

-- Unary operators
data UnOp = Len | Negate | New | Not | LowerCase | UpperCase deriving (Eq, Ord)

instance Show UnOp where
  show Len       = "#"
  show Negate    = "-"
  show New       = "summon "
  show Not       = "!"
  show UpperCase = "buff "
  show LowerCase = "debuff "

