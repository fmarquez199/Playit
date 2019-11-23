{- |
 * Grammatical types
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.Types where

import Control.Monad.Trans.RWS
import qualified Data.Map as M
import Data.List (intercalate,elemIndex)


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                       AST representation data types
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- | Program Variables, registers, unions and subroutines ids
type Id = String

-- Instruction's position
type Pos = (Int, Int)

-- Subroutine's parameters / arguments 
type Params = [Expr]

-- Instruction sequence
type InstrSeq = [Instr]


-- Tipo de dato que pueden ser las expresiones
data Type = 
    TArray Expr Type |
    TBool            |
    TChar            |
    TDummy           | -- Temp for when the type its still unknown
    TPDummy          | -- Temp for when the subroutine is promised to be defined later
    TError           | -- Error type, type checks fail
    TFloat           |
    TInt             |
    TList Type       |
    TNew Id          |
    TNull            |
    TPointer Type    |
    TRead            | -- Cableado para que el input corra
    TRegister        |
    TStr             |
    TUnion           |
    TVoid              -- Procedures type by default
    deriving(Eq, Ord)

instance Show Type where
    show (TPointer t) = "(" ++ show t ++ "*)"
    show (TArray e t) = show t ++ "|}" ++ show e ++ "{|"
    show TBool        = "Battle"
    show TChar        = "Rune"
    show TError       = "Type error"   
    show TDummy       = "Type unkown"
    show TPDummy      = "Subroutine promise"
    show TFloat       = "Skill"
    show TInt         = "Power"
    show (TList t)    = "Kit of(" ++ show t ++ ")"
    show (TNew str)   = str
    show TNull        = "DeathZone*"
    show TRegister    = "Inventory"
    show TRead        = ""
    show TStr         = "Runes"
    show TUnion       = "Items"
    show TVoid        = "Void"

    
-- Kinds of variables
data Var =
    Param Id Type Ref   |
    Desref Var Type     | -- Desreferenced variable
    Var Id Type         |
    Index Var Expr Type | -- Indexed variable
    Field Var Id Type     -- Registers / unions field
    deriving (Eq, Ord)


instance Show Var where
    show (Param n t Value) = "Parameter: " ++ {-"("++show t++")"++-}n
    show (Param n t _)     = "Parameter: ?" ++ {-"("++show t++")"++-}n
    show (Desref v t)      = {-"("++show t++")"++-}"puff (" ++ show v ++ ")"
    show (Var n t)         = {-"("++show t++")"++-}n
    show (Index v e t)     = {-"("++show t++")"++-}show v ++ " index: " ++ show e
    show (Field v n t)     = {-"("++show t++") "++-}"("++show v ++ " spawn " ++ n

-- Specify if a parameter is by value or reference
data Ref =
    Reference |
    Value
    deriving(Eq, Show, Ord)

-- Instructions
data Instr  = 
    Assig Var Expr                      |
    Break                               |
    Continue                            |
    For Id Expr Expr InstrSeq           |
    ForEach Id Expr InstrSeq            |
    ForWhile Id Expr Expr Expr InstrSeq |
    Free Id                             |
    Print [Expr]                        |
    ProcCall Subroutine                 |
    Program InstrSeq                    |
    Return Expr                         |
    Assigs InstrSeq                     |
    IF [(Expr, InstrSeq)]               |
    While Expr InstrSeq
    deriving (Eq,Ord)

instance Show Instr where
    show (Assig v e)             = "  " ++ show v ++ " = " ++ show e ++ "\n"
    show Break                   = "  GameOver\n"
    show Continue                = "  KeepPlaying\n"
    show (For n e1 e2 s)         = "  controller " ++ n ++ " = " ++ show e1 ++ " -> "
        ++ show e2 ++ ":\n  " ++ intercalate "  " (map show s) ++ "\n  .~\n"

    show (ForEach n e s)         = "  controller " ++ n ++ " <- " ++ show e ++
        ":\n  " ++ intercalate "  " (map show s) ++ "\n  .~\n"

    show (ForWhile n e1 e2 e3 s) = "  controller " ++ n ++ " = " ++ show e1 ++ " -> " ++
        show e2 ++ " lock " ++ show e3 ++ ":\n  " ++ 
        intercalate "  " (map show s) ++ "\n  .~\n"

    show (Free n)                = "  free " ++ n ++ "\n"
    show (Print e)               = "  drop " ++ show e ++ "\n"
    show (ProcCall s)            = "  kill " ++ show s ++ "\n"
    show (Program s)             = "\nworld:\n" ++ concatMap show s ++ ".~\n"
    show (Return e)              = "  unlock " ++ show e
    show (Assigs s)              = intercalate "  " (map show s)
    show (IF s)                  = "  Button:\n  " ++ concatMap (++"\n  ") guards ++ ".~\n"
        where
            conds = map (show . fst) s
            instrs =  map (concatMap show . snd) s
            guards = ["| "++c++" }\n    "++i | c<-conds,i<-instrs,elemIndex c conds==elemIndex i instrs]

    show (While e s)             = "  play:\n    " ++
        intercalate "    " (map show s) ++ "  lock " ++ show e ++ "\n  .~\n"


data Subroutine = Call Id Params    deriving (Eq, Ord)

instance Show Subroutine where
    show (Call n p) = n ++ "(" ++ intercalate "," (map show p) ++ ")"


-- Expressions
data Expr   = 
    ArrayList [Expr] Type        |
    Binary BinOp Expr Expr Type  |
    FuncCall Subroutine Type     |
    IdType Type                  |
    IfSimple Expr Expr Expr Type |
    Literal Literal Type         |
    Null                         | -- tipo: compatible con apt de lo que sea o que el contexto lo diga
    Unary UnOp Expr Type         |
    Read Expr Type               |
    Variable Var Type
    deriving (Eq, Ord)

instance Show Expr where
    show (ArrayList lst t)     = {-"("++show t++")"++-}"[" ++ intercalate "," (map show lst) ++ "]"
    show (FuncCall s t)        = {-"("++show t++")"++-}"kill " ++ show s
    show (IdType t)            = show t
    show (IfSimple e1 e2 e3 t) = {-"("++show t++")"++-}show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3
    show (Literal lit t)       = {-"("++show t++")"++-}show lit
    show Null                  = "DeathZone"
    show (Binary op e1 e2 t)   = {-"("++show t++")"++-}"(" ++ show e1 ++ show op ++ show e2 ++ ")"
    show (Unary op e1 t)       = {-"("++show t++")"++-}show op ++ show e1
    show (Read e t)            = "joystick " ++ show e
    show (Variable var t)      = {-"E("++show t++")"++-}show var

data Literal =
    ArrLst [Literal] | -- >> Arrays and lists
    Boolean Bool     |
    Character Char   |
    EmptyVal         |
    Floatt Float     |
    Integer Int      |
    Register [Expr]  |
    Str String       
    deriving (Eq, Ord)

instance Show Literal where
    show (ArrLst l@(ArrLst _:_))    = show $ map show l 
    show (ArrLst l@(Boolean _:_))   = show $ map ((\x->read x::Bool) . show) l
    show (ArrLst l@(Character _:_)) = show $ map ((\x->read x::Char) . show) l
    show (ArrLst l@(Integer _:_))   = show $ map ((\x->read x::Int) . show) l
    show (ArrLst l@(Floatt _:_))    = show $ map ((\x->read x::Float) . show) l
    show (Register inits)           = "{" ++ intercalate "," (map show inits) ++ "}"
    show (ArrLst l)                 = concatMap show l
    show (Boolean val)              = show val
    show (Character val)            = show val
    show (Integer val)              = show val
    show (Floatt val)               = show val
    show (Str val)                  = val
    show EmptyVal                   = "Empty Value"


-- Binary operators
data BinOp =
    And         |
    Anexo       |
    Concat      |
    NotEq       |
    DivEntera   |
    Division    |
    Eq          |
    Greater     |
    GreaterEq   |
    Less        |
    LessEq      |
    Module      |
    Mult        |
    Or          |
    Minus       |
    Add
    deriving (Eq, Ord)

instance Show BinOp where
    show And       = " && "
    show Anexo     = " : "
    show Concat    = " :: "
    show NotEq     = " != "
    show DivEntera = " // "
    show Division  = " / "
    show Eq        = " == "
    show Greater   = " > "
    show GreaterEq = " >= "
    show Less      = " < "
    show LessEq    = " <= "
    show Module    = " % "
    show Mult      = " * "
    show Or        = " || "
    show Minus     = " - "
    show Add       = " + "


-- Unary operators
data UnOp =
    Length    |
    LowerCase |
    Negative  |
    New       |
    Not       |
    UpperCase
    deriving (Eq, Ord)

instance Show UnOp where
    show Length    = "#"
    show LowerCase = "."
    show Negative  = "-"
    show New       = "summon "
    show Not       = "!"
    show UpperCase = "^"


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                          Symbol table data types
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Id scope
type Scope = Integer

-- Static chain
type ActiveScopes = [Scope]

-- Symbols categories
data Category  = 
    Constants         |
    Fields            |
    Functions         |
    IterationVariable |
    Parameters Ref    |
    Pointers          |
    Procedures        |
    TypeConstructors  |
    Types             |
    Variables
    deriving (Eq, Ord)

instance Show Category where
    show Constants         = "Constants"
    show Fields            = "Fields"
    show Functions         = "Functions"
    show IterationVariable = "Iteration's Variable"
    show (Parameters r)    = "Parameters by " ++ show r
    show Pointers          = "Pointers"
    show Procedures        = "Procedures"
    show TypeConstructors  = "Type Constructors"
    show Types             = "Types"
    show Variables         = "Variables"

-- Symbol extra information
data ExtraInfo =
    AST InstrSeq       |
    Params [(Type,Id)] |
    FromReg Id         -- Register / union al que pertenece el campo/variable
    deriving (Eq, Ord)

instance Show ExtraInfo where
    show (AST s)     = "    AST:\n      " ++ intercalate "\t  " (map show s)
    show (Params p)  = "    Parameters: " ++ show p ++ "\n"
    show (FromReg n) = "    Campo del registro: " ++ show n


-- Symbol related information in the symbol table entry
data SymbolInfo = SymbolInfo {
    getType :: Type,
    getScope :: Scope,
    getCategory :: Category,
    getExtraInfo :: [ExtraInfo]
    }
    deriving (Eq, Ord)

instance Show SymbolInfo where
    show (SymbolInfo t s c i) = "\n  Type: " ++ show t ++ " | Scope: " ++
        show s ++ " | Category: "++ show c ++
        if not (null i) then
            "\n    Extra:\n  " ++ intercalate "  " (map show i) ++ "\n"
        else ""


-- Subroutine promise for co-recursive subroutines
data Promise = Promise {
    getIdPromise :: Id,
    getParamsPromise :: [Type],
    getTypePromise :: Type,
    getPosPromise :: Pos
    }
    deriving (Eq, Ord)

type Promises = [Promise]


{- | New type that represents the symbol table
 * Hash table:
 *   Key: Id
 *   Value: Symbol information
-}
newtype SymTab  = SymTab { getSymTab :: M.Map Id [SymbolInfo] }
                deriving (Eq)

instance Show SymTab where
    show (SymTab st) = header ++ info ++ symbols
        where
            header = "\n------------\n Symbol table \n------------\n"
            info = "- Symbol | Related information \n------------\n"
            table = M.toList st
            stWithScopes = M.map (map getScope) st
            symbols' = map fst $ M.toList $ M.filter (any (>0)) stWithScopes
            showInfo i = if getScope i > 0 then show i else ""
            showTable (k,v) = 
                if k `elem` symbols' then
                    k ++ " -> " ++ concatMap showInfo (reverse v) ++ "\n"
                else ""
            symbols = concatMap showTable table


-- State that stores the symbol table, active scopes, total scopes and subroutines promises
type SymTabState = (SymTab, ActiveScopes, Scope, Promises)

-- Reader that stores the file name and the code for better show of errors
type FileCodeReader = (String,String)

-- Symbol table, active scopes and total scopes monad transformer handler
type MonadSymTab a = RWST FileCodeReader [String] SymTabState IO a