{-
* Representacion de la gramatica para el analisis sintactico
*
* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Types where

import Control.Monad.Trans.State
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.List (intercalate)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--             Tipos de datos que representan la estructura del AST
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Identificador de variable
type Nombre = String


-- Alcance de la tabla de simbolos actual y de una variable
type Alcance = Integer


-- Posicion donde se encuentra la instruccion
type Posicion = (Int, Int)


type SecuenciaInstr = [Instr]


-- Informacion del identificador de variable
data VarInfo = VarInfo {getType :: Tipo, getVal :: Literal, getScope :: Alcance}
                deriving (Eq, Show, Ord)


-- Tipo de dato que pueden ser las expresiones
data Tipo   = TInt | TBool | TChar | TArray Expr Tipo
            | TError    -- Tipo error, no machean los tipos como deben
            | TDummy    -- Tipo temporal cuando todavia no se lee el tipo de la
                        -- variable en una asignacion en las declaraciones o no
                        -- esta inicializada todavia (no tiene valor asignado)
            deriving(Eq, Show, Ord)


data Vars   = VarIndex Vars Expr Tipo
            | Var Nombre Tipo
            deriving (Eq, Show, Ord)


data Instr  = Asignacion Vars Expr
            | BloqueInstr SecuenciaInstr SymTab
            | For Nombre Expr Expr SecuenciaInstr SymTab
            | For_with_Step Nombre Expr Expr Expr SecuenciaInstr SymTab
            | While Expr SecuenciaInstr
            | If Expr SecuenciaInstr
            | If_Otherwise Expr SecuenciaInstr SecuenciaInstr
            | Print Expr
            | Read Vars
            deriving (Eq, Show)


data Expr   = Operador_Binario BinOp Expr Expr Tipo
            | Operador_Unario UnOp Expr Tipo
            | ListaExpr [Expr] Tipo
            | Variables Vars Tipo
            | Literal Literal Tipo
            deriving (Eq, Show, Ord)


data Literal    = Entero Int
                | Caracter Char
                | Booleano Bool
                | Arreglo [Literal]
                | ValorVacio
                deriving (Eq, Show, Ord)


-- Operadores binarios
data BinOp  = Suma
            | Resta
            | Multiplicacion
            | Division
            | Modulo
            | Menor
            | Mayor
            | Menor_Igual
            | Mayor_Igual
            | Igual
            | Desigual
            | Concatenacion
            | And
            | Or
            deriving (Eq, Show, Ord)


-- Operadores unarios
data UnOp   = Negativo
            | Not
            deriving (Eq, Show, Ord)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--             Tipos de datos que representan la tabla de simbolos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Nuevo tipo de dato para representar la tabla de simbolos
-- Primer elemento del par: Tabla donde se guardaran las declaraciones de las
--      variables con su tipo
-- 
-- Segundo elemento del par: Tabla padre del alcance externo justo anterior
-- 
newtype SymTab  = SymTab { getSymTab :: (M.Map Nombre VarInfo, Maybe SymTab) }
                deriving (Eq, Show)


-- Transformador monadico para crear y manejar la tabla de simbolos en el 
-- alcance actual
type MonadSymTab a = StateT (SymTab, Alcance) IO a



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                      Para mostrar mejor los errores
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Show de las variables
showVar :: Vars -> String
showVar (Var name _) = name
showVar (VarIndex vars e _) =
    showVar vars ++ "[" ++ showE e ++ "]"


-- Show de las expresiones
showE :: Expr -> String
showE (Literal lit _)                           = showL lit
showE (Variables vars _)                        = showVar vars
showE (Operador_Binario Suma e1 e2 _)           = showE e1 ++ " + " ++ showE e2
showE (Operador_Binario Punto e1 e2 _)          = showE e1 ++ " . " ++ showE e2
showE (Operador_Binario Resta e1 e2 _)          = showE e1 ++ " - " ++ showE e2
showE (Operador_Binario Modulo e1 e2 _)         = showE e1 ++ " % " ++ showE e2
showE (Operador_Binario Division e1 e2 _)       = showE e1 ++ " / " ++ showE e2
showE (Operador_Binario Multiplicacion e1 e2 _) = showE e1 ++ " * " ++ showE e2
showE (Operador_Binario Menor e1 e2 _)          = showE e1 ++ " < " ++ showE e2
showE (Operador_Binario Mayor e1 e2 _)          = showE e1 ++ " > " ++ showE e2
showE (Operador_Binario Igual e1 e2 _)          = showE e1 ++ " = " ++ showE e2
showE (Operador_Binario Desigual e1 e2 _)       = showE e1 ++ " /= " ++ showE e2
showE (Operador_Binario Menor_Igual e1 e2 _)    = showE e1 ++ " <= " ++ showE e2
showE (Operador_Binario Mayor_Igual e1 e2 _)    = showE e1 ++ " >= " ++ showE e2
showE (Operador_Binario Concatenacion e1 e2 _)  = showE e1 ++ " :: " ++ showE e2
showE (Operador_Binario Or e1 e2 _)             = showE e1 ++ " \\/ " ++ showE e2
showE (Operador_Binario And e1 e2 _)            = showE e1 ++ " /\\ " ++ showE e2
showE (Operador_Unario Negativo e _)            = "-" ++ showE e
showE (Operador_Unario Not e _)                 = "not" ++ showE e
showE (ListaExpr lst _)                         = "[" ++ (intercalate "," $ map showE lst) ++ "]"


-- 
showL :: Literal -> String
showL (Entero val)    = show val
showL (Caracter val)  = show val
showL (Booleano val)  = show val
showL (ValorVacio)    = "Valor vacio"
showL (Arreglo lst@((Entero _):_)) = show $ map (\x->read x::Int) $ map showL lst
showL (Arreglo lst@((Booleano _):_)) = show $ map (\x->read x::Bool) $ map showL lst
showL (Arreglo lst@((Caracter _):_)) = show $ map (\x->read x::Char) $ map showL lst
showL (Arreglo lst@((Arreglo _):_)) = show $ map showL lst

-- Show para los tipos
showType :: Tipo -> String
showType (TInt)       = "Entero(s)"
showType (TChar)      = "Caracter(es)"
showType (TBool)      = "Booleano(s)"
showType (TArray e t) = "Arreglo de tamaño " ++ showE e ++ " de " ++ showType t
showType _            = "Ivalido"