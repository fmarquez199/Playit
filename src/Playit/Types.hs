{-
* Representacion de la gramatica para el analisis sintactico
*
* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.Types where

import Control.Monad.State
--import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.List (intercalate)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--             Tipos de datos que representan la estructura del AST
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Identificador de variable, registros, uniones y subrutinas
type Nombre = String

type Parametros = [Expr]

-- Identificador del nombre de un programa
type Programa = String

-- Alcance de la tabla de simbolos actual y de una variable
type Alcance = Integer

-- Posicion donde se encuentra la instruccion
type Posicion = (Int, Int)


type SecuenciaInstr = [Instr]


-- Informacion del identificador de variable
data IdInfo = IdInfo {getType :: Tipo, getVal :: Literal, getScope :: Alcance}
                deriving (Eq, Show, Ord)


-- Tipo de dato que pueden ser las expresiones
data Tipo   = TInt | TFloat | TBool | TChar | TStr | TArray Expr Tipo
            | TLista Tipo | TRegister | TUnion | TApuntador
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
            | ForEach Nombre Expr Expr Expr SecuenciaInstr SymTab
            | While Expr SecuenciaInstr
            | ButtonIF [(Expr,SecuenciaInstr)] 
            | Proc Parametros SecuenciaInstr
            | Func Parametros Tipo SecuenciaInstr
            | Free Nombre
            | CrearSubrutina Nombre Parametros
            | SubrutinaCall Nombre Parametros
            | Break
            | Continue
            | Return Expr
            | Print Expr
            | Read Vars
            deriving (Eq, Show)

data Expr   = OpBinario BinOp Expr Expr Tipo
            | OpUnario UnOp Expr Tipo
            | ListaExpr [Expr] Tipo
            | Variables Vars Tipo
            | Literal Literal Tipo
            deriving (Eq, Show, Ord)


data Literal    = Entero Int
                | Flotante Float
                | Caracter Char
                | Str String
                | Booleano Bool
                | Arreglo [Literal]
                | Lista 
                | ValorVacio
                deriving (Eq, Show, Ord)

data Compuesto  = Registro Nombre SecuenciaInstr
                deriving (Eq, Show)

-- Operadores binarios
data BinOp  = Suma
            | Resta
            | Multiplicacion
            | Division
            | DivEntera
            | Modulo
            | Menor
            | Mayor
            | MenorIgual
            | MayorIgual
            | Igual
            | Desigual
            | Anexo
            | Concatenacion
            | And
            | Or
            | IfSimple
            deriving (Eq, Show, Ord)


-- Operadores unarios
data UnOp   = Negativo
            | TamArregloLista
            | UpperCase
            | LowerCase
            | Incremento
            | Decremento
            | Desreferenciar
            | Not
            | New
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
newtype SymTab  = SymTab { getSymTab :: (M.Map Nombre IdInfo, Maybe SymTab) }
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
    showVar vars ++ "|}" ++ showE e ++ "{|"


-- Show de las expresiones
showE :: Expr -> String
showE (Literal lit _)                           = showL lit
showE (Variables vars _)                        = showVar vars
showE (OpBinario Suma e1 e2 _)           = showE e1 ++ " + " ++ showE e2
-- showE (OpBinario Punto e1 e2 _)          = showE e1 ++ " . " ++ showE e2
showE (OpBinario Resta e1 e2 _)          = showE e1 ++ " - " ++ showE e2
showE (OpBinario Modulo e1 e2 _)         = showE e1 ++ " % " ++ showE e2
showE (OpBinario Division e1 e2 _)       = showE e1 ++ " / " ++ showE e2
showE (OpBinario Multiplicacion e1 e2 _) = showE e1 ++ " * " ++ showE e2
showE (OpBinario Menor e1 e2 _)          = showE e1 ++ " < " ++ showE e2
showE (OpBinario Mayor e1 e2 _)          = showE e1 ++ " > " ++ showE e2
showE (OpBinario Igual e1 e2 _)          = showE e1 ++ " == " ++ showE e2
showE (OpBinario Desigual e1 e2 _)       = showE e1 ++ " != " ++ showE e2
showE (OpBinario MenorIgual e1 e2 _)     = showE e1 ++ " <= " ++ showE e2
showE (OpBinario MayorIgual e1 e2 _)     = showE e1 ++ " >= " ++ showE e2
showE (OpBinario Concatenacion e1 e2 _)  = showE e1 ++ " :: " ++ showE e2
showE (OpBinario Or e1 e2 _)             = showE e1 ++ " || " ++ showE e2
showE (OpBinario And e1 e2 _)            = showE e1 ++ " && " ++ showE e2
showE (OpUnario Negativo e _)            = "-" ++ showE e
showE (OpUnario Not e _)                 = "!" ++ showE e
showE (ListaExpr lst _)                  = "[" ++ intercalate "," (map showE lst) ++ "]"


-- 
showL :: Literal -> String
showL ValorVacio      = "Valor vacio"
showL (Entero val)    = show val
showL (Caracter val)  = show val
showL (Booleano val)  = show val
showL (Arreglo lst@(Entero _:_)) = show $ map ((\x->read x::Int) . showL) lst
showL (Arreglo lst@(Booleano _:_)) = show $ map ((\x->read x::Bool) . showL) lst
showL (Arreglo lst@(Caracter _:_)) = show $ map ((\x->read x::Char) . showL) lst
showL (Arreglo lst@(Arreglo _:_)) = show $ map showL lst

-- Show para los tipos
showType :: Tipo -> String
showType TInt         = "Entero(s)"
showType TFloat       = "Flotante(s)"
showType TChar        = "Caracter(es)"
showType TStr         = "String(s)"
showType TBool        = "Booleano(s)"
showType (TArray e t) = "Arreglo de tamaño " ++ showE e ++ " de " ++ showType t
showType _            = "Ivalido"
