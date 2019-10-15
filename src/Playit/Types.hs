{-
* Representacion de la gramatica para el analisis sintactico
*
* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.Types where

import Control.Monad.Trans.RWS
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

-- Identificador del nombre de un programa
type Programa = String

-- Posicion donde se encuentra la instruccion
type Posicion = (Int, Int)

type Parametros = [Expr]

type SecuenciaInstr = [Instr]


-- Tipo de dato que pueden ser las expresiones
data Tipo   = TInt | TFloat | TBool | TChar | TStr | TArray Expr Tipo
            | TLista Tipo | TRegistro | TUnion | TApuntador Tipo
            | TError    -- Tipo error, no machean los tipos como deben
            | TDummy    -- Tipo temporal cuando todavia no se lee el tipo de la
                        -- variable en una asignacion en las declaraciones o no
                        -- esta inicializada todavia (no tiene valor asignado)
            deriving(Eq, Show, Ord)


data Vars   = Var Nombre Tipo
            | VarIndex Vars Expr Tipo       --- Indice para array , listas
            | VarCompIndex Vars Nombre Tipo --- Variable de acceso a registros, uniones
            | Param Nombre Tipo Ref
            | PuffValue Vars Tipo           -- Variable deferenciada con puff
            deriving (Eq, Show, Ord)

data Ref    = Valor | Referencia
            deriving(Eq, Show, Ord)

data Instr  = Asignacion Vars Expr
            | BloqueInstr SecuenciaInstr SymTab
            | Registro Nombre SecuenciaInstr Tipo
            | Union Nombre SecuenciaInstr Tipo
            -- For (Nombre) = (Expr) <- (Expr) : (Instrucciones) (scope) 
            | For Nombre Expr Expr SecuenciaInstr SymTab
            -- For (Nombre) = (Expr)  <- (Expr) while (Expr) : (Instrucciones) (scope) 
            | ForWhile Nombre Expr Expr Expr SecuenciaInstr SymTab
            | ForEach Nombre Expr SecuenciaInstr SymTab
            | SecDeclaraciones SecuenciaInstr SymTab
            | While Expr SecuenciaInstr
            | ButtonIF [(Expr, SecuenciaInstr)]  -- [(cond,instruc)]
            | Proc Nombre Parametros SecuenciaInstr SymTab
            | Func Nombre Parametros Tipo SecuenciaInstr SymTab
            | Free Nombre
            | Break
            | Continue
            | Return Expr
            | Print Expr
            deriving (Eq, Show)


data Expr   = OpBinario BinOp Expr Expr Tipo
            | OpUnario UnOp Expr Tipo
            | IfSimple Expr Expr Expr Tipo
            | SubrutinaCall Nombre Parametros Tipo
            | ListaExpr [Expr] Tipo
            | Variables Vars Tipo
            | Literal Literal Tipo
            | Read Expr
            | ExprVacia
            deriving (Eq, Show, Ord)


data Literal    = Entero Int
                | Flotante Float
                | Caracter Char
                | Str String
                | Booleano Bool
                | ArrLst [Literal]      -- >> Arreglos y listas
                | ValorVacio
                deriving (Eq, Show, Ord)


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
            deriving (Eq, Show, Ord)


-- Operadores unarios
data UnOp   = Negativo
            | Longitud
            | UpperCase
            | LowerCase
            | Incremento
            | Decremento
            | Desreferenciar
            | Not
            | New
            | Len
            deriving (Eq, Show, Ord)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--             Tipos de datos que representan la tabla de simbolos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Alcance de un identificador
type Alcance = Integer

-- Pila de alcances
type StackScopes = [Alcance]

-- Informacion pertinente a la entrada de la tabla de simbolos
data SymbolInfo = SymbolInfo {
    getVal :: Literal,
    getType :: Tipo,
    getScope :: Alcance
    }
    deriving (Eq, Show, Ord)


{- Nuevo tipo de dato para representar la tabla de simbolos
* Tabla de hash:
*   Key: Nombre
*   Value: Lista de la informacion pertinente
-}
newtype SymTab  = SymTab { getSymTab :: M.Map Nombre [SymbolInfo] }
                deriving (Eq, Show)


-- Transformador monadico para crear y manejar la tabla de simbolos junto con 
-- la pila de alcances
type MonadSymTab a = RWST (SymTab, StackScopes) IO a


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                      Para mostrar mejor los errores
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Show de las variables
showVar :: Vars -> String
showVar (Var name _) = name
showVar (VarIndex vars e t) =
    showVar vars ++ "|)" ++ showE e ++ "(|" 
    -- TODO : Falta mostrar lista


-- Show de las expresiones
showE :: Expr -> String
showE (Literal lit _)                           = showL lit
showE (Variables vars _)                        = showVar vars
showE (OpBinario Suma e1 e2 _)           = "(" ++ showE e1 ++ " + " ++ showE e2 ++ ")"
-- showE (OpBinario Punto e1 e2 _)          = showE e1 ++ " . " ++ showE e2
showE (OpBinario Resta e1 e2 _)          = "(" ++ showE e1 ++ " - " ++ showE e2 ++ ")"
showE (OpBinario Modulo e1 e2 _)         = "(" ++ showE e1 ++ " % " ++ showE e2 ++ ")"
showE (OpBinario Division e1 e2 _)       = "(" ++ showE e1 ++ " / " ++ showE e2 ++ ")"
showE (OpBinario Multiplicacion e1 e2 _) = "(" ++ showE e1 ++ " * " ++ showE e2 ++ ")"
showE (OpBinario Menor e1 e2 _)          = "(" ++ showE e1 ++ " < " ++ showE e2 ++ ")"
showE (OpBinario Mayor e1 e2 _)          = "(" ++ showE e1 ++ " > " ++ showE e2 ++ ")"
showE (OpBinario Igual e1 e2 _)          = "(" ++ showE e1 ++ " == " ++ showE e2 ++ ")"
showE (OpBinario Desigual e1 e2 _)       = "(" ++ showE e1 ++ " != " ++ showE e2 ++ ")"
showE (OpBinario MenorIgual e1 e2 _)     = "(" ++ showE e1 ++ " <= " ++ showE e2 ++ ")"
showE (OpBinario MayorIgual e1 e2 _)     = "(" ++ showE e1 ++ " >= " ++ showE e2 ++ ")"
showE (OpBinario Concatenacion e1 e2 _)  = "(" ++ showE e1 ++ " :: " ++ showE e2 ++ ")"
showE (OpBinario Or e1 e2 _)             = "(" ++ showE e1 ++ " || " ++ showE e2 ++ ")"
showE (OpBinario And e1 e2 _)            = "(" ++ showE e1 ++ " && " ++ showE e2 ++ ")"
showE (OpUnario Negativo e _)            = "-" ++ showE e
showE (OpUnario Not e _)                 = "!" ++ showE e
showE (ListaExpr lst _)                  = "[" ++ intercalate "," (map showE lst) ++ "]"
showE (IfSimple e1 e2 e3 t)                = "(" ++ showE e1 ++ " ? " ++ showE e2 ++ " : " ++ showE e3 ++ ")"

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

getNombre :: Vars -> Nombre
getNombre (Param n _ _) = n
getNombre (Var n _) = n
getNombre (VarIndex v _ _) = getNombre v
