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


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--             Tipos de datos que representan la estructura del AST
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- Identificador de variable, registros, uniones y subrutinas
type Nombre = String

-- Identificador del nombre de un programa
type Programa = String

-- Posicion donde se encuentra la instruccion
type Posicion = (Int, Int)

type Parametros = [Expr]

type SecuenciaInstr = [Instr]


-- Categorias a las que pertenecen los simbolos de la tabla de simbolos
data Categoria  = Variable
                | Parametros
                | Constructores
                | ConstructoresTipos
                | Tipos
                | Apuntadores
                | Procedimientos
                | Funciones
                deriving (Eq, Show, Ord)


-- Tipo de dato que pueden ser las expresiones
data Tipo   = TInt | TFloat | TBool | TChar | TStr | TRegistro | TUnion
            | TArray Expr Tipo | TLista Tipo | TApuntador Tipo
            | TError    -- Tipo error, no machean los tipos como deben
            | TDummy    -- Tipo temporal cuando todavia no se lee el tipo de la
                        -- variable en una asignacion en las declaraciones o no
                        -- esta inicializada todavia (no tiene valor asignado)
            deriving(Eq, Show, Ord)


data Vars   = Var Nombre Tipo
            | VarIndex Vars Expr Tipo       -- Indice para array, listas
            | VarCompIndex Vars Nombre Tipo -- Campos de los registros y uniones
            | Param Nombre Tipo Ref
            | PuffValue Vars Tipo           -- Variable deferenciada con puff
            deriving (Eq, Show, Ord)


-- Especifica si un parametro es pasado como valor o por referencia
data Ref    = Valor | Referencia
            deriving(Eq, Show, Ord)


data Instr  = Asignacion Vars Expr
            | Programa Cosas
            | SecDeclaraciones SecuenciaInstr
            -- For var = (Expr) -> (Expr) while (Expr) : (Instrucciones)
            | ForWhile Nombre Expr Expr Expr SecuenciaInstr
            -- For var = (Expr) -> (Expr) : (Instrucciones)
            | For Nombre Expr Expr SecuenciaInstr
            | ForEach Nombre Expr SecuenciaInstr
            | Switch [(Expr, SecuenciaInstr)]  -- [(cond, instruc)]
            | While Expr SecuenciaInstr
            | ProcCall Subrutina
            | Free Nombre
            | Return Expr
            | Print Expr
            | Break
            | Continue
            deriving (Eq, Show)


-- Lo que se puede escribir dentro de un programa
data Cosas = SecInstr SecuenciaInstr | Definiciones Definicion {-| Nada-}
            deriving (Eq, Show)

-- 
data Subrutina = SubrutinaCall Nombre Parametros
                deriving (Eq, Show, Ord)


-- Definiciones de las subrutinas, registros y uniones
data Definicion = Proc Nombre Parametros SecuenciaInstr
                | Func Nombre Parametros Tipo SecuenciaInstr
                | Registro Nombre SecuenciaInstr Tipo
                | Union Nombre SecuenciaInstr Tipo
                deriving (Eq, Show)


data Expr   = OpBinario BinOp Expr Expr Tipo
            | OpUnario UnOp Expr Tipo
            | IfSimple Expr Expr Expr Tipo
            | FuncCall Subrutina Tipo
            | ArrLstExpr [Expr] Tipo
            | Variables Vars Tipo
            | Literal Literal Tipo
            | Read Expr
            | Null
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


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--             Tipos de datos que representan la tabla de simbolos
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- Alcance de un identificador
type Alcance = Integer

-- Pila de alcances. El ultimo alcance agregado es el primero de la lista
type StackScopes = [Alcance]

-- Informacion pertinente a la entrada de la tabla de simbolos
data SymbolInfo = SymbolInfo {
    getType :: Tipo,
    getScope :: Alcance,
    getCategory :: Categoria
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
type MonadSymTab a = RWST () () (SymTab, StackScopes) IO a


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                      Para mostrar mejor los errores
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- Show de las variables
showVar :: Vars -> String
showVar (Var name _) = name
showVar (Param name _ _) = name
showVar (PuffValue vars _) = showVar vars
showVar (VarIndex vars e _) =
    showVar vars ++ "|)" ++ showE e ++ "(|" 
    -- TODO : Falta mostrar lista

getTypeVar :: Vars -> Tipo
getTypeVar (Var _ t) = t
getTypeVar (Param _ t _) = t
getTypeVar (PuffValue _ t) = t
getTypeVar (VarIndex _ _ t) = t

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
showE (ArrLstExpr lst _)                  = "[" ++ intercalate "," (map showE lst) ++ "]"
showE (IfSimple e1 e2 e3 t)                = "(" ++ showE e1 ++ " ? " ++ showE e2 ++ " : " ++ showE e3 ++ ")"

-- 
showL :: Literal -> String
showL ValorVacio      = "Valor vacio"
showL (Entero val)    = show val
showL (Caracter val)  = show val
showL (Booleano val)  = show val
showL (ArrLst lst@(Entero _:_)) = show $ map ((\x->read x::Int) . showL) lst
showL (ArrLst lst@(Booleano _:_)) = show $ map ((\x->read x::Bool) . showL) lst
showL (ArrLst lst@(Caracter _:_)) = show $ map ((\x->read x::Char) . showL) lst
showL (ArrLst lst@(ArrLst _:_)) = show $ map showL lst

-- Show para los tipos
showType :: Tipo -> String
showType TInt         = "Entero(s)"
showType TFloat       = "Flotante(s)"
showType TChar        = "Caracter(es)"
showType TStr         = "String(s)"
showType TBool        = "Booleano(s)"
showType (TArray e t) = "Arreglo de tamaño " ++ showE e ++ " de " ++ showType t
showType _            = "Ivalido"

getNombreParametro :: Expr -> Nombre
getNombreParametro (Variables x _) = showVar x
getNombreParametro _ = ""

getTipoParametro :: Expr -> Tipo
getTipoParametro (Variables x _) = getTypeVar x
getTipoParametro _ = TError

getNombreSubrutina :: Subrutina -> Nombre
getNombreSubrutina (SubrutinaCall n _) = n