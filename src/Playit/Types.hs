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

type Sentencias = [Sentencia]

type SecuenciaInstr = [Instr]


-- Categorias a las que pertenecen los simbolos de la tabla de simbolos
data Categoria  = 
    Apuntadores        |
    Campos             |
    Constantes         |
    ConstructoresTipos |
    Funciones          |
    Parametros Ref     |
    Procedimientos     |
    Tipos              |
    Variable
    deriving (Eq, Ord)

instance Show Categoria where
    show Apuntadores        = "Apuntador"
    show Campos             = "Campos"
    show Constantes         = "Constante"
    show ConstructoresTipos = "Constructor de Tipos"
    show Funciones          = "Funciones"
    show (Parametros r)     = "Parametro por " ++ show r
    show Procedimientos     = "Procedimiento"
    show Tipos              = "Tipo"
    show Variable           = "Variable"


-- Tipo de dato que pueden ser las expresiones
data Tipo = 
    TApuntador Tipo  |
    TArray Expr Tipo |
    TBool            |
    TChar            |
    TDummy           | -- Tipo temporal cuando todavia no se lee el tipo de la
                       -- variable en una asignacion en las declaraciones o no
                       -- esta inicializada todavia (no tiene valor asignado)
    TError           | -- Tipo error, no machean los tipos como deben
    TFloat           |
    TInt             |
    TLista Tipo      |
    TRegistro        |
    TStr             |
    TUnion
    deriving(Eq, Ord)


instance Show Tipo where
    show (TApuntador t) = "Apuntador de " ++ show t
    show (TArray e t)   = "Arreglo de tamaño " ++ show e ++ " de " ++ show t
    show TBool          = "Booleano"
    show TChar          = "Caracter"
    show TDummy         = "Sin tipo definido aun"
    show TError         = "Mal tipado"   
    show TFloat         = "Flotante"
    show TInt           = "Entero"
    show (TLista t)     = "Lista de " ++ show t
    show TRegistro      = "Registro"
    show TStr           = "String"
    show TUnion         = "Union"


data Vars =
    Param Nombre Tipo Ref         |
    PuffValue Vars Tipo           | -- Variable deferenciada con puff
    Var Nombre Tipo               |
    VarIndex Vars Expr Tipo       | -- Indice para array, listas
    VarCompIndex Vars Nombre Tipo   -- Campos de los registros y uniones
    deriving (Eq, Ord)

instance Show Vars where
    show (Param n t r)    = "Parametro: " ++ n ++ " de tipo: " ++ show t ++ " pasado por: " ++ show r
    show (PuffValue v t)  = "Desreferenciacion de: " ++ show v ++ " de tipo: " ++ show t
    show (Var n t)        = "Variable: " ++ n ++ " de tipo: " ++ show t
    show (VarIndex v e t) = "Indexacion de: " ++ show v ++ " en la posicion " ++ show e ++ "de tipo: " ++ show t
    --TODO: show (VarCompIndex v n t) = ""

-- Especifica si un parametro es pasado como valor o por referencia
data Ref =
    Referencia |
    Valor
    deriving(Eq, Show, Ord)


data Instr  = 
    Asignacion Vars Expr                          |
    Break                                         |
    Continue                                      |
    For Nombre Expr Expr SecuenciaInstr           |
    ForEach Nombre Expr SecuenciaInstr            |
    ForWhile Nombre Expr Expr Expr SecuenciaInstr |
    Free Nombre                                   |
    Print Expr                                    |
    ProcCall Subrutina                            |
    Programa Sentencias                           |
    Return Expr                                   |
    SecDeclaraciones SecuenciaInstr               |
    Switch [(Expr, SecuenciaInstr)]               |
    While Expr SecuenciaInstr
    deriving (Eq)

instance Show Instr where
-- Esto imprime el tipo de instruccion pero no imprime la secuenciaInstr
    show (Asignacion v e)        = "Asignacion de " ++ show e ++ "a " ++ show v
    show Break                   = "Break"
    show Continue                = "Continue"
    show (For n e1 e2 s)         = "Ciclo For iterando sobre " ++ n ++ " desde: " ++ show e1 ++ " hasta: " ++ show e2 ++ ": " ++ show s
    show (ForEach n e s)         = "Ciclo ForEach iterando sobre " ++ n ++ " sobre los elementos de: " ++ show e ++ ": " ++ show s
    show (ForWhile n e1 e2 e3 s) = "Ciclo For Logico iterando sobre " ++ n ++ "desde: " ++ show e1 ++ " hasta: " ++ show e2 ++ " mientras sea verdad: " ++ show e3 ++ ": " ++ show s
    show (Free n)                = "Liberar memoria reservada por " ++ n
    show (Print e)               = "Imprimir a la salida estandar " ++ show e
    show (ProcCall s)            = "Llamada de: " ++ show s
    show (Programa c)            = "Programa: " ++ show c
    show (Return e)              = "Retornar " ++ show e
    show (SecDeclaraciones s)    = "Declaraciones: Esto no es una instruccion: " ++ show s
    show (Switch ls)             = "Switch/Case: " ++ show ls
    show (While e s)             = "Ciclo While iterando mientras sea verdad: " ++ show e



-- Lo que se puede escribir dentro de un programa
data Sentencia = Def SecuenciaInstr
               | Sec SecuenciaInstr
               | Nada
               deriving (Eq)

instance Show Sentencia where
    show (Def s) = show s
    show (Sec s) = show s 
    show Nada    = ""

getInstr :: Sentencia -> SecuenciaInstr
getInstr (Def s) = s
getInstr (Sec s) = s
getInstr Nada    = []


-- 
data Subrutina = SubrutinaCall Nombre Parametros
                deriving (Eq, Ord)

instance Show Subrutina where
    show (SubrutinaCall n p) = "Subrutina " ++ n ++ " con parametros " ++ show p


-- Definiciones de las subrutinas, registros y uniones
{-data Definicion = Defs
                | Func Nombre Parametros Tipo SecuenciaInstr
                | Proc Nombre Parametros SecuenciaInstr
                | Registro Nombre SecuenciaInstr Tipo
                | Union Nombre SecuenciaInstr Tipo
                deriving (Eq)

instance Show Definicion where
    show Defs             = "Definiciones"
    show (Func n p t s)   = "Funcion: " ++ n ++ " que recibe: " ++ show p ++ " y retorna un: " ++ show t ++ ": " ++ show s
    show (Proc n p s)     = "Procedimiento: " ++ n ++ " que recibe: " ++ show p ++ ": " ++ show s
    show (Registro n s t) = "Registro: " ++ n
    show (Union n s t)    = "Union: " ++ n-}


data Expr   = 
    ArrLstExpr [Expr] Tipo         |
    FuncCall Subrutina Tipo        |
    IfSimple Expr Expr Expr Tipo   |
    Literal Literal Tipo           |
    Null                           |
    OpBinario BinOp Expr Expr Tipo |
    OpUnario UnOp Expr Tipo        |
    Read Expr                      |
    Variables Vars Tipo
    deriving (Eq, Ord)

instance Show Expr where
    show (ArrLstExpr lst _)     = "[" ++ intercalate "," (map show lst) ++ "]"
    show (FuncCall s _)         = "Llamada a funcion: " ++ show s
    show (IfSimple e1 e2 e3 t)  = show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3
    show (Literal lit _)        = show lit
    show Null                   = "Apuntador nada"
    show (OpBinario op e1 e2 _) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
    show (OpUnario op e1 _)     = show op ++ show e1
    show (Read e1)              = "Leyendo de entrada estandar: " ++ show e1
    show (Variables vars _)     = show vars

data Literal =
    ArrLst [Literal] | -- >> Arreglos y listas
    Booleano Bool    |
    Caracter Char    |
    Entero Int       |
    Flotante Float   |
    Str String       |
    ValorVacio
    deriving (Eq, Ord)

instance Show Literal where
    show (ArrLst l@(ArrLst _:_))   = show $ map show l 
    show (ArrLst l@(Booleano _:_)) = show $ map ((\x->read x::Bool) . show) l
    show (ArrLst l@(Caracter _:_)) = show $ map ((\x->read x::Char) . show) l
    show (ArrLst l@(Entero _:_))   = show $ map ((\x->read x::Int) . show) l
    show (Booleano val)            = show val
    show (Caracter val)            = show val
    show (Entero val)              = show val
    show (Flotante val)            = show val
    show (Str val)                 = show val
    show ValorVacio                = "Valor vacio"


-- Operadores binarios
data BinOp =
    And            |
    Anexo          |
    Concatenacion  |
    Desigual       |
    DivEntera      |
    Division       |
    Igual          |
    Mayor          |
    MayorIgual     |
    Menor          |
    MenorIgual     |
    Modulo         |
    Multiplicacion |
    Or             |
    Resta          |
    Suma
    deriving (Eq, Ord)

instance Show BinOp where
    show And            = " && "
    show Anexo          = " : "
    show Concatenacion  = " :: "
    show Desigual       = " != "
    show DivEntera      = " // "
    show Division       = " / "
    show Igual          = " == "
    show Mayor          = " > "
    show MayorIgual     = " >= "
    show Menor          = " < "
    show MenorIgual     = " <= "
    show Modulo         = " % "
    show Multiplicacion = " * "
    show Or             = " || "
    show Resta          = " - "
    show Suma           = " + "


-- Operadores unarios
data UnOp =
    Desreferenciar |
    Longitud       |
    LowerCase      |
    Negativo       |
    New            |
    Not            |
    UpperCase
    deriving (Eq, Ord)

instance Show UnOp where
    show Desreferenciar = "puff "
    show Longitud       = "#"
    show LowerCase      = "."
    show Negativo       = "-"
    show New            = "summon "
    show Not            = "!"
    show UpperCase      = "^"


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--             Tipos de datos que representan la tabla de simbolos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


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
    deriving (Eq, Ord)

instance Show SymbolInfo where
    show (SymbolInfo t s c) =
        "Tipo: " ++ show t ++ ", en el alcance: " ++ show s ++ ", de categoria: " ++ show c ++ "\n\t"


{- Nuevo tipo de dato para representar la tabla de simbolos
* Tabla de hash:
*   Key: Nombre
*   Value: Lista de la informacion pertinente
-}
newtype SymTab  = SymTab { getSymTab :: M.Map Nombre [SymbolInfo] }
                deriving (Eq)

instance Show SymTab where
    show (SymTab map) = "----------\nTabla de simbolos\n----------\n" ++ symbols
        where
            tabla = M.toList map
            symbols = concatMap (\(k,v)-> k ++ " : " ++ concatMap show v) tabla


-- Transformador monadico para crear y manejar la tabla de simbolos junto con 
-- la pila de alcances
type MonadSymTab a = RWST () () (SymTab, StackScopes) IO a


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                      Para mostrar mejor los errores
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

getNameParam :: Expr -> Nombre
getNameParam (OpBinario _ e1 e2 _) =
    getNameParam e1
getNameParam (Variables x _) = showVar x
getNameParam _ = ""

-- getTipoParametro :: Expr -> Tipo
-- getTipoParametro (Variables x _) = getTypeVar x
-- getTipoParametro _ = TError

-- getTypeVar :: Vars -> Tipo
-- getTypeVar (Var _ t) = t
-- getTypeVar (Param _ t _) = t
-- getTypeVar (PuffValue _ t) = t
-- getTypeVar (VarIndex _ _ t) = t

-- Show de las variables
showVar :: Vars -> String
showVar (Var name _) = name
showVar (Param name _ _) = name
showVar (PuffValue vars _) = showVar vars
showVar (VarIndex vars e _) =
    showVar vars ++ "|)" ++ show e ++ "(|" 
    -- TODO : Falta mostrar lista

-- Show de las expresiones
showE :: Expr -> String
showE (Literal lit _)                           = showL lit
showE (Variables vars _)                        = showVar vars
showE (OpBinario Suma e1 e2 _)           = "(" ++ showE e1 ++ " + " ++ showE e2 ++ ")"
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


getRef :: Vars -> Ref
getRef (Param _ _ ref) = ref
getRef _ = error "Esto no es un parametro, que haces aqui?"

isVar :: Expr -> Bool
isVar (Variables _ _) = True
isVar (OpBinario _ x y _) = isVar x || isVar y
isVar (OpUnario _ x _) = isVar x
isVar (IfSimple x y z _) = isVar x || isVar y || isVar z
isVar (ArrLstExpr x _) = any isVar x
isVar _ = False

getVar :: Expr -> [Vars]
getVar (Variables v _) = [v]
getVar (OpUnario _ x _) = getVar x
getVar (OpBinario _ x y _) = concatMap getVar $ filter isVar $ x : [y]
getVar (ArrLstExpr x _) = concatMap getVar $ filter isVar x
getVar _ = error "Esto no tiene variables, que haces aqui?"