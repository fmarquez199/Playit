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
import Data.List (intercalate,elemIndex)


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


data ExtraInfo =
    AST SecuenciaInstr |
    Params Int         |
    FromReg Nombre       -- Registro o union al que pertenece el campo/variable
    deriving (Eq, Ord)

instance Show ExtraInfo where
    show (AST secInstr) = "\t\tAST:\n" ++ concatMap show secInstr ++ "\n"
    show (Params p)     = "\t\tParametros: " ++ show p ++ "\n"

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
    show (TArray e t)   = "Arreglo de tamano " ++ show e ++ " de " ++ show t
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
    show (PuffValue v t)  = show t ++ " puff " ++ show v
    show (Var n t)        = show t ++ " " ++ n
    show (VarIndex v e t) = show t ++ " " ++ show v ++ " index: " ++ show e
    show (VarCompIndex v n t) = show t ++ " " ++ show v ++ " spawn " ++ n

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
    Programa SecuenciaInstr                       |
    Return Expr                                   |
    Asignaciones SecuenciaInstr                   |
    IF [(Expr, SecuenciaInstr)]                   |
    While Expr SecuenciaInstr
    deriving (Eq,Ord)

instance Show Instr where
-- Esto imprime el tipo de instruccion pero no imprime la secuenciaInstr
    show (Asignacion v e)        = "\t" ++ show v ++ " = " ++ show e ++ "\n"
    show Break                   = "\tGameOver\n"
    show Continue                = "\tKeepPlaying\n"
    show (For n e1 e2 s)         = "\tFor " ++ n ++ " = " ++ show e1 ++ " -> "
        ++ show e2 ++ ":\n\t" ++ concatMap show s ++ "\n"
    show (ForEach n e s)         = "\tForEach " ++ n ++ " <- " ++ show e ++
        ":\n\t" ++ concatMap show s ++ "\n"
    show (ForWhile n e1 e2 e3 s) = "\tFor " ++ n ++ " = " ++ show e1 ++ " -> " ++
        show e2 ++ " while: " ++ show e3 ++ ":\n\t" ++ concatMap show s ++ "\n"
    show (Free n)                = "\tfree " ++ n ++ "\n"
    show (Print e)               = "\tdrop " ++ show e ++ "\n"
    show (ProcCall s)            = "\tkill " ++ show s ++ "\n"
    show (Programa c)            = "\nworld:\n" ++ concatMap show c ++ "\n"
    show (Return e)              = "\tunlock " ++ show e ++ "\n"
    show (Asignaciones s)        = concatMap show s ++ "\n"
    show (IF s)                  = "\tIF:\n" ++ concat guardias
        where
            conds = map (show . fst) s
            instrs =  map (concatMap show . snd) s
            guardias = [c ++ " }\n" ++ i | c <- conds, i <- instrs, elemIndex c conds == elemIndex i instrs]
    show (While e s)             = "\tWhile " ++ show e ++ ":\n\t" ++ concatMap show s ++ "\n"


-- 
data Subrutina = SubrutinaCall Nombre Parametros
                deriving (Eq, Ord)

instance Show Subrutina where
    show (SubrutinaCall n p) = n ++ "(" ++ concatMap show p ++ ")"


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
    show (FuncCall s _)         = "kill " ++ show s
    show (IfSimple e1 e2 e3 t)  = show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3
    show (Literal lit _)        = show lit
    show Null                   = "DeathZone"
    show (OpBinario op e1 e2 _) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
    show (OpUnario op e1 _)     = show op ++ show e1
    show (Read e1)              = "joystick " ++ show e1
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

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--             Tipos de datos que representan la tabla de simbolos
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- Alcance de un identificador
type Alcance = Integer

-- Lista con los alcances activos que pueden ser accedidos. Cadena estática
type ActiveScopes = [Alcance]

-- Informacion pertinente a la entrada de la tabla de simbolos
data SymbolInfo = SymbolInfo {
    getType :: Tipo,
    getScope :: Alcance,
    getCategory :: Categoria,
    getExtraInfo :: [ExtraInfo]
    }
    deriving (Eq, Ord)

instance Show SymbolInfo where
    show (SymbolInfo t s c i) = "Tipo: " ++ show t ++ " | Alcance: " ++
        show s ++ " | Categoria: "++ show c ++ ".\n\tExtra:\n" ++
        concatMap show i ++ "\n"


{- Nuevo tipo de dato para representar la tabla de simbolos
* Tabla de hash:
*   Key: Nombre
*   Value: Lista de la informacion pertinente
-}
newtype SymTab  = SymTab { getSymTab :: M.Map Nombre [SymbolInfo] }
                deriving (Eq)

instance Show SymTab where
    show (SymTab hash) = header ++ info ++ symbols
        where
            header = "\n------------\n Tabla de simbolos \n------------\n"
            info = "- Simbolo | Informacion asociada \n------------\n"
            tabla = M.toList hash
            symbols' = map fst $ M.toList $ M.filter (any (>0)) $ M.map (map getScope) hash
            showInfo info = if getScope info > 0 then show info else ""
            showTable (k,v) = 
                if k `elem` symbols' then
                    k ++ " -> " ++ concatMap showInfo v
                else ""
            -- showTable (k,v) = k ++ " -> " ++ concatMap show v
            symbols = concatMap showTable tabla

-- Transformador monadico para crear y manejar la tabla de simbolos junto con 
-- la pila de alcances y cuales estan activos

type MonadSymTab a = RWST String () (SymTab, ActiveScopes, Alcance) IO a

