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
    show Apuntadores        = "Apuntadores"
    show Campos             = "Campos"
    show Constantes         = "Constantes"
    show ConstructoresTipos = "Constructores de Tipos"
    show Funciones          = "Funciones"
    show (Parametros r)     = "Parametros por " ++ show r
    show Procedimientos     = "Procedimientos"
    show Tipos              = "Tipos"
    show Variable           = "Variables"


data ExtraInfo =
    AST SecuenciaInstr    |
    Params [Nombre{-,Tipo-}] | -- Para verif tipos [(Nombre,Tipo)]
    FromReg Nombre       -- Registro o union al que pertenece el campo/variable
    deriving (Eq, Ord)

instance Show ExtraInfo where
    show (AST s)     = "    AST:\n      " ++ intercalate "\t  " (map show s)
    show (Params p)  = "    Parametros: " ++ show p ++ "\n"
    show (FromReg n) = "    Campo del registro: " ++ show n

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
    TNuevo String    |
    TRegistro        |
    TStr             |
    TUnion           |
    TVoid              -- Tipo de los procedimientos
    deriving(Eq, Ord)


instance Show Tipo where
    show (TApuntador t) = "Apt(" ++ show t ++ ")"
    show (TArray e t)   = show t ++ "|}" ++ show e ++ "{| "
    show TBool          = "Battle"
    show TChar          = "Rune"
    show TDummy         = "Sin tipo aun"
    show TError         = "Mal tipado"   
    show TFloat         = "Skill"
    show TInt           = "Power"
    show (TLista t)     = "Kit of(" ++ show t ++ ")"
    show (TNuevo str)   = str
    show TRegistro      = "Inventory"
    show TStr           = "Runes"
    show TUnion         = "Items"
    show TVoid          = "Void"


data Vars =
    Param Nombre Tipo Ref         |
    PuffValue Vars Tipo           | -- Variable desreferenciada con puff
    Var Nombre Tipo               |
    VarIndex Vars Expr Tipo       | -- Indice para array, listas
    VarCompIndex Vars Nombre Tipo   -- Campos de los registros y uniones
    deriving (Eq, Ord)

instance Show Vars where
    show (Param n t Valor)    = "Parametro: " ++ {-"("++show t++")"++-}n
    show (Param n t _)        = "Parametro: ?" ++ {-"("++show t ++")?"++-}n
    show (PuffValue v t)      = {-"("++show t++")"++-}"puff (" ++ show v ++ ")"
    show (Var n t)            = {-"("++show t++")"++-}n
    show (VarIndex v e t)     = {-"("++show t++")"++-}show v ++ " index: " ++ show e
    show (VarCompIndex v n t) = {-"("++show t++") ("++-}show v ++ " spawn " ++ n

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
    show (Asignacion v e)        = "  " ++ show v ++ " = " ++ show e ++ "\n"
    show Break                   = "  GameOver\n"
    show Continue                = "  KeepPlaying\n"
    show (For n e1 e2 s)         = "  For " ++ n ++ " = " ++ show e1 ++ " -> "
        ++ show e2 ++ ":\n    " ++ intercalate "    " (map show s) ++ "\n"

    show (ForEach n e s)         = "  ForEach " ++ n ++ " <- " ++ show e ++
        ":\n    " ++ intercalate "    " (map show s) ++ "\n"

    show (ForWhile n e1 e2 e3 s) = "  For " ++ n ++ " = " ++ show e1 ++ " -> " ++
        show e2 ++ " while: " ++ show e3 ++ ":\n    " ++ 
        intercalate "    " (map show s) ++ "\n"

    show (Free n)                = "  free " ++ n ++ "\n"
    show (Print e)               = "  drop " ++ show e
    show (ProcCall s)            = "  kill " ++ show s ++ "\n"
    show (Programa s)            = "\nworld:\n" ++ concatMap show s ++ "\n"
    show (Return e)              = "  unlock " ++ show e
    show (Asignaciones s)        = intercalate "  " (map show s)
    show (IF s)                  = "  IF:\n    " ++ concat guardias
        where
            conds = map (show . fst) s
            instrs =  map (concatMap show . snd) s
            guardias = [c ++ " }\n    " ++ i | c <- conds, i <- instrs, elemIndex c conds == elemIndex i instrs]

    show (While e s)             = "  While " ++ show e ++ ":\n    " ++
        intercalate "    " (map show s) ++ "\n"


-- 
data Subrutina = SubrutinaCall Nombre Parametros
                deriving (Eq, Ord)

instance Show Subrutina where
    show (SubrutinaCall n p) = n ++ "(" ++ intercalate "," (map show p) ++ ")"


data Expr   = 
    ArrLstExpr [Expr] Tipo         |
    FuncCall Subrutina Tipo        |
    IdTipo Tipo                    |
    IfSimple Expr Expr Expr Tipo   |
    Literal Literal Tipo           |
    Null                           | -- tipo: compatible con apt de lo que sea o que el contexto lo diga
    OpBinario BinOp Expr Expr Tipo |
    OpUnario UnOp Expr Tipo        |
    Read Expr                      |
    Variables Vars Tipo
    deriving (Eq, Ord)

instance Show Expr where
    show (ArrLstExpr lst _)     = "[" ++ intercalate "," (map show lst) ++ "]"
    show (FuncCall s _)         = "kill " ++ show s
    show (IdTipo t)             = show t
    show (IfSimple e1 e2 e3 _)  = show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3
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
    show (ArrLst l@(Flotante _:_)) = show $ map ((\x->read x::Float) . show) l
    show (ArrLst l)                = concatMap show l
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
    show Desreferenciar = "puff " -- no es un operador??
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
    show (SymbolInfo t s c i) = "\n  Tipo: " ++ show t ++ " | Alcance: " ++
        show s ++ " | Categoria: "++ show c ++
        if not (null i) then
            "\n    Extra:\n  " ++ intercalate "  " (map show i) ++ "\n"
        else ""


{- Nuevo tipo de dato para representar la tabla de simbolos
* Tabla de hash:
*   Key: Nombre
*   Value: Lista de la informacion pertinente
-}
newtype SymTab  = SymTab { getSymTab :: M.Map Nombre [SymbolInfo] }
                deriving (Eq)

instance Show SymTab where
    show (SymTab st) = header ++ info ++ symbols
        where
            header = "\n------------\n Tabla de simbolos \n------------\n"
            info = "- Simbolo | Informacion asociada \n------------\n"
            tabla = M.toList st
            stWithScopes = M.map (map getScope) st
            symbols' = map fst $ M.toList $ M.filter (any (>0)) stWithScopes
            showInfo i = if getScope i > 0 then show i else ""
            showTable (k,v) = 
                if k `elem` symbols' then
                    k ++ " -> " ++ concatMap showInfo (reverse v) ++ "\n"
                else ""
            symbols = concatMap showTable tabla

type SymTabState = (SymTab, ActiveScopes, Alcance)

type FileCodeReader = (String,String)

-- Transformador monadico para crear y manejar la tabla de simbolos junto con 
-- la pila de alcances y cuales estan activos
type MonadSymTab a = RWST FileCodeReader [String] SymTabState IO a


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Funciones auxiliares
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Determina si el simbolo es de un registro o union
getRegName :: [ExtraInfo] -> String
getRegName [] = ""
getRegName (FromReg rname:_) = rname
getRegName (_:rs) = getRegName rs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Obtiene la cantidad de parametros
getNParams :: [ExtraInfo] -> Maybe Int
getNParams [] = Nothing
getNParams (Params p:_) = Just $ length p
getNParams (_:rs) = getNParams rs
-------------------------------------------------------------------------------