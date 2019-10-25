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

-- type Definiciones = [Definicion]

type Sentencias = [Sentencia]


-- Categorias a las que pertenecen los simbolos de la tabla de simbolos




-- Especifica si un parametro es pasado como valor o por referencia


-- 



-- Definiciones de las subrutinas, registros y uniones
{-data Definicion = Defs
                | Func Nombre Parametros Tipo SecuenciaInstr
                | Proc Nombre Parametros SecuenciaInstr
                | Registro Nombre SecuenciaInstr Tipo
                | Union Nombre SecuenciaInstr Tipo



-- Operadores binarios



-- Operadores unarios



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
            info = "- Symbolo | Informacion asociada \n------------\n"
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

