{-
Modulo para la creacion y manejo de la tabla de simbolos

* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.SymbolTable where

import Control.Monad.Trans.RWS
import Control.Monad (void,forM,when)
import qualified Data.Map as M
-- import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust, isNothing)
import Playit.Types


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Creacion y manejo de la tabla de simbolos
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Estado inicial con todo lo predefinido del lenguaje
initState :: (SymTab,ActiveScopes,Alcance)
initState = createInitSymTab (SymTab M.empty)


createInitSymTab :: SymTab -> (SymTab,ActiveScopes,Alcance)
createInitSymTab st = (insertSymbols symbols info st,[0],0)
    where

-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Se apila el nuevo alcance
pushNewScope :: MonadSymTab ()
pushNewScope = do

-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Se desapila el alcance actual
popScope :: MonadSymTab ()
popScope = do

-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Agrega a la tabla de simbolos la lista de identificadores con su informacion:
--  Tipo, alcance, categoria
addToSymTab :: [Nombre] -> [SymbolInfo] -> SymTab -> ActiveScopes -> Alcance
            -> MonadSymTab ()
addToSymTab ids info actualSymTab activeScopes scope = 
    put (insertSymbols ids info actualSymTab, activeScopes, scope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Inserta los identificadores con su tipo en la tabla de simbolos dada
insertSymbols :: [Nombre] -> [SymbolInfo] -> SymTab -> SymTab
insertSymbols [] _ symTab = symTab
insertSymbols (id:ids) (info:infos) (SymTab table)

    where
        -- Tabla de simbolos con el identificador insertado

-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupInSymTab :: Nombre -> SymTab -> Maybe [SymbolInfo]
lookupInSymTab var (SymTab table) = M.lookup var table
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
