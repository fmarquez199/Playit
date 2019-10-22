{-
Modulo para la creacion y manejo de la tabla de simbolos

* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.SymbolTable where

import Control.Monad.Trans.RWS
import Control.Monad (void,forM)
import qualified Data.Map as M
import Playit.Lexer
-- import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust, isNothing)
import Playit.Types
--import Playit.ErrorM


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
        -- TODO: terminar de agregar todos los simbolos del lenguaje
        symbols = t ++ words
        t = ["Power", "Skill", "Rune", "Runes", "Battle", "Inventory", "Items"]
        words = ["Win", "Lose", "free", "puff"]
        info = ti ++ wi
        ti = [pInfo, sInfo, rInfo, rsInfo, bInfo, inventoryInfo, itemsInfo]
        wi = [boolsInfo, boolsInfo, aptInfo, aptInfo]
        pInfo = SymbolInfo TInt 0 Tipos [Nada]
        sInfo = SymbolInfo TFloat 0 Tipos [Nada]
        rInfo = SymbolInfo TChar 0 Tipos [Nada]
        rsInfo = SymbolInfo TStr 0 Tipos [Nada]
        bInfo = SymbolInfo TBool 0 Tipos [Nada]
        inventoryInfo = SymbolInfo TRegistro 0 ConstructoresTipos [Nada]
        itemsInfo = SymbolInfo TUnion 0 ConstructoresTipos [Nada]
        boolsInfo = SymbolInfo TBool 0 Variable [Nada]
        aptInfo = SymbolInfo (TApuntador TDummy) 0 Apuntadores [Nada]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Se empila el nuevo alcance
pushNewScope :: MonadSymTab ()
pushNewScope = do
    (actualSymTab, activeScopes, scope) <- get
    let newScope = scope + 1
    put (actualSymTab, newScope:activeScopes, newScope)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Se desempila el alcance actual
popScope :: MonadSymTab ()
popScope = do
    (actualSymTab, _:prevScopes, scope) <- get
    put (actualSymTab, prevScopes, scope)
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
    | M.member id table = insertSymbols ids infos updSymTab
    | otherwise = insertSymbols ids infos newSymTab
    
    where
        -- Tabla de simbolos con el identificador insertado
        newSymTab = SymTab $ M.insert id [info] table
        updSymTab = SymTab $ M.adjust (info:) id table
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Añade las variables a la tabla de simbolos
insertDeclarations :: [(Nombre,Posicion)] -> Tipo -> SecuenciaInstr -> MonadSymTab SecuenciaInstr
insertDeclarations ids t asigs = do
    (actualSymTab, activeScopes@(activeScope:_), scope) <- get
        
    ids' <- forM ids $ \(id,(f,c)) -> do
        _ <- if isJust $ lookupScopesNameInSymTab [activeScope] id actualSymTab
            then error $ "Error: redeclaración de \'" ++ id ++ "\' " ++ " en la fila: " ++ (show f) ++ ", columna: " ++ (show c)
            else return ()
        return id
    
    let info = replicate (length ids) (SymbolInfo t activeScope Variable [Nada])
    addToSymTab ids' info actualSymTab activeScopes scope
    return asigs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupInSymTab :: Nombre -> SymTab -> Maybe [SymbolInfo]
lookupInSymTab var (SymTab table) = M.lookup var table
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Busca los identificadores de una variable en la tabla de simbolos dada.
lookupInSymTab' :: [Nombre] -> SymTab -> [Maybe [SymbolInfo]]
lookupInSymTab' [] _ = [Nothing]
lookupInSymTab' [x] symtab = [lookupInSymTab x symtab]
lookupInSymTab' (x:xs) symtab = lookupInSymTab x symtab:lookupInSymTab' xs symtab
-------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupScopesInSymInfos :: [Alcance]-> Maybe [SymbolInfo] ->  Maybe SymbolInfo
lookupScopesInSymInfos scopes Nothing = Nothing
lookupScopesInSymInfos scopes (Just r) 
    | null lstAlcances  = Nothing
    | otherwise = Just $ fst $ head lstAlcances
    where
        lstAlcances = [(s,a) | s <- r, a <- scopes,getScope s == a]
        
--------------------------------------------------------------------------------
-- Busca el identificador de una variable en la tabla de simbolos dada.
lookupScopesNameInSymTab :: [Alcance] -> Nombre -> SymTab-> Maybe SymbolInfo
lookupScopesNameInSymTab scopes nombre symtab  = lookupScopesInSymInfos scopes (lookupInSymTab nombre symtab)
--------------------------------------------------------------------------------


modifiExtraInfoSymbol (SymbolInfo t s c [Nada]) extraInfo = SymbolInfo t s c extraInfo
modifiExtraInfoSymbol (SymbolInfo t s c ei) extraInfo = SymbolInfo t s c (extraInfo ++ ei)


-------------------------------------------------------------------------------
-- Actualiza la informacion extra del simbolo con la nueva
-- Encuentra al simbolo al que se le quiere modificar la información extra dado su nombre y alcance
-- Modifica su entrada para informacón extra
-- NOTA: Es primeramente para agregar como informacion extra en las subrutinas
--      su AST y parametros, puede necesitar modificarse si se quiere usar para
--      algo diferente
updateExtraInfo :: Nombre -> Alcance -> [ExtraInfo] -> MonadSymTab ()
updateExtraInfo name scope extraInfo = do

    (symTab@(SymTab table), scopes, lastScope) <- get
    
    -- Obtenemos todos los simbolos asociados al nombre    
    let infos = lookupInSymTab name symTab
    
    if isJust infos then do
        let isTargetSymbol sym = getScope sym == scope
            updateExtraInfo' = fmap (\sym -> if isTargetSymbol sym then modifiExtraInfoSymbol sym extraInfo else sym)
        put(SymTab $ M.adjust updateExtraInfo' name table, scopes, lastScope)

    else return ()
-------------------------------------------------------------------------------

