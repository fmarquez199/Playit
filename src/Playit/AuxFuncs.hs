{- |
 * Auxiliary functions for other modules
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.AuxFuncs where

import Playit.Types


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


-- ST related

-------------------------------------------------------------------------------
-- Modify the symbol type
modifyType :: SymbolInfo -> Type -> SymbolInfo
modifyType (SymbolInfo _ s c ei) newT = SymbolInfo newT s c ei
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Actualiza la informacion extra del simbolo con la nueva
modifyExtraInfo :: SymbolInfo -> [ExtraInfo] -> SymbolInfo
modifyExtraInfo (SymbolInfo t s c ei) extraInfo = SymbolInfo t s c (ei ++ extraInfo)
-------------------------------------------------------------------------------
