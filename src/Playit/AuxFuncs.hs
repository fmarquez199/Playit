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