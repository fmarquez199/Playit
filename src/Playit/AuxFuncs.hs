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


-- Check types realed


--------------------------------------------------------------------------------
-- Verifica el el tipo de todas las asignaciones sea el mismo
eqTypesAsigs :: InstrSeq -> Type -> Bool
eqTypesAsigs asigs t = all (\(Assig _ expr) -> typeE expr == t) asigs  
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Obtiene el tipo asociado a la expresion 
typeE :: Expr -> Type
typeE (Variable _ t)           = t
typeE (Literal _ t)             = t
typeE (Binary _ _ _ t)       = t
typeE (Unary _ _ t)          = t
typeE (ArrayList _ t)           = t
typeE (Read _)                  = TStr
typeE (IfSimple _ _ _ t)        = t
typeE (Call _  _ t)    = t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isArray :: Type -> Bool
isArray (TArray _ _) = True
isArray _ = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isList :: Type -> Bool
isList (TList _) = True
isList _ = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isPointer :: Type -> Bool
isPointer (TApuntador _) = True
isPointer _ = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isRegUnionType :: Type -> Bool
isRegUnionType (TNuevo _) = True
isRegUnionType _ = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isTEscalar:: Type -> Bool
isTEscalar t
    | t `elem` [TBool,TChar,TInt,TFloat,TStr] = True
    | otherwise = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Determina el tipo base de los elementos del arreglo
typeArrLst (TArray _ t@(TArray _ _)) = typeArrLst t
typeArrLst (TArray _ t)              = t
typeArrLst (TList t@(TList _))     = typeArrLst t
typeArrLst (TList t)                = t
-------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Obtiene el tipo asociado a una variable
typeVar :: Var -> Type
typeVar (Var _ t)            = t
typeVar (Index _ _ t)     = t
typeVar (Param _ t _)        = t
typeVar (Field _ _ t) = t
typeVar (Desref v t)      = t
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--- Determine the root type
typeVar' :: Var -> Type
typeVar' (Var _ t)            = typeTipo t
typeVar' (Index _ _ t)     = typeTipo t
typeVar' (Param _ t _)        = typeTipo t
typeVar' (Field v _ t) = typeTipo t
typeVar' (Desref v _)        = typeVar' v

typeTipo :: Type -> Type
typeTipo (TList t)     = typeTipo t
typeTipo (TArray _ t)   = typeTipo t
typeTipo (TPointer t) = typeTipo t
typeTipo TBool          = TBool
typeTipo TChar          = TChar
typeTipo TDummy         = TDummy
typeTipo TError         = TError
typeTipo TFloat         = TFloat
typeTipo TInt           = TInt
typeTipo TRegister      = TRegister
typeTipo TStr           = TStr
typeTipo TUnion         = TUnion
typeTipo t              = t
--------------------------------------------------------------------------------



-- Dada una lista [(List t)] regresa el t(si t es una lista recursiona) , si no es de esa forma regresa Nothing
-- Util para <<>> == <<>> y <<>>::<<>> y derivados
getTLists:: [Type]-> Maybe Type
getTLists ts 
    | all isList ts = Just (\l -> TList l) <*> getTLists (map (\(TList t) -> t) ts) -- [[[int]]] = recursivo [[int]]
    | all (\t -> isTEscalar t || t ==TDummy) ts =  -- [[int]] = Just [int]
        if any isTEscalar ts then  
            Just (head (filter (/=TDummy) ts))
        else 
            Just TDummy
    | otherwise = Nothing


-- Dado un tipo y una lista (List t) regresa el t(si t es una lista recursiona) , si no es de esa forma regresa Nothing
-- Util Para el problema de <<2>>:<< <<>> >> 
getTListAnexo:: Type -> Type -> Maybe Type
getTListAnexo t1 (TList t2) 
    | isTEscalar t1 && isTEscalar t2 && t1 == t2 = Just (TList t1)-- int : [int] = Just int
    | isTEscalar t1 && t2 == TDummy  = Just (TList t1)-- int : [TDummy] = Just int
    | t1 == TDummy && isTEscalar t2  = Just (TList t2)-- TDummy : [int] = Just int
    | t1 == TDummy && t2 == TDummy  = Just (TList TDummy)-- TDummy : [TDummy] = Just int
    | isList t1 && isList t2 = Just (\l -> TList l) <*> getTListAnexo (typeArrLst t1) t2 -- [t] :[[t]] = recursivo t [t]
    | otherwise = Nothing
getTListAnexo _ _ = Nothing

