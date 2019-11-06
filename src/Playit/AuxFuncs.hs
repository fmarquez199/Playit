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
-------------------------------------------------------------------------------
--                           AST creation related
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Gets the field register / union name 
getReg :: [ExtraInfo] -> String
getReg []               = ""
getReg (FromReg name:_) = name
getReg (_:rs)           = getReg rs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Gets the amount of parameters
getNParams :: [ExtraInfo] -> Maybe Int
getNParams []           = Nothing
getNParams (Params p:_) = Just $ length p
getNParams (_:rs)       = getNParams rs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Symbol table related
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Modify the symbol type
modifyType :: SymbolInfo -> Type -> SymbolInfo
modifyType (SymbolInfo _ s c ei) newT = SymbolInfo newT s c ei
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Modify the symbol extra info
modifyExtraInfo :: SymbolInfo -> [ExtraInfo] -> SymbolInfo
modifyExtraInfo (SymbolInfo t s c ei) newEi = SymbolInfo t s c (ei ++ newEi)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Check types related
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the all's assignations types is the same 
eqAssigsTypes :: InstrSeq -> Type -> Bool
eqAssigsTypes assigs t = all (\(Assig _ expr) -> typeE expr == t) assigs  
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isSimpleType:: Type -> Bool
isSimpleType t
    | t `elem` [TBool,TChar,TInt,TFloat,TStr] = True
    | otherwise = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isArray :: Type -> Bool
isArray (TArray _ _) = True
isArray _            = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isList :: Type -> Bool
isList (TList _) = True
isList _         = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isPointer :: Type -> Bool
isPointer (TPointer _) = True
isPointer _            = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isRegUnion :: Type -> Bool
isRegUnion (TNew _) = True
isRegUnion _        = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Determina si la variable a cambiar su TDummy es la de iteracion
--isVarIter :: Var -> SymTab -> Scope -> Bool
--isVarIter (Var name _) symTab scope
--    | (getScope $ fromJust info) < scope = False
--    | otherwise = True
    
--    where info = lookupInSymTab name symTab
--isVarIter (Index var _ _) symTab scope = isVarIter var symTab scope

--isVarIter :: Var -> SymTab -> Scope -> Bool
isVarIter (Var name _) symTab scope  = False
--    | otherwise = True
    
--    where info = lookupInSymTab name symTab
--isVarIter (Index var _ _) symTab scope = isVarIter var symTab scope
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
--- | Gets the base variable's type
baseTypeVar :: Var -> Type
baseTypeVar (Var _ t)     = baseTypeT t
baseTypeVar (Index _ _ t) = baseTypeT t
baseTypeVar (Param _ t _) = baseTypeT t
baseTypeVar (Field v _ t) = baseTypeT t  -- baseTypeVar v
-- baseTypeVar (Desref v t)  = baseTypeT t
baseTypeVar (Desref v _)  = baseTypeVar v
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
--- | Gets the base expression's type
baseTypeE :: Expr -> Type
baseTypeE (Variable _ t)     = baseTypeT t
baseTypeE (Literal _ t)      = baseTypeT t
baseTypeE (Binary _ _ _ t)   = baseTypeT t
baseTypeE (Unary _ _ t)      = baseTypeT t
baseTypeE (ArrayList _ t)    = baseTypeT t
baseTypeE (Read _)           = TStr
baseTypeE (IfSimple _ _ _ t) = baseTypeT t
baseTypeE (FuncCall _ t)     = baseTypeT t
baseTypeE (IdType t)         = baseTypeT t
-- baseTypeE (Null t)           = baseTypeT t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
--- | Gets the base type's type
baseTypeT :: Type -> Type
baseTypeT (TList t)    = baseTypeT t
baseTypeT (TArray _ t) = baseTypeT t
baseTypeT (TPointer t) = baseTypeT t
baseTypeT t            = t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Gets the variable type
typeVar :: Var -> Type
typeVar (Var _ t)     = t
typeVar (Index _ _ t) = t
typeVar (Param _ t _) = t
typeVar (Field _ _ t) = t
typeVar (Desref _ t)  = t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Gets the expression type
typeE :: Expr -> Type
typeE (Variable _ t)     = t
typeE (Literal _ t)      = t
typeE (Binary _ _ _ t)   = t
typeE (Unary _ _ t)      = t
typeE (ArrayList _ t)    = t
typeE (Read _)           = TStr
typeE (IfSimple _ _ _ t) = t
typeE (FuncCall _ t)     = t
typeE (IdType t)         = t
-- typeE (Null t)           = t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Determina el tipo base de los elementos del arreglo
-- typeArrLst :: Type -> Type
-- typeArrLst (TArray _ t@(TArray _ _)) = typeArrLst t
-- typeArrLst (TArray _ t)              = t
-- typeArrLst (TList t@(TList _))       = typeArrLst t
-- typeArrLst (TList t)                 = t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Dada una lista [(List t)] regresa el t(si t es una lista recursiona), si no es de esa forma regresa Nothing
-- Util para <<>> == <<>> y <<>>::<<>> y derivados
getTLists:: [Type]-> Maybe Type
getTLists ts 
    | all isList ts = Just (\l -> TList l) <*> getTLists (map (\(TList t) -> t) ts) -- [[[int]]] = recursivo [[int]]
    | all (\t -> isSimpleType t || t ==TDummy) ts =  -- [[int]] = Just [int]
        if any isSimpleType ts then  
            Just (head (filter (/=TDummy) ts))
        else 
            Just TDummy
    | otherwise = Nothing
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Dado un tipo y una lista (List t) regresa el t(si t es una lista recursiona) , si no es de esa forma regresa Nothing
-- Util Para el problema de <<2>>:<< <<>> >> 
getTListAnexo:: Type -> Type -> Maybe Type
getTListAnexo t1 (TList t2) 
    | isSimpleType t1 && isSimpleType t2 && t1 == t2 = Just (TList t1)-- int : [int] = Just int
    | isSimpleType t1 && t2 == TDummy  = Just (TList t1)-- int : [TDummy] = Just int
    | t1 == TDummy && isSimpleType t2  = Just (TList t2)-- TDummy : [int] = Just int
    | t1 == TDummy && t2 == TDummy  = Just (TList TDummy)-- TDummy : [TDummy] = Just int
    | isList t1 && isList t2 = Just (\l -> TList l) <*> getTListAnexo (baseTypeT t1) t2 -- [t] :[[t]] = recursivo t [t]
    | otherwise = Nothing
getTListAnexo _ _ = Nothing
-------------------------------------------------------------------------------
