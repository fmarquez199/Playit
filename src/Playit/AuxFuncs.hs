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
-- | Gets the lists of parameters
getParams :: [ExtraInfo] -> Maybe [(Type, Id)]
getParams []           = Nothing
getParams (Params p:_) = Just p
getParams (_:rs)       = getParams rs
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
modifyExtraInfo (SymbolInfo t s c []) ei      =  SymbolInfo t s c ei
modifyExtraInfo (SymbolInfo t s c ei) (ex:r)  = 
    SymbolInfo t s c (map (\einf -> if areSameTExtInf einf ex then ex else einf ) ei)
    
areSameTExtInf :: ExtraInfo -> ExtraInfo -> Bool
areSameTExtInf (Params _ ) (Params _)   = True
areSameTExtInf (FromReg _ ) (FromReg _) = True
areSameTExtInf (AST _ ) (AST _)         = True
areSameTExtInf _ _                      = False
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Check types related
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Checks the all's assignations types is the same
-- TODO: modificar para que se pueda hacer:
--      Kit of Power l = << >>, l2
--      Power puff x = DeathZone
--      RegUnion r = {3,*r*}
eqAssigsTypes :: InstrSeq -> Type -> Bool
eqAssigsTypes assigs t = all (\(Assig _ expr _) -> typeE expr == t) assigs
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
isFunctionCall :: Expr -> Bool
isFunctionCall (FuncCall _ _) = True
isFunctionCall _              = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isVoid :: Instr -> Bool
isVoid = (== TVoid) . getTypeInstr
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isSubType :: Type -> Type -> Bool
isSubType t1 (TArray _ t2) = t1 == t2
isSubType t1 (TList t2)    = t1 == t2
isSubType t1 _             = False
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
getName :: Var -> Id
getName (Desref v _)  = getName v
getName (Field v _ _) = getName v
getName (Index v _ _) = getName v
getName (Param n _ _) = n
getName (Var n _)     = n
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
--- | Gets the base variable's type
baseTypeVar :: Var -> Type
baseTypeVar (Var _ t)     = baseTypeT t
baseTypeVar (Index _ _ t) = baseTypeT t
baseTypeVar (Param _ t _) = baseTypeT t
baseTypeVar (Field _ _ t) = baseTypeT t
-- baseTypeVar (Desref v t)  = baseTypeT t
baseTypeVar (Desref v _)  = baseTypeVar v
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Gets the expression type
typeE :: Expr -> Type
typeE (Variable _ t)     = t
typeE (Literal _ t)      = t
typeE (Binary _ _ _ t)   = t
typeE (Unary _ _ t)      = t
typeE (ArrayList _ t)    = t
typeE (Read _ t)         = t
typeE (IfSimple _ _ _ t) = t
typeE (FuncCall _ t)     = t
typeE (IdType t)         = t
typeE Null               = TNull
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
--- | Gets the base expression's type
baseTypeE :: Expr -> Type
baseTypeE (Variable _ t)     = baseTypeT t
baseTypeE (Literal _ t)      = baseTypeT t
baseTypeE (Binary _ _ _ t)   = baseTypeT t
baseTypeE (Unary _ _ t)      = baseTypeT t
baseTypeE (ArrayList _ t)    = baseTypeT t
baseTypeE (Read _ t)         = baseTypeT t
baseTypeE (IfSimple _ _ _ t) = baseTypeT t
baseTypeE (FuncCall _ t)     = baseTypeT t
baseTypeE (IdType t)         = baseTypeT t
baseTypeE Null               = TNull
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
-- Determina el tipo base de los elementos del arreglo
typeArrLst :: Type -> Type
typeArrLst (TArray _ t@(TArray _ _)) = typeArrLst t
typeArrLst (TArray _ t)              = t
typeArrLst (TList t@(TList _))       = typeArrLst t
typeArrLst (TList t)                 = t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
baseTypeArrLst :: Type -> Type
baseTypeArrLst (TArray _ t)              = t
baseTypeArrLst (TList t)                 = t
-- en cualquiero otro caso, error, eso no deberia pasar
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Dada una lista de tipos lista, [(List t)], determina si todos los tipos 't'
-- concuerdan y retorna ese tipo 't'. Recursiona de ser necesario
-- Util para << >> == << >>, << >>::<< >> y derivados
getTLists:: [Type]-> Maybe Type
getTLists ts 
    | all isList ts = Just (\l -> TList l) <*> getTLists (map (\(TList t) -> t) ts) -- [[[int]]] = recursivo [[int]]
    | all (not . isList) ts =  -- [[int]] = Just [int]

        if any (/= TDummy) ts then
            let 
                listWithNoTDummy = filter (/=TDummy) ts
                listWithNoTNull = filter (/=TNull) listWithNoTDummy
            in
                if null listWithNoTNull then  -- Si la lista tiene todos TNull
                    Just TNull 
                else
                    let 
                        tFirst = head listWithNoTNull
                        isTypeTFirst t = t == tFirst || (t == TNull && isPointer tFirst )
                    in
                        if all isTypeTFirst listWithNoTDummy then Just tFirst
                        else Nothing
        else 
            Just TDummy
            
    | otherwise = Nothing
-------------------------------------------------------------------------------
-- << <<>>, <<DeathZone>> >>

-------------------------------------------------------------------------------
-- Dado un tipo y una lista (List t) regresa el t(si t es una lista recursiona) , si no es de esa forma regresa Nothing
-- Util Para el problema de <<2>>:<< <<>> >> 
getTListAnexo:: Type -> Type -> Maybe Type
getTListAnexo t1 (TList t2) 
    | isSimpleType t1 && isSimpleType t2 && t1 == t2 = Just (TList t1) -- int : [int] = Just int
    | isSimpleType t1 && t2 == TDummy  = Just (TList t1) -- int : [TDummy] = Just int
    | t1 == TDummy && isSimpleType t2  = Just (TList t2) -- TDummy : [int] = Just int
    | t1 == TDummy && t2 == TDummy  = Just (TList TDummy) -- TDummy : [TDummy] = Just int
    | isList t1 && isList t2 = Just (\l -> TList l) <*> getTListAnexo (typeArrLst t1) t2 -- [t] :[[t]] = recursivo t [t]
    | otherwise = Nothing
getTListAnexo _ _ = Nothing
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
getTypeInstr :: Instr -> Type
getTypeInstr (Assig _ _ t)          = t
getTypeInstr (Assigs _ t)           = t
getTypeInstr (Break t)              = t
getTypeInstr (Continue t)           = t
getTypeInstr (For _ _ _ _ t)        = t
getTypeInstr (ForEach _ _ _ t)      = t
getTypeInstr (ForWhile _ _ _ _ _ t) = t
getTypeInstr (Free _ t)             = t
getTypeInstr (IF _ t)               = t
getTypeInstr (Print _ t)            = t
getTypeInstr (ProcCall _ t)         = t
getTypeInstr (Program _ t)          = t
getTypeInstr (Return _ t)           = t
getTypeInstr (While _ _ t)          = t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
getPromiseSubroutine:: Id -> Promises -> Maybe Promise
getPromiseSubroutine _ []                                = Nothing
getPromiseSubroutine name (promise@(Promise id _ _ _):r) = 
    if  name == id then Just promise else getPromiseSubroutine name r
-------------------------------------------------------------------------------
