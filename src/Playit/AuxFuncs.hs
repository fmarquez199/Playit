{- |
 * Auxiliary functions for other modules
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.AuxFuncs where

import Data.Maybe
import Playit.Types
import qualified Playit.TACType as T


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
modifyType symInfo newT = symInfo{symType = newT}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Modify the symbol category
modifyCategory :: SymbolInfo -> Category -> SymbolInfo
modifyCategory symInfo newC = symInfo{category = newC}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Modify the symbol extra info
-- TODO: Fix correctly
modifyExtraInfo :: SymbolInfo -> [ExtraInfo] -> SymbolInfo
modifyExtraInfo (SymbolInfo n t s c []) ei      =  SymbolInfo n t s c ei
modifyExtraInfo info@(SymbolInfo n t s c ei) newI = newInfo
  where
    newInfo = SymbolInfo n t s c (newI ++ ei)


modifyExtraInfoProm :: SymbolInfo -> [ExtraInfo] -> SymbolInfo
-- modifyExtraInfoProm (SymbolInfo n t s c []) []      =  SymbolInfo n t s c []
modifyExtraInfoProm (SymbolInfo n t s c []) ei      =  SymbolInfo n t s c ei
-- modifyExtraInfoProm info [] = info
modifyExtraInfoProm info@(SymbolInfo n t s c ei) (ex:r) = newInfo
  where
    modInfo i = if sameEI i ex then ex else i
    newInfo   = SymbolInfo n t s c (map modInfo ei)

-- modifyExtraInfo symInfo@(SymbolInfo _ _ _ _ []) ei      = symInfo{extraInfo = ei}
-- modifyExtraInfo symInfo@SymbolInfo{extraInfo = ei} (ex:r) = 
--     symInfo{extraInfo = map (\einf -> if sameEI einf ex then ex else einf) ei}
    
sameEI :: ExtraInfo -> ExtraInfo -> Bool
sameEI (Params _) (Params _)   = True
sameEI (FromReg _) (FromReg _) = True
sameEI (AST _) (AST _)         = True
sameEI _ _                     = False-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Check types related
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
{- | Checks the all's assignations types is the same
  TODO: modificar para que se pueda hacer:
    Kit of Power l = << >>, l2
    Power puff x = DeathZone
    Reg a, Reg b
    Union r = {a}
    r = {b} -> error
    RegUnion r = {3,*r*}
-}
eqAssigsTypes :: InstrSeq -> Type -> Bool
eqAssigsTypes assigs t = all (\(Assig _ expr _) -> compatibles expr) assigs
  where
    compatibles e = (typeE e == t) || isJust (getTLists [typeE e, t])
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isSimpleType:: Type -> Bool
isSimpleType t
    | t `elem` [TBool,TChar,TInt,TFloat,TStr] = True
    | otherwise = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isArray :: Type -> Bool
isArray TArray{} = True
isArray _        = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isList :: Type -> Bool
isList TList{} = True
isList _       = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isPointer :: Type -> Bool
isPointer TPointer{} = True
isPointer _          = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isRegUnion :: Type -> Bool
isRegUnion TNew{} = True
isRegUnion _      = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isFunctionCall :: Expr -> Bool
isFunctionCall FuncCall{} = True
isFunctionCall _          = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isVoid :: Instr -> Bool
isVoid = (== TVoid) . getTypeInstr
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isSubType :: Type -> Type -> Bool
isSubType t1 (TArray _ t2) = t1 == t2
isSubType t1 (TList t2)    = t1 == t2
isSubType _ _              = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isTypeNumber :: Type -> Bool
isTypeNumber t = (t == TInt) || (t == TFloat)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- TODO: estan todos aquí no? ._.
isTypeComparableEq :: Type -> Bool
isTypeComparableEq t = isTypeNumber t || isList t || isPointer t || (t == TBool)
    || (t == TChar) || (t == TStr) || isRegUnion t || isArray t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Determines if the type isn't undefined
isRealType :: Type -> Bool
isRealType t = baseTypeT t `notElem` [TPDummy, TDummy, TNull]
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
isArrLst :: Expr -> Bool
isArrLst ArrayList{} = True
isArrLst _           = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
baseTypeArrLst :: Type -> Type
baseTypeArrLst (TArray _ t) = t
baseTypeArrLst (TList t)    = t
-- en cualquiero otro caso, error, eso no deberia pasar
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
getTPointer :: [Type] -> Maybe Type
getTPointer [] = Just TDummy
getTPointer ts 
    | all (== TDummy)   ts  = return TDummy
    | all (== TPDummy)  ts  = return TPDummy
    | all (\t -> isPointer t || (t == TPDummy) || (t == TDummy)) ts = do
        let listNoDummy = filter (\t -> (t /= TPDummy) && (t /= TDummy)) ts
        t <- getTPointer $ map (\(TPointer t) -> t) listNoDummy

        return (TPointer t)
    | all (not . isPointer) ts = getTLists ts
    | otherwise = Nothing
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
{- |
  Dada una lista de tipos lista, [(List t)], determina si todos los tipos 't'
  concuerdan y retorna ese tipo 't'. Recursiona de ser necesario
  Util para << >> == << >>, << >>::<< >> y derivados

  Examples :

  $ >lt = [TList TInt,TList TDummy]
  $ >getTLists lt
  Just Kit of(Power)
-}
getTLists:: [Type]-> Maybe Type
getTLists [] = Just TDummy
getTLists ts
  -- Contiene solo llamadas a funciones no definidas  ejem << kill f1(), kill f2() >>
  | all (== TPDummy) ts = return TPDummy
    -- Contiene solo TDummy de listas vacias (TList TDummy)
    -- ejem [ TDummy  , TDummy , TDummy ]
  | all (== TDummy) ts  = return TDummy
  -- Contiene solo TDummy de listas vacias(TList TDummy) y/o llamdas a func no definidas 
  -- ejem [ TDummy  ,kill f1() ]
  | all (`elem` [TPDummy, TDummy]) ts = return TPDummy
  -- Contiene solo arreglos y/o TPDummy llamadas a func no definidas 
  -- ejem [ |)2,3,1(|,kill f1() ] -> resultado un puntero
  | all (\t -> isArray t || t == TPDummy) ts = do
    let 
      listNoDummy           = filter (/= TPDummy) ts
      tmb                   = getTLists (map (\(TArray e ta) -> ta) listNoDummy)
      (TArray exprLength t) = head listNoDummy

    if isJust tmb && all (\(TArray e _) -> e == exprLength) listNoDummy then
      -- Si son de tipo equivalente los tipos de las expresiones y los tamanyos son los mismos
      -- Siempre s epuede ver si los tamanyos son los mismos porque estos son 
      -- literales de arrays  
      return $ TArray exprLength (fromJust tmb)
    else 
      Nothing
  -- Contiene solo punteros y/o TDummy de punteros (TPointer TDummy) y/o llamdas a func no definidas 
  -- ejem [ TNull, TDummy,Pointer Int ,kill f1() ] -> resultado un puntero
  | all (\t -> isPointer t || t `elem` [TPDummy, TDummy, TNull]) ts = do
    let 
      listNoDummy = filter (`notElem` [TPDummy, TDummy, TNull]) ts

    if null listNoDummy then 
      return (TPointer TDummy)
    else do
      t <- getTLists $ map (\(TPointer t) -> t) listNoDummy
      return (TPointer t)

  -- Contiene solo listas y/o llamadas a func no definidas 
  -- ejem [ <<>>,kill f1() ] -> resultado lista de algo
  | all (\t -> isList t || (t == TPDummy) || (t == TDummy)) ts = do
    let
      listNoDummy = filter (`notElem` [TPDummy, TDummy]) ts

    t <- getTLists $ map (\(TList t) -> t) listNoDummy
    return (TList t)
  -- Contiene solo TDummys de listas vacias y/0 tpdummys de llamadas a funciones y/o tipos simples
  -- ejem [ <<>>,kill f1() ] -> resultado lista de algo
  | all (not . isList) ts = 

    let 
      listNoDummy = filter (`notElem` [TPDummy, TDummy]) ts
    in
      if not $ null listNoDummy then do
        let
          tExpected = head listNoDummy
        if all (==tExpected) listNoDummy then return tExpected 
        else 
          Nothing
      else 
        Nothing
          
  | otherwise = Nothing
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
{- | Dado un tipo y una lista (List t) regresa el t(si t es una lista recursiona),
  si no es de esa forma regresa Nothing
  Util Para el problema de <<2>>:<< <<>> >> 
-}
getTListAnexo:: Type -> Type -> Maybe Type
getTListAnexo t1 (TList t2) 
    | isSimpleType t1 && isSimpleType t2 && t1 == t2 = Just (TList t1) -- int : [int] = Just int
    | isSimpleType t1 && t2 == TDummy                = Just (TList t1) -- int : [TDummy] = Just int
    | t1 == TDummy && isSimpleType t2                = Just (TList t2) -- TDummy : [int] = Just int
    | t1 == TDummy && t2 == TDummy                   = Just (TList TDummy) -- TDummy : [TDummy] = Just int
    | isList t1 && isList t2 = Just TList <*> getTListAnexo (typeArrLst t1) t2 -- [t] :[[t]] = recursivo t [t]
    | otherwise = Nothing
getTListAnexo _ _ = Nothing
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
getTExpectedTGot :: [(Type,Pos)] -> (Type,(Type,Pos))
getTExpectedTGot lts = (te,tgot)
  where
    accum (s,te,(tg,ptg)) (t,pt) =
      if not s then
        let tmb = getTLists [te,t]
        in
          if isJust tmb then (False,fromJust tmb, (t,pt) )
          else
            (True,te,(t,pt))
      else
        (s,te,(tg,ptg))
    
    (t1,pl1)    = head lts
    (s,te,tgot) = foldl accum (False,t1,(t1,pl1)) lts
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
getPromise :: Id -> Promises -> Maybe Promise
getPromise _ []               = Nothing
getPromise name (promise : r)
  | name == promiseId promise = Just promise
  | otherwise                 = getPromise name r
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- | Gets the statements of each instruction
getInstrSeq :: Instr -> InstrSeq
getInstrSeq (Assigs is _)           = is
getInstrSeq (For _ _ _ is _)        = is
getInstrSeq (ForEach _ _ is _)      = is
getInstrSeq (ForWhile _ _ _ _ is _) = is
getInstrSeq (IF gs _)               = concatMap snd gs
getInstrSeq (Program is _)          = is
getInstrSeq (While _ is _)          = is
getInstrSeq instr                   = [instr]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
isReturn :: Instr -> Bool
isReturn (Return _ _) = True
isReturn _            = False
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
typeReturn :: Instr -> Type
typeReturn (Return e _) = typeE e
typeReturn _            = TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                                 TAC related
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Gets the TAC's label operand
-- getLout :: TAC -> TACOP
-- getLout (T.TACC T.NewLabel out Nothing Nothing) = out
-- getLout _                                       = Nothing
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Gets the value of the label operand
-- getLabel :: TACOP -> String
-- getLabel Nothing            = ""
-- getLabel (Just (T.Label l)) = l
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Creates the TAC's variable operand
tacVariable :: TACInfo -> TACOP
tacVariable v = Just $ T.Variable v
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Creates the TAC's constant operand
tacConstant :: (String, Type) -> TACOP
tacConstant c = Just $ T.Constant c
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Creates the TAC's label operand
tacLabel :: String -> TACOP
tacLabel l = Just $ T.Label l
-------------------------------------------------------------------------------


binOpToTACOP :: BinOp -> T.Operation
binOpToTACOP op = case op of
  Add       -> T.Add
  DivEntera -> T.Div
  Division  -> T.Div
  Minus     -> T.Sub
  Module    -> T.Mod
  Mult      -> T.Mult
  Greater   -> T.Gt
  GreaterEq -> T.Gte
  Less      -> T.Lt
  LessEq    -> T.Lte
  Eq        -> T.Eq
  NotEq     -> T.Neq


negation :: T.Operation -> T.Operation
negation op = case op of
  T.Lt  -> T.Gte
  T.Lte -> T.Gt
  T.Gt  -> T.Lte
  T.Gte -> T.Lt
  T.Eq  -> T.Neq
  T.Neq -> T.Eq


fall :: TACOP
fall = Just $ T.Label "-1"


isFall :: TACOP -> Bool
isFall (Just (T.Label l)) = l == "-1"
isFall _                  = error "Calling isFall with no label operand"


tac :: T.Operation -> TACOP -> TACOP -> TACOP -> [TAC]
tac op lv rv1 rv2 = [T.TACC op lv rv1 rv2]


-------------------------------------------------------------------------------
-- Creates the TAC's GoTo
tacGoto :: TACOP -> [TAC]
tacGoto label = [T.TACC T.GoTo Nothing Nothing label]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Creates the TAC's GoTo
tacNewLabel :: TACOP -> [TAC]
tacNewLabel label = [T.TACC T.NewLabel label Nothing Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacAssign :: TACOP -> TACOP -> [TAC]
tacAssign lv rv = [T.TACC T.Assign lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacMinus :: TACOP -> TACOP -> [TAC]
tacMinus lv rv = [T.TACC T.Minus lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacLen :: TACOP -> TACOP -> [TAC]
tacLen lv rv = [T.TACC T.Length lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacNew :: TACOP -> TACOP -> [TAC]
tacNew lv rv = [T.TACC T.New lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacNot :: TACOP -> TACOP -> [TAC]
tacNot lv rv = [T.TACC T.Not lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacDeref :: TACOP -> TACOP -> [TAC]
tacDeref lv rv = [T.TACC T.Deref lv rv Nothing]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacAdd :: TACOP -> TACOP -> TACOP -> [TAC]
tacAdd lv rv1 rv2 = [T.TACC T.Add lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacSub :: TACOP -> TACOP -> TACOP -> [TAC]
tacSub lv rv1 rv2 = [T.TACC T.Sub lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacMult :: TACOP -> TACOP -> TACOP -> [TAC]
tacMult lv rv1 rv2 = [T.TACC T.Mult lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacDiv :: TACOP -> TACOP -> TACOP -> [TAC]
tacDiv lv rv1 rv2 = [T.TACC T.Div lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacMod :: TACOP -> TACOP -> TACOP -> [TAC]
tacMod lv rv1 rv2 = [T.TACC T.Mod lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacGt :: TACOP -> TACOP -> TACOP -> [TAC]
tacGt lv rv1 rv2 = [T.TACC T.Gt lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacGte :: TACOP -> TACOP -> TACOP -> [TAC]
tacGte lv rv1 rv2 = [T.TACC T.Gte lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacLt :: TACOP -> TACOP -> TACOP -> [TAC]
tacLt lv rv1 rv2 = [T.TACC T.Lt lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacLte :: TACOP -> TACOP -> TACOP -> [TAC]
tacLte lv rv1 rv2 = [T.TACC T.Lte lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacEq :: TACOP -> TACOP -> TACOP -> [TAC]
tacEq lv rv1 rv2 = [T.TACC T.Eq lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacNeq :: TACOP -> TACOP -> TACOP -> [TAC]
tacNeq lv rv1 rv2 = [T.TACC T.Neq lv rv1 rv2]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacIf :: TACOP -> TACOP -> [TAC]
tacIf cond label = [T.TACC T.If Nothing cond label]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- tacIfFalse :: TACOP -> TACOP -> [TAC]
-- tacIfFalse cond label = [T.TACC T.IfFalse Nothing cond label]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacSet :: TACOP -> TACOP -> TACOP -> [TAC]
tacSet x i y = [T.TACC T.Set x i y]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
tacGet :: TACOP -> TACOP -> TACOP -> [TAC]
tacGet x y i = [T.TACC T.Get x y i]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Modify the symbol offset
modifyOffSet :: TACOP -> OffSet -> TACOP
modifyOffSet (Just (T.Variable (Temp n _))) newO      = tacVariable $ Temp n newO
modifyOffSet (Just (T.Variable (TACVar info _))) newO = tacVariable $ TACVar info newO
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Gets the with of the Type
getWidth :: Type -> Int
getWidth array@(TArray _ t) = lenArray array * getWidth t
getWidth TBool              = 1
getWidth TChar              = 1
getWidth TFloat             = 8
getWidth TInt               = 4
getWidth (TList t)          = getWidth t -- ?
getWidth TListEmpty         = 1 -- como base?
getWidth (TNew n)           = 0 -- Reg or Union
getWidth (TPointer t)       = 4 * getWidth t -- Una palabra del procesador
getWidth TNull              = 4 -- Reservemos ese espacio asi igual
-- getWidth TStr               = 1 -- Se calcula cuando se tiene el valor del string
-- getWidth TRegister          = 1 -- Suma de los witdth de cada campo. Esto esta en su offset
-- getWidth TUnion             = 1 -- witdth mayor. Esto esta en su offset
getWidth _                  = -1 -- This shouldn't happen
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
lenArray :: Type -> Int
lenArray (TArray (Literal (Integer i) _) _) = i
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- getAST :: [ExtraInfo] -> InstrSeq
-- getAST []        = []
-- getAST (AST i:_) = i
-- getAST (_:rs)    = getAST rs
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- getRefVar :: Var -> Var
-- getRefVar (Desref v _) = v
-- getRefVar var          = var
-------------------------------------------------------------------------------

