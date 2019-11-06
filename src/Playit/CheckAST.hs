{- |
 * Checks the AST types
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.CheckAST where

import Data.Maybe (fromJust)
--import Playit.SymbolTable
import Playit.Types


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                        Verificar tipos del AST
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


chequearTipo :: Nombre -> Posicion -> MonadSymTab ()
chequearTipo name p = do
    (symTab, scopes, _) <- get
    file <- ask
    let info = lookupInScopes [1] name symTab
    if isJust info then do
        let sym = fromJust info
        if getCategory sym == Tipos then
            return ()
        else
            error $ "\n\nError: " ++ file ++ ": " ++ show p ++
                "\n\tIdentificador '" ++ show name ++ "' no es un tipo.\n"
    else
        error $ "\n\nError: " ++ file ++ ": " ++ show p ++ "\n\tTipo '" ++
                show name ++ "' no definido.\n"


--------------------------------------------------------------------------------
-- Verifica que el tipo de las 2 expresiones sea el esperado
checkBin :: Expr -> Expr -> Type -> Type -> Type -> Type
checkBin e1 e2 t1 t2 tr
    | tE1 == t1 && tE2 == t2 = tr
    | tE1 == TDummy || tE2 == TDummy = TDummy
    | otherwise = TError

    where
        tE1 = typeE e1
        tE2 = typeE e2
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Verifica que el tipo de la expresion sea el esperado
checkUn :: Expr -> Type -> Type -> Type
checkUn e t tr
    | tE == t = tr
    | tE == TDummy = TDummy
    | otherwise = TError

    where
        tE = typeE e
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Verifica el tipo de las asignaciones en las declaraciones de variables
checkTypesAsigs :: InstrSeq -> Type -> Pos -> InstrSeq
checkTypesAsigs asigs t (line,_)
    | eqTypesAsigs updatedAsigs t = updatedAsigs
    | otherwise = 
        error ("\n\nError semantico en las asignaciones de las declaraciones"
                ++ " de variables.\nTipo esperado: " ++ show t ++
                ".En la linea: " ++ show line ++ "\n")

    where
        updatedAsigs = map (changeTDummyAsigs t) asigs
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--           Verificar que los limites y paso de un 'for' son validos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Verifica que el limite inferior sea menor que el superior en las
-- instrucciones de un for
--checkInfSup :: Expr -> Expr -> Pos -> SymTab -> MonadSymTab Bool
--checkInfSup e1 e2 (line,_) symTab
--    | (evalInt $ evalE symTab e1) <= (evalInt $ evalE symTab e2) = return True
--    | otherwise = 
--        error ("\n\nError semantico. El limite inferior: '" ++ showE e1 ++
--                "' es mayor que el " ++ "limite superior: '" ++ showE e2 ++
--                "' del for. En la linea: " ++ show line ++ "\n")
checkInfSup e1 e2 (line,_) symTab =  return True
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Verifica que el paso sea > 0
--checkStep :: Expr -> Pos -> SymTab -> MonadSymTab Bool
--checkStep e (line,_) symTab
--    | (evalInt $ evalE symTab e) > 0 = return True
--    | otherwise =
--        error ("\n\nError semantico. El paso: '" ++ showE e ++
--                "' es menor o igual que 0 en el for. En la linea: "
--                ++ show line ++ "\n")

checkStep e (line,_) symTab = return True
--    | otherwise =
--        error ("\n\nError semantico. El paso: '" ++ showE e ++
--                "' es menor o igual que 0 en el for. En la linea: "
--                ++ show line ++ "\n")
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--       Cambiar el tipo 'TDummy' cuando se lee el tipo de la declacion
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


changeTDummyList :: Type -> Type-> Type
changeTDummyList (TList TDummy) newT = TList newT
changeTDummyList (TList t) newT      = TList (changeTDummyList t newT)
changeTDummyList t newT               = t



--------------------------------------------------------------------------------
-- Cambia el TDummy de una variable en las declaraciones
changeTDummyLvalAsigs :: Var -> Type -> Var
changeTDummyLvalAsigs (Var n TDummy) t       = Var n t
changeTDummyLvalAsigs var@(Var _ _) _        = var
changeTDummyLvalAsigs (Index var e t') t  =
    let newVar = changeTDummyLvalAsigs var t
    in Index newVar e t'
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Cambia el TDummy de las variables en las declaraciones
changeTDummyAsigs :: Type -> Instr -> Instr
changeTDummyAsigs t (Assig lval e) =
    Assig (changeTDummyLvalAsigs lval t) e
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--          Cambiar el tipo 'TDummy' en las instrucciones del 'for'
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Cambia el TDummy de la variable de iteracion en el 'for'
changeTDummyLval :: Var -> Type -> Var
changeTDummyLval (Var n TDummy) t       = Var n t
changeTDummyLval var@(Var _ _) _       = var
changeTDummyLval (Index var e t') t  =
    let newE = changeTDummyExpr t e
    in Index var newE t'
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Cambia el TDummy de las expresiones en el 'for'
changeTDummyExpr :: Type -> Expr -> Expr
changeTDummyExpr t (Literal lit TDummy) = Literal lit t
--------------------------------------------------------------------------
changeTDummyExpr _ lit@(Literal _ _) = lit
--------------------------------------------------------------------------
changeTDummyExpr t (Variable var TDummy) =
    let newVar = changeTDummyLval var t
    in Variable newVar t
--------------------------------------------------------------------------
changeTDummyExpr _ vars@(Variable _ _) = vars
--------------------------------------------------------------------------
changeTDummyExpr t (ArrayList exprs TDummy) =
    let newExprs = map (changeTDummyExpr t) exprs
    in ArrayList newExprs t
--------------------------------------------------------------------------
changeTDummyExpr _ lst@(ArrayList _ _) = lst
--------------------------------------------------------------------------
changeTDummyExpr t (Unary op e TDummy) =
    let newE = changeTDummyExpr t e
    in Unary op newE t
--------------------------------------------------------------------------
changeTDummyExpr _ opUn@Unary{} = opUn
--------------------------------------------------------------------------
changeTDummyExpr t (Binary op e1 e2 TDummy) =
    let newE1 = changeTDummyExpr t e1
        newE2 = changeTDummyExpr t e2
    in Binary op newE1 newE2 t
--------------------------------------------------------------------------
changeTDummyExpr _ opBin@Binary{} = opBin
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Cambia el TDummy de la secuencia de instrucciones del 'for'
changeTDummyFor :: Type -> SymTab -> Scope -> Instr -> Instr
changeTDummyFor t symTab scope (Assig lval e)
    | isVarIter lval symTab scope =
        error ("\n\nError semantico, la variable de iteracion: '" ++
               show lval ++ "', no se puede modificar.\n")
    | otherwise =
        let newLval = changeTDummyLval lval t
            newE = changeTDummyExpr t e
        in Assig newLval newE
--------------------------------------------------------------------------
-- changeTDummyFor t symTab scope (Program seqI) =
--     let newSeqI = map (changeTDummyFor t symTab scope) seqI
--     in Program newSeqI
--------------------------------------------------------------------------
changeTDummyFor t symTab scope (For name e1 e2 seqI) =
    let newE1 = changeTDummyExpr t e1
        newE2 = changeTDummyExpr t e2
        newSeqI = map (changeTDummyFor t symTab scope) seqI
    in For name newE1 newE2 newSeqI
--------------------------------------------------------------------------
--changeTDummyFor t symTab scope (ForEach name e1 e2 e3 --seqI st) =
    --let newE1 = changeTDummyExpr t e1
--        newE2 = changeTDummyExpr t e2
--        newE3 = changeTDummyExpr t e3
--        newSeqI = map (changeTDummyFor t symTab scope) seqI
--    in ForEach name newE1 newE2 newE3 newSeqI st
--------------------------------------------------------------------------
changeTDummyFor t symTab scope (While e seqI) =
    let newE = changeTDummyExpr t e
        newSeqI = map (changeTDummyFor t symTab scope) seqI
    in While newE newSeqI
--------------------------------------------------------------------------
{-changeTDummyFor t symTab scope (If e seqI) =
    let newE = changeTDummyExpr t e
        newSeqI = map (changeTDummyFor t symTab scope) seqI
    in If newE newSeqI
--------------------------------------------------------------------------
changeTDummyFor t symTab scope (IfElse e seqI1 seqI2) =
    let newE = changeTDummyExpr t e
        newSeqI1 = map (changeTDummyFor t symTab scope) seqI1
        newSeqI2 = map (changeTDummyFor t symTab scope) seqI2
    in IfElse newE newSeqI1 newSeqI2 -}
--------------------------------------------------------------------------
changeTDummyFor t symTab scope (Print e) =
    let newE = changeTDummyExpr t e
    in Print newE

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------
