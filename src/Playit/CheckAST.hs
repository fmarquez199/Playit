{-
Modulo para la verificacion de tipos del AST y modificaciones pertinentes

* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}

module Playit.CheckAST where

import Data.Maybe (fromJust)
--import Playit.SymbolTable
import Playit.Types
--import Playit.Eval


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                        Verificar tipos del AST
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Verifica que el tipo de las 2 expresiones sea el esperado
checkBin :: Expr -> Expr -> Tipo -> Tipo -> Tipo -> Tipo
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
checkUn :: Expr -> Tipo -> Tipo -> Tipo
checkUn e t tr
    | tE == t = tr
    | tE == TDummy = TDummy
    | otherwise = TError

    where
        tE = typeE e
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Verifica el tipo de las asignaciones en las declaraciones de variables
checkTypesAsigs :: SecuenciaInstr -> Tipo -> Posicion -> SecuenciaInstr
checkTypesAsigs asigs t (line,_)
    | eqTypesAsigs updatedAsigs t = updatedAsigs
    | otherwise = 
        error ("\n\nError semantico en las asignaciones de las declaraciones"
                ++ " de variables.\nTipo esperado: " ++ showType t ++
                ".En la linea: " ++ show line ++ "\n")

    where
        updatedAsigs = map (changeTDummyAsigs t) asigs
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Verifica el el tipo de todas las asignaciones sea el mismo
eqTypesAsigs :: SecuenciaInstr -> Tipo -> Bool
eqTypesAsigs asigs t = all (\(Asignacion _ expr) -> typeE expr == t) asigs  
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Obtiene el tipo asociado a la expresion 
typeE :: Expr -> Tipo
typeE (Variables _ t)           = t
typeE (Literal _ t)             = t
typeE (OpBinario _ _ _ t)       = t
typeE (OpUnario _ _ t)          = t
typeE (ListaExpr _ t)           = t
typeE (Read _)                  = TStr
typeE (IfSimple _ _ _ t)        = t
typeE (SubrutinaCall _  _ t)    = t


--------------------------------------------------------------------------------

isArray (TArray _ _) = True
isArray _ = False

isList (TLista _) = True
isList _ = False


-------------------------------------------------------------------------------
-- Determina el tipo base de los elementos del arreglo
typeArray (TArray _ t@(TArray _ _)) = typeArray t
typeArray (TArray _ t) = t
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Obtiene el tipo asociado a una variable
typeVar :: Vars -> Tipo
typeVar (Var _ t)          = t
typeVar (VarIndex _ _ t)   = t
typeVar (Param _ t _)   = t
typeVar (VarCompIndex _ _ t)   = t
typeVar (PuffValue _ t)   = t
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--           Verificar que los limites y paso de un 'for' son validos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Verifica que el limite inferior sea menor que el superior en las
-- instrucciones de un for
--checkInfSup :: Expr -> Expr -> Posicion -> SymTab -> MonadSymTab Bool
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
--checkStep :: Expr -> Posicion -> SymTab -> MonadSymTab Bool
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
--       Cambiar el tipo 'TDummy' cuando se lee el tipo de la declacio
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Cambia el TDummy de una variable en las declaraciones
changeTDummyLvalAsigs :: Vars -> Tipo -> Vars
changeTDummyLvalAsigs (Var n TDummy) t       = Var n t
changeTDummyLvalAsigs var@(Var _ _) _       = var
changeTDummyLvalAsigs (VarIndex var e t') t  =
    let newVar = changeTDummyLvalAsigs var t
    in VarIndex newVar e t'
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Cambia el TDummy de las variables en las declaraciones
changeTDummyAsigs :: Tipo -> Instr -> Instr
changeTDummyAsigs t (Asignacion lval e) =
    Asignacion (changeTDummyLvalAsigs lval t) e
--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--          Cambiar el tipo 'TDummy' en las instrucciones del 'for'
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Cambia el TDummy de la variable de iteracion en el 'for'
changeTDummyLval :: Vars -> Tipo -> Vars
changeTDummyLval (Var n TDummy) t       = Var n t
changeTDummyLval var@(Var _ _) _       = var
changeTDummyLval (VarIndex var e t') t  =
    let newE = changeTDummyExpr t e
    in VarIndex var newE t'
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Cambia el TDummy de las expresiones en el 'for'
changeTDummyExpr :: Tipo -> Expr -> Expr
changeTDummyExpr t (Literal lit TDummy) = Literal lit t
--------------------------------------------------------------------------
changeTDummyExpr _ lit@(Literal _ _) = lit
--------------------------------------------------------------------------
changeTDummyExpr t (Variables var TDummy) =
    let newVar = changeTDummyLval var t
    in Variables newVar t
--------------------------------------------------------------------------
changeTDummyExpr _ vars@(Variables _ _) = vars
--------------------------------------------------------------------------
changeTDummyExpr t (ListaExpr exprs TDummy) =
    let newExprs = map (changeTDummyExpr t) exprs
    in ListaExpr newExprs t
--------------------------------------------------------------------------
changeTDummyExpr _ lst@(ListaExpr _ _) = lst
--------------------------------------------------------------------------
changeTDummyExpr t (OpUnario op e TDummy) =
    let newE = changeTDummyExpr t e
    in OpUnario op newE t
--------------------------------------------------------------------------
changeTDummyExpr _ opUn@(OpUnario _ _ _) = opUn
--------------------------------------------------------------------------
changeTDummyExpr t (OpBinario op e1 e2 TDummy) =
    let newE1 = changeTDummyExpr t e1
        newE2 = changeTDummyExpr t e2
    in OpBinario op newE1 newE2 t
--------------------------------------------------------------------------
changeTDummyExpr _ opBin@(OpBinario _ _ _ _) = opBin
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Cambia el TDummy de la secuencia de instrucciones del 'for'
changeTDummyFor :: Tipo -> SymTab -> Alcance -> Instr -> Instr
changeTDummyFor t symTab scope (Asignacion lval e)
    | isVarIter lval symTab scope =
        error ("\n\nError semantico, la variable de iteracion: '" ++
               showVar lval ++ "', no se puede modificar.\n")
    | otherwise =
        let newLval = changeTDummyLval lval t
            newE = changeTDummyExpr t e
        in Asignacion newLval newE
--------------------------------------------------------------------------
changeTDummyFor t symTab scope (BloqueInstr seqI st) =
    let newSeqI = map (changeTDummyFor t symTab scope) seqI
    in BloqueInstr newSeqI st
--------------------------------------------------------------------------
changeTDummyFor t symTab scope (For name e1 e2 seqI st) =
    let newE1 = changeTDummyExpr t e1
        newE2 = changeTDummyExpr t e2
        newSeqI = map (changeTDummyFor t symTab scope) seqI
    in For name newE1 newE2 newSeqI st
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
--isVarIter :: Vars -> SymTab -> Alcance -> Bool
--isVarIter (Var name _) symTab scope
--    | (getScope $ fromJust info) < scope = False
--    | otherwise = True
    
--    where info = lookupInSymTab name symTab
--isVarIter (VarIndex var _ _) symTab scope = isVarIter var symTab scope

--isVarIter :: Vars -> SymTab -> Alcance -> Bool
isVarIter (Var name _) symTab scope  = False
--    | otherwise = True
    
--    where info = lookupInSymTab name symTab
--isVarIter (VarIndex var _ _) symTab scope = isVarIter var symTab scope


--------------------------------------------------------------------------------
