{-
Modulo para la creacion del arbol sintactico abstracto y
la verificacion de tipos

* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.AST where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.Map as M
import Data.List(intercalate)
import Data.Maybe (fromJust)
import Playit.SymbolTable
import Playit.CheckAST
import Playit.Types


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                        Crear nodos del AST
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para identificadores de variables
crearIdvar :: Nombre -> MonadSymTab Vars
crearIdvar name = do
    (symTab, scope) <- get
    let info = lookupInSymTab name symTab

    if info /= Nothing then return $ Var name (getType $ fromJust info)
    else 
        error ("\n\nError semantico, la variable: '" ++ name ++ 
                "', no esta declarada.\n")
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para variables de indexacion
crearVarIndex :: Vars -> Expr -> Vars
crearVarIndex v e = 
    let t = case typeVar v of 
                tipo@(TArray _ _) -> typeArray tipo
                otherwise -> TError
    in VarIndex v e t
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crear el nodo para asignar a un identificador de variable una expresion
crearAsignacion :: Vars -> Expr -> Posicion -> Instr
-- crearAsignacion lval (ListaExpr [] _)
crearAsignacion lval e (line,_)
    | tE == tV = Asignacion lval e
    | otherwise =
        error ("\n\nError semantico en la asignacion: '" ++ var ++
                " <- " ++ expr ++ "'.\nEl tipo de la variable: " ++
                showType tV' ++ ",\n\tno es igual al de la expresion: " ++
                showType tE' ++ ".\nEn la linea: " ++ show line ++ "\n")

    where
        expr   = showE e
        var    = showVar lval
        tE'    = typeE e
        tE     =
            case tE' of
                t@(TArray _ _) -> typeArray t
                otherwise -> tE'
        tV'    = typeVar lval
        tV     = 
            case tV' of
                t@(TArray _ _) -> typeArray t
                otherwise -> tV'
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para un operador binario
crearOpBin :: Tipo -> Tipo -> Tipo -> BinOp -> Expr -> Expr -> Expr
crearOpBin t1 t2 tr op e1 e2 = 
    OpBinario op e1 e2 (checkBin e1 e2 t1 t2 tr)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para un operador unario
crearOpUn :: Tipo -> Tipo -> UnOp -> Expr -> Expr
crearOpUn t tr op e = 
    OpUnario op e (checkUn e t tr)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones con arreglos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para el operador concatenar 2 arreglos, caso especial
crearOpConcat :: BinOp -> Expr -> Expr -> Expr
crearOpConcat op e1 e2 = 
    OpBinario op e1 e2 tr

    where
        t1 = typeE e1
        t2 = typeE e2
        tr = if t1 == t2 then case t1 of
                                (TArray _ _) -> t1
                                otherwise -> TError
             else TError
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para el operador shift de arreglos, caso especial
crearOpShift :: UnOp -> Expr -> Expr
crearOpShift op e =
    let t = typeE e
        tr = case t of 
                (TArray _ _) -> t
                otherwise -> TError
    in OpUnario op e tr
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para representar arreglos de expresiones del mismo tipo      <--------------------------(*)
crearListaExpr :: [Expr] -> Expr
crearListaExpr [] = ListaExpr [] (TArray (Literal (Entero 0) TInt) TDummy)
crearListaExpr e =
    ListaExpr e (TArray (Literal (Entero $ length e) TInt) tipo)
    where
        mapaTipos = map typeE e
        tipoPrimero = head mapaTipos
        tipo = if all (== tipoPrimero) mapaTipos then tipoPrimero else TError
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de condicionales
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Crea el nodo para una instruccion If
crearGuardiaIF :: Expr -> SecuenciaInstr -> Posicion -> Instr
crearGuardiaIF exprCond seqInstrs (line,_)
    | tExpreCondicional == TBool = ButtonIF [(exprCond, seqInstrs)]
    | otherwise = 
        error ("\n\nError semantico en la expresion del if: '" ++ showE exprCond
                ++ "', de tipo: " ++ showType tExpreCondicional ++ ". En la linea: "
                ++ show line ++ "\n")

    where
        tExpreCondicional = typeE exprCond


-- Crea el nodo para una instruccion IfElse
--crearIfOtherwise :: Expr -> SecuenciaInstr -> SecuenciaInstr -> Posicion -> Instr
--crearIfOtherwise e i1 i2 (line,_)
--    | tE == TBool = IfElse e i1 i2
--    | otherwise = 
--        error ("\n\nError semantico en la expresion del if: '" ++ showE e
--                ++ "', de tipo: " ++ showType tE ++ ". En la linea: "
--                ++ show line ++ "\n")

--    where
--        tE = typeE e


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de iteraciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Crea el nodo para una instruccion For
crearFor :: Nombre -> Expr -> Expr -> SecuenciaInstr -> SymTab -> Alcance -> Posicion 
            -> MonadSymTab Instr
crearFor var e1 e2 i st scope pos@(line,_)
    | tE1 == TInt && tE2 == TInt =
        do
            let newI = map (changeTDummyFor TInt st scope) i
            checkInfSup e1 e2 pos st
            return $ For var e1 e2 newI st
    --------------------------------------------------------------------------
    | tE1 == TInt =
        error ("\n\nError semantico en segunda la expresion del 'for': '"
                ++ expr2 ++ "', de tipo: " ++ showType tE2
                ++ ". En la linea: " ++ show line ++ "\n")
    --------------------------------------------------------------------------
    | tE2 == TInt =
        error ("\n\nError semantico en la primera expresion del 'for': '"
                ++ expr1 ++ "', de tipo: " ++ showType tE1 ++ ". En la linea: "
                ++ show line ++ "\n")
    --------------------------------------------------------------------------
    | otherwise =
        error ("\n\nError semantico en la primera expresion: '" ++ expr1 ++
                "', de tipo: " ++ showType tE1 ++ ", y segunda expresion: '"
                ++ expr2 ++ "', de tipo: " ++ showType tE2 ++
                ", del 'for'. En la linea: " ++ show line ++ "\n")

    where
        expr1 = showE e1
        expr2 = showE e2
        tE1 = typeE e1
        tE2 = typeE e2
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para una instruccion ForEach
crearForEachDetermined :: Nombre -> Expr -> SecuenciaInstr -> SymTab -> Alcance -> Posicion -> MonadSymTab Instr
crearForEachDetermined var e1 i st scope pos@(line,_) = do
    return $ ForEach var e1 i st 
    

--------------------------------------------------------------------------------
-- Crea el nodo para una instruccion While
-- crearWhile' = observe "Que pasa con while " crearWhile
crearWhile :: Expr -> SecuenciaInstr -> Posicion -> Instr
crearWhile e i (line,_)
    | tE == TBool = While e i
    | otherwise = 
        error ("\n\nError semantico en la expresion del 'while': '" ++
                showE e ++ "', de tipo: " ++ showType tE ++
                ". En la linea: " ++ show line ++ "\n")
    where
        tE = typeE e
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de entrada y salida
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para una instruccion Print
crearPrint :: Expr -> Posicion -> Instr
crearPrint e (line,_)
    | tE /= TError = Print e
    | otherwise = 
        error ("\n\nError semantico en la expresion del 'print': '" ++
                showE e ++ "', de tipo: " ++ showType tE ++
                ". En la linea: " ++ show line ++ "\n")
    where
        tE = typeE e
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para una instruccion Read
crearRead :: Vars -> Posicion -> Instr
crearRead lval@(Var name _) (line,_) = Read lval
crearRead var@(VarIndex vars e t) l@(line,_)
    | tE /= TError = Read var
    | otherwise = 
        error ("\n\nError semantico en la variable del 'read': '" ++
                showVar var ++ ", con el indice '" ++ showE e ++ "', de tipo: "
                ++ showType tE ++ ". En la linea: " ++ show line ++ "\n")

    where
        tE = typeE e
--------------------------------------------------------------------------------
