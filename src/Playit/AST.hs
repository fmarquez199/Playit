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
import Control.Monad.Trans.RWS
import qualified Data.Map as M
import Data.List(intercalate)
import Data.Maybe (fromJust, isJust, isNothing)
import Playit.SymbolTable
import Playit.CheckAST
import Playit.Types


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                        Crear nodos del AST
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para identificadores de variables y verifica que estén declarados
crearIdvar :: Nombre -> MonadSymTab Vars
crearIdvar name = return $ Var name TDummy
{-crearIdvar name = do
    (symTab, scope) <- get
    let info = lookupInSymTab name symTab

    if isJust info then return $ Var name (getType $ fromJust info)
    else 
        error ("\n\nError semantico, la variable: '" ++ name ++ 
                "', no esta declarada.\n") -}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para variables de indexacion
crearVarIndex :: Vars -> Expr -> Vars
crearVarIndex v e = 
    let t = case typeVar v of 
                tipo@(TArray _ _) -> typeArrLst tipo
                tipo@(TLista _) -> typeArrLst tipo
                _ -> TError
    in VarIndex v e t
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crear el nodo para asignar a un identificador de variable una expresion
crearAsignacion :: Vars -> Expr -> Posicion -> Instr
-- crearAsignacion lval (ListaExpr [] _)
crearAsignacion lval e (line, _) = Asignacion lval e
    -- | True = Asignacion lval e
    -- | otherwise =
    --     error ("\n\nError semantico en la asignacion: '" ++ var ++
    --             " <- " ++ expr ++ "'.\nEl tipo de la variable: " ++
    --             showType tV' ++ ",\n\tno es igual al de la expresion: " ++
    --             showType tE' ++ ".\nEn la linea: " ++ show line ++ "\n")

    -- where
    --     expr   = showE e
    --     var    = showVar lval
    --     tE'    = typeE e
    --     tE     =
    --         case tE' of
    --             t@(TArray _ _) -> typeArray t
    --             _ -> tE'
    --     tV'    = typeVar lval
    --     tV     = 
    --         case tV' of
    --             t@(TArray _ _) -> typeArray t
    --             _ -> tV'
-------------------------------------------------------------------------------

crearIncremento :: Vars -> Posicion -> Instr
crearIncremento lval (line, _) = Asignacion lval (crearSuma (Variables lval TInt) (Literal (Entero 1) TInt))
{-    | typeVar lval == TInt =
        Asignacion lval (crearSuma (Variables lval TInt) (Literal (Entero 1) TInt))
    | otherwise = error("Error semantico en el incremento, variable no es de tipo Entero, en la linea " ++ show line)
-}

crearDecremento :: Vars -> Posicion -> Instr
crearDecremento lval (line, _) = Asignacion lval (crearResta (Variables lval TInt) (Literal (Entero 1) TInt))
{-    | typeVar lval == TInt =
        Asignacion lval (crearResta (Variables lval TInt) (Literal (Entero 1) TInt))
    | otherwise = error("Error semantico en el decremento, variable no es de tipo Entero, en la linea " ++ show line)
-}

crearSuma :: Expr -> Expr -> Expr
crearSuma e1 e2 = OpBinario Suma e1 e2 t
    where
        t1 = typeE e1
        t2 = typeE e2
        t = if t1 == t2 && t1 == TInt then t1 else TError

crearResta :: Expr -> Expr -> Expr
crearResta e1 e2 = OpBinario Resta e1 e2 t
    where
        t1 = typeE e1
        t2 = typeE e2
        t = if t1 == t2 && t1 == TInt then t1 else TError

-------------------------------------------------------------------------------
-- Crea el nodo para un operador binario
crearOpBin :: Tipo -> Tipo -> Tipo -> BinOp -> Expr -> Expr -> Expr
crearOpBin t1 t2 tr op e1 e2 = 
    OpBinario op e1 e2 (checkBin e1 e2 t1 t2 tr)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para un operador unario
crearOpUn :: Tipo -> Tipo -> UnOp -> Expr -> Expr
crearOpUn t tr op e = 
    OpUnario op e (checkUn e t tr)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones con arreglos
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para el operador concatenar 2 arreglos, caso especial
crearOpConcat :: BinOp -> Expr -> Expr -> Expr
crearOpConcat op e1 e2 = 
    OpBinario op e1 e2 tr

    where
        t1 = typeE e1
        t2 = typeE e2
        tr = if t1 == t2 then case t1 of
                                (TLista _) -> t1
                                _ -> TError
             else TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
---- >>>>>>>> Se pueden juntar en opUn ??????
crearOpUpper :: UnOp -> Expr -> Expr
crearOpUpper op e = OpUnario op e t
  where t = if typeE e == TChar then TChar else TError


crearOpLower :: UnOp -> Expr -> Expr
crearOpLower op e = OpUnario op e t
  where t = if typeE e == TChar then TChar else TError

crearOpLen :: UnOp -> Expr -> Expr
crearOpLen op e =
    OpUnario op e tr
    
    where
        t = typeE e
        tr = if isArray t || isList t then t else TError
-------------------------------------------------------------------------------

crearOpAnexo :: BinOp -> Expr -> Expr -> Expr
crearOpAnexo op e1 e2 =
    OpBinario op e1 e2 t

    where
        t2 = typeE e2
        t = if isList t2 then t2 else TError 


-------------------------------------------------------------------------------
-- Crea el nodo para representar arreglos de expresiones del mismo tipo <---(*)
crearListaExpr :: [Expr] -> Expr
crearListaExpr [] = ListaExpr [] (TArray (Literal (Entero 0) TInt) TDummy)
crearListaExpr e =
    ListaExpr e (TArray (Literal (Entero $ length e) TInt) tipo)
    where
        mapaTipos = map typeE e
        tipoPrimero = head mapaTipos
        tipo = if all (== tipoPrimero) mapaTipos then tipoPrimero else TError
------------------------------------------------------------------------------


------------------------------------------------------------------------------
------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de condicionales
------------------------------------------------------------------------------
------------------------------------------------------------------------------


-- Crea el nodo para una instruccion If
crearGuardiaIF :: Expr -> SecuenciaInstr -> Posicion -> Instr
crearGuardiaIF exprCond seqInstrs (line,_) = ButtonIF [(exprCond, seqInstrs)]
{-crearGuardiaIF exprCond seqInstrs (line,_)
    | tExpreCondicional == TBool = ButtonIF [(exprCond, seqInstrs)]
    | otherwise = 
        error ("\n\nError semantico en la expresion del if: '" ++ showE exprCond
                ++ "', de tipo: " ++ showType tExpreCondicional ++ ". En la linea: "
                ++ show line ++ "\n")

    where
        tExpreCondicional = typeE exprCond

-}
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



crearIfSimple :: Expr -> Expr -> Expr -> Tipo ->  Posicion -> Expr
crearIfSimple con v f t (linea, col) = IfSimple con v f t
  {-| t con == TBool && t v == t f && t v /= TError = IfSimple con v f
  | otherwise = error ("\n\nError semantico en el operador ternario '? :' en la linea: " ++ show linea ++ " tipo de verdad: " ++ (show $ t v) ++ " tipo de mentira: " ++ (show $ t f))
  where t = typeE-}


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de iteraciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- Crea el nodo para una instruccion For
crearFor :: Nombre -> Expr -> Expr -> SecuenciaInstr -> SymTab -> Alcance -> Posicion 
            -> MonadSymTab Instr
crearFor var e1 e2 i st scope pos@(line,_) = return $ For var e1 e2 i
    -- | tE1 == TInt && tE2 == TInt =
    --     do
    --         let newI = map (changeTDummyFor TInt st scope) i
    --         checkInfSup e1 e2 pos st
    --         return $ For var e1 e2 newI
    -- --------------------------------------------------------------------------
    -- | tE1 == TInt =
    --     error ("\n\nError semantico en segunda la expresion del 'for': '"
    --             ++ expr2 ++ "', de tipo: " ++ showType tE2
    --             ++ ". En la linea: " ++ show line ++ "\n")
    -- --------------------------------------------------------------------------
    -- | tE2 == TInt =
    --     error ("\n\nError semantico en la primera expresion del 'for': '"
    --             ++ expr1 ++ "', de tipo: " ++ showType tE1 ++ ". En la linea: "
    --             ++ show line ++ "\n")
    -- --------------------------------------------------------------------------
    -- | otherwise =
    --     error ("\n\nError semantico en la primera expresion: '" ++ expr1 ++
    --             "', de tipo: " ++ showType tE1 ++ ", y segunda expresion: '"
    --             ++ expr2 ++ "', de tipo: " ++ showType tE2 ++
    --             ", del 'for'. En la linea: " ++ show line ++ "\n")

    -- where
    --     expr1 = showE e1
    --     expr2 = showE e2
    --     tE1 = typeE e1
    --     tE2 = typeE e2
-------------------------------------------------------------------------------

crearForWhile :: Nombre -> Expr -> Expr -> Expr -> SecuenciaInstr -> SymTab -> Alcance -> Posicion 
            -> MonadSymTab Instr
crearForWhile var e1 e2 e3 i st scope pos@(line,_) = return $ ForWhile var e1 e2 e3 i
{-crearForWhile var e1 e2 e3 i st scope pos@(line,_)
    | tE1 == TInt && tE2 == TInt && tE3 == TBool =
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
    | tE3 == TBool =
        error ("\n\nError semantico en la primera expresion: '" ++ expr1 ++
                "', de tipo: " ++ showType tE1 ++ ", y segunda expresion: '"
                ++ expr2 ++ "', de tipo: " ++ showType tE2 ++
                ", del 'for'. En la linea: " ++ show line ++ "\n")
    --------------------------------------------------------------------------
    | otherwise =
        error ("\n\nError semantico en la primera expresion: '" ++ expr1 ++
                "', de tipo: " ++ showType tE1 ++ ", segunda expresion: '"
                ++ expr2 ++ "', de tipo: " ++ showType tE2 ++
                ", y tercera expresion: '" ++ expr3 ++ "', de tipo: " ++ showType tE3 ++", del 'for'. En la linea: " ++ show line ++ "\n")

    where
        expr1 = showE e1
        expr2 = showE e2
        expr3 = showE e3
        tE1 = typeE e1
        tE2 = typeE e2
        tE3 = typeE e3

-}
-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion ForEach
crearForEach :: Nombre -> Expr -> SecuenciaInstr -> SymTab -> Alcance
                        -> Posicion -> MonadSymTab Instr
crearForEach var e1 i st scope pos@(line,_) =
    return $ ForEach var e1 i
    

-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion While
-- crearWhile' = observe "Que pasa con while " crearWhile
crearWhile :: Expr -> SecuenciaInstr -> Posicion -> Instr
crearWhile e i (line,_) = While e i
{-    | tE == TBool = While e i
    | otherwise = 
        error ("\n\nError semantico en la expresion del 'while': '" ++
                showE e ++ "', de tipo: " ++ showType tE ++
                ". En la linea: " ++ show line ++ "\n")
    where
        tE = typeE e
        -}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--        Crear Nodos de las instrucciones de procedimientos y funciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que crea un procedimiento
crearProcedimiento :: Nombre -> Parametros -> SecuenciaInstr -> SymTab -> Alcance
                    -> Posicion -> Definicion
crearProcedimiento name params i st scope pos@(line,_) =
    Proc name params i
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que crea una funcion
crearFuncion :: Nombre -> Parametros -> Tipo -> SecuenciaInstr -> SymTab -> Alcance
                    -> Posicion -> Definicion
crearFuncion name params returnT i st scope pos@(line,_) =
    Func name params returnT i
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que llama a la subrutina
-- llamarSubrutina :: Nombre -> Parametros->Tipo -> Expr
-- llamarSubrutina = SubrutinaCall
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--            Crear Nodos de las instrucciones de registros y uniones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que crea los registros
definirRegistro :: Nombre -> SecuenciaInstr -> Definicion
definirRegistro id decls = Registro id decls TRegistro
-------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que crea las uniones
definirUnion :: Nombre -> SecuenciaInstr -> Definicion
definirUnion id decls = Registro id decls TUnion
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de entrada y salida
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion Read
crearRead :: Posicion -> Expr -> Expr
crearRead _ = Read
-------------------------------------------------------------------------------
