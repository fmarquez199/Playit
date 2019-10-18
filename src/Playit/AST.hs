{-
* Modulo para la creacion del arbol sintactico abstracto y
* la verificacion de tipos
*
* Copyright : (c) 
*  Manuel Gonzalez     11-10390
*  Francisco Javier    12-11163
*  Natascha Gamboa     12-11250
-}
module Playit.AST where

import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Map as M
import Data.List(intercalate, null)
import Playit.SymbolTable
import Playit.CheckAST
import Playit.Types


----------------------------------------------------------------------------------------------------------------------------------------------------------------                        Crear nodos del AST
--------------------------------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------- Crea el nodo para identificadores de variables y verifica que estén declarados
crearIdvar :: Nombre -> MonadSymTab Vars
crearIdvar name = do
    (symTab, scopes) <- get
    let info = lookupInSymTab name symTab

    if isJust info then return $ Var name (getType $ head $ fromJust info)
    else 
        error ("\n\nError semantico, la variable: '" ++ name ++ 
                "', no esta declarada.\n")
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------- Crea el nodo para variables de indexacion
crearVarIndex :: Vars -> Expr -> Vars
crearVarIndex v e = 
    let t = case typeVar v of 
                tipo@(TArray _ _) -> typeArrLst tipo
                tipo@(TLista _) -> typeArrLst tipo
                _ -> TError
    in VarIndex v e t
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------- Crea el nodo para variables de acceso a registros, uniones (campos)
crearVarCompIndex :: Vars -> Nombre -> MonadSymTab Vars
crearVarCompIndex v campo = do
    (symTab, scopes) <- get
    let info = lookupInSymTab campo symTab

    if isJust info then return $ VarCompIndex v campo (getType $ head $ fromJust info)
    else 
        error ("\n\nError semantico, el campo: '" ++ campo ++ 
                "', no esta declarado.\n")
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------- Crear el nodo para asignar a un identificador de variable una expresion
-- TODO: Modificar para que asigne el primer elemento de un arreglo/lista a la variable
crearAsignacion :: Vars -> Expr -> Posicion -> Instr
-- crearAsignacion lval (ArrLstExpr [] _)
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


-------------------------------------------------------------------------------
-- TODO: Ver si es realmente necesario
-- crearIncremento :: Vars -> Posicion -> Instr
-- crearIncremento lval (line, _) = Asignacion lval (crearSuma (Variables lval TInt) (Literal (Entero 1) TInt))
-- {-    | typeVar lval == TInt =
--         Asignacion lval (crearSuma (Variables lval TInt) (Literal (Entero 1) TInt))
--     | otherwise = error("Error semantico en el incremento, variable no es de tipo Entero, en la linea " ++ show line)
-- -}

-- crearDecremento :: Vars -> Posicion -> Instr
-- crearDecremento lval (line, _) = Asignacion lval (crearResta (Variables lval TInt) (Literal (Entero 1) TInt))
-- {-    | typeVar lval == TInt =
--         Asignacion lval (crearResta (Variables lval TInt) (Literal (Entero 1) TInt))
--     | otherwise = error("Error semantico en el decremento, variable no es de tipo Entero, en la linea " ++ show line)
-- -}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- TODO: Para verificacion de los tipos, int + int, int+float, float+int, float+float
-- crearSuma :: Expr -> Expr -> Expr
-- crearSuma e1 e2 = OpBinario Suma e1 e2 t
--     where
--         t1 = typeE e1
--         t2 = typeE e2
--         t = if t1 == t2 && t1 == TInt then t1 else TError

-- crearResta :: Expr -> Expr -> Expr
-- crearResta e1 e2 = OpBinario Resta e1 e2 t
--     where
--         t1 = typeE e1
--         t2 = typeE e2
--         t = if t1 == t2 && t1 == TInt then t1 else TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para un operador binario
crearOpBin :: BinOp -> Expr -> Expr -> Tipo -> Tipo -> Tipo -> Expr
crearOpBin op e1 e2 t1 t2 tOp = OpBinario op e1 e2 tOp
    -- OpBinario op e1 e2 (checkBin e1 e2 t1 t2 tOp)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para un operador unario
crearOpUn :: UnOp -> Expr -> Tipo -> Tipo -> Expr
crearOpUn op e t tOp = OpUnario op e tOp
    -- OpUnario op e (checkUn e t tOp)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones con arreglos y listas
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para el operador concatenar 2 listas
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
-- Crea el nodo para el operador agregar un elemento al inicio de la lista
crearOpAnexo :: BinOp -> Expr -> Expr -> Expr
crearOpAnexo op e1 e2 =
    OpBinario op e1 e2 t

    where
        t2 = typeE e2
        t = if isList t2 then t2 else TError 
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para el operador tamaño de array o lista
crearOpLen :: UnOp -> Expr -> Expr
crearOpLen op e =
    OpUnario op e tr
    
    where
        t = typeE e
        tr = if isArray t || isList t then t else TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- TODO
-- Crea el nodo para representar arreglos o lista de expresiones del mismo tipo <---(*)
crearArrLstExpr :: [Expr] -> Expr
crearArrLstExpr [] = ArrLstExpr [] (TArray (Literal (Entero 0) TInt) TDummy)
crearArrLstExpr e =
    ArrLstExpr e (TArray (Literal (Entero $ length e) TInt) tipo)
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
crearCasoSwitch :: Expr -> SecuenciaInstr -> Posicion -> (Expr, SecuenciaInstr)
crearCasoSwitch exprCond seqInstrs (line, _) = (exprCond, seqInstrs)
{-crearGuardiaIF exprCond seqInstrs (line,_)
    | tExpreCondicional == TBool = IF [(exprCond, seqInstrs)]
    | otherwise = 
        error ("\n\nError semantico en la expresion del if: '" ++ showE exprCond
                ++ "', de tipo: " ++ showType tExpreCondicional ++ ". En la linea: "
                ++ show line ++ "\n")

    where
        tExpreCondicional = typeE exprCond

-}


crearIfSimple :: Expr -> Expr -> Expr -> Tipo ->  Posicion -> Expr
crearIfSimple con v f t (linea, col) = IfSimple con v f t
  {-| t con == TBool && t v == t f && t v /= TError = IfSimple con v f
  | otherwise = error ("\n\nError semantico en el operador ternario '? :' en la linea: " ++ show linea ++ " tipo de verdad: " ++ (show $ t v) ++ " tipo de mentira: " ++ (show $ t f))
  where t = typeE-}

crearSwitch :: [(Expr, SecuenciaInstr)] -> Posicion -> MonadSymTab Instr
crearSwitch casos (line, col) = return $ Switch casos

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de iteraciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion For
crearFor :: Nombre -> Expr -> Expr -> SecuenciaInstr -> SymTab -> Alcance -> Posicion -> MonadSymTab Instr
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

-------------------------------------------------------------------------------
crearForWhile :: Nombre -> Expr -> Expr -> Expr -> SecuenciaInstr -> SymTab -> Alcance -> Posicion -> MonadSymTab Instr
crearForWhile var e1 e2 e3 i st scope pos@(line,_) =
    return $ ForWhile var e1 e2 e3 i
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
                ", y tercera expresion: '" ++ expr3 ++ "', de tipo: " ++ showType tE3 ++
                ", del 'for'. En la linea: " ++ show line ++ "\n")

    where
        expr1 = showE e1
        expr2 = showE e2
        expr3 = showE e3
        tE1 = typeE e1
        tE2 = typeE e2
        tE3 = typeE e3 -}


-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion ForEach
crearForEach :: Nombre -> Expr -> SecuenciaInstr -> Posicion -> MonadSymTab Instr
crearForEach var e i pos@(line,_) = return $ ForEach var e i
-------------------------------------------------------------------------------
    

-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion While
-- crearWhile' = observe "Que pasa con while " crearWhile
crearWhile :: Expr -> SecuenciaInstr -> Posicion -> MonadSymTab Instr
crearWhile e i (line,_) = return $ While e i
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
-- crearProc :: Nombre -> Parametros -> SecuenciaInstr -> MonadSymTab Definicion
-- crearProc name params i = do
--     -- TODO: Agregar la subrutina a la symtab
--     (symtab, scopes) <- get
--     let info = [SymbolInfo TDummy (head scopes) Procedimientos]
--     addToSymTab [name] info symtab scopes
--     return $ Proc name params i
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para la definicion de una subrutina
crearSubrutina :: Nombre -> Parametros -> Tipo -> SecuenciaInstr -> MonadSymTab Definicion
crearSubrutina name params returnT i = return $ Func name params returnT i
-------------------------------------------------------------------------------

-- Agrega el nombre de la subrutina a la tabla de símbolos.
crearNombreSubrutina :: Nombre -> Tipo -> Categoria -> MonadSymTab ()
crearNombreSubrutina nombre tipo categoria = do
    (symtab, scopes) <- get
    let info = [SymbolInfo tipo (head scopes) categoria]
    addToSymTab [nombre] info symtab scopes
    return ()

-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que llama a la subrutina
crearSubrutinaCall :: Nombre -> Parametros -> MonadSymTab Subrutina
crearSubrutinaCall nombre params = do
    -- TODO: Verificar que los parametros y la subrutina esten en la symtab y el scope concuerde con el actual
    (symtab, scopes) <- get
    let lista = nombre : (map showVar $ concat $ map getVar $ filter isVar params)
    if all isJust $ inSymbTab lista symtab then
        return $ SubrutinaCall nombre params
    else
        error "Error semantico, al menos uno de los parametros o la subrutina no ha sido definida."
-------------------------------------------------------------------------------

inSymbTab :: [Nombre] -> SymTab -> [Maybe [SymbolInfo]]
inSymbTab [] _ = [Nothing]
inSymbTab [x] symtab = [lookupInSymTab x symtab]
inSymbTab (x:xs) symtab = (lookupInSymTab x symtab):(inSymbTab xs symtab)  

-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que llama a la funcion
crearFuncCall :: Subrutina -> MonadSymTab Expr
crearFuncCall subrutina = do
    -- TODO: Colocar el tipo de retorno de la funcion, buscarlo en la symtab
    (symtab, scopes) <- get
    let nombre = getNombreSubrutina subrutina
    let datos = lookupInSymTab nombre symtab
    let existe = isJust datos
    if existe then do
        let fs = filter (\i -> getCategory i == Funciones) $ fromJust datos
        let found = filter (\i -> getScope i == (maximum $ map getScope fs)) fs
        if not $ null found then
            return $ FuncCall subrutina (getType $ head found)
        else
            error "Error semantico, funcion no definida."
    else
        error "Error semantico, funcion no definida."
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para la definicion de los parametros
crearParam :: Vars -> Tipo -> MonadSymTab Expr
crearParam param t = do
    -- TODO: Agregar el parametro a la symtab
    (symtab, scopes) <- get
    let tipo = getRef param
    let info = [SymbolInfo (getTypeVar param) (head scopes) (Parametros tipo)]
    addToSymTab [showVar param] info symtab scopes
    return $ Variables param t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--            Crear Nodos de las instrucciones de registros y uniones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que define los registros
definirRegistro :: Nombre -> SecuenciaInstr -> MonadSymTab Definicion
definirRegistro id decls = do
    (symTab, scopes@(scope:_)) <- get
    let info = [SymbolInfo TRegistro scope ConstructoresTipos]
    addToSymTab [id] info symTab scopes
    return $ Registro id decls TRegistro
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que define las uniones
definirUnion :: Nombre -> SecuenciaInstr -> MonadSymTab Definicion
definirUnion id decls = do
    (symTab, scopes@(scope:_)) <- get
    let info = [SymbolInfo TUnion scope ConstructoresTipos]
    addToSymTab [id] info symTab scopes
    return $ Registro id decls TUnion
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
crearRead :: Expr -> Posicion -> Expr
crearRead e _ = Read e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de apuntadores
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion Free
crearFree :: Nombre -> MonadSymTab Instr
crearFree var = do
    -- TODO: verificar que nombre este en la symtab y el scope concuerde con el actual
    (symtab, scopes) <- get
    let datos = lookupInSymTab var symtab
    let existe = isJust datos
    if existe then
        -- Falta verificar el alcance
        return $ Free var
    else
        error "Error semantico, variable fuera de alcance."
-------------------------------------------------------------------------------