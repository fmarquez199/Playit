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
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)
import Playit.CheckAST
import Playit.SymbolTable
import Playit.Types


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                        Crear nodos del AST
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para identificadores de variables y verifica que estén declarados
crearIdvar :: Nombre -> Posicion -> MonadSymTab Vars
crearIdvar name p = do
    (symTab, scopes, _) <- get
    file <- ask
    let info = lookupInScopes scopes name symTab

    if isJust info then return $ Var name (getType $ fromJust info)
    else 
        error ("\n\nError: " ++ file ++ ": " ++ show p ++ "\n\tVariable '"
                ++ name ++ "' no declarada.\n")
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para variables de indexacion
crearVarIndex :: Vars -> Expr -> Vars
crearVarIndex v e = 
    let t = case typeVar v of 
                tipo@(TArray _ _) -> typeArrLst tipo
                tipo@(TLista _) -> typeArrLst tipo
                tipo@(TApuntador t) -> t
                _ -> TError
    in VarIndex v e t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para los campos de los registros y uniones
crearCampo :: Vars -> Nombre -> Posicion -> MonadSymTab Vars
crearCampo v campo p = do
    (symTab, _, _) <- get
    file <- ask
    
    -- Verificar que 'v' tiene como tipo un reg
    let reg = case typeVar' v of 
            (TNuevo name) -> name
            _ -> ""
    
    if reg == "" then
        error ("\n\nError: " ++ file ++ ": " ++ show p ++ "\n\t'" ++ show v ++ 
            "' no es registro o union.\n")
    else do
        
        --chequearTipo reg p
        
        let info = lookupInSymTab campo symTab
        if isJust info then do

            let isInRegUnion (SymbolInfo _ _ c e) = c == Campos && getRegName e == reg
                symbols = filter isInRegUnion (fromJust info ) -- Debería tener un elemento o ninguno
                        
            if null symbols then
                error ("\n\nError: " ++ file ++ ": " ++ show p ++ "\n\t'" ++ 
                    reg ++ "' no tiene campo '" ++ campo ++ "'.\n")
            else 
                return $ VarCompIndex v campo (getType $ head symbols) 
        else
            error ("\n\nError: " ++ file ++ ": " ++ show p ++ "\n\tCampo '"
                    ++ campo ++ "' no declarado.\n")
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crear el nodo para asignar a un identificador de variable una expresion
-- TODO: Modificar para que asigne el primer elemento de un arreglo/lista a la variable
crearAsignacion :: Vars -> Expr -> Posicion -> Instr
-- crearAsignacion lval (ArrLstExpr [] _)
crearAsignacion lval e (_,_) = Asignacion lval e
-- | tE == tV = Asignacion lval e
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
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de condicionales
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion If
crearGuardiaIF :: Expr -> SecuenciaInstr -> Posicion -> (Expr, SecuenciaInstr)
crearGuardiaIF exprCond seqInstrs (line, _) = (exprCond, seqInstrs)
{-crearGuardiaIF exprCond seqInstrs (line,_)
    | tExpreCondicional == TBool = IF [(exprCond, seqInstrs)]
    | otherwise = 
        error ("\n\nError semantico en la expresion del if: '" ++ showE exprCond
                ++ "', de tipo: " ++ showType tExpreCondicional ++ ". En la linea: "
                ++ show line ++ "\n")
    where
        tExpreCondicional = typeE exprCond-}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
crearIfSimple :: Expr -> Expr -> Expr -> Tipo ->  Posicion -> Expr
crearIfSimple cond v f t p = IfSimple cond v f t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
crearIF :: [(Expr, SecuenciaInstr)] -> Posicion -> Instr
crearIF casos (line, col) = IF casos
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de iteraciones
-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion de for con condicion
crearForWhile :: Nombre -> Expr -> Expr -> Expr -> SecuenciaInstr -> SymTab
    -> Alcance -> Posicion  -> MonadSymTab Instr
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
crearForEach var e i pos@(line,_) =
    return $ ForEach var e i
-------------------------------------------------------------------------------
    

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
--        Definiciones e instrucciones de procedimientos y funciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Actualiza el tipo y  la informacion extra de la subrutina
definirSubrutina' :: Nombre -> SecuenciaInstr -> Categoria
                    -> MonadSymTab SecuenciaInstr
definirSubrutina' n [] c = return (updateExtraInfo n c) >> return []
definirSubrutina' n i c = return (updateExtraInfo n c [AST i]) >> return i
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Agrega el nombre de la subrutina a la tabla de símbolos.
definirSubrutina :: Nombre -> Categoria -> Posicion -> MonadSymTab ()
definirSubrutina nombre categoria p = do
    (symTab, activeScopes, scope) <- get
    file <- ask
    let info = lookupInSymTab nombre symTab

    if isNothing info then 
        let i = [SymbolInfo TDummy 1 categoria []]
        in addToSymTab [nombre] i symTab activeScopes scope
    else
        error $ "\nError: " ++ file ++ ": " ++ show p ++ "\n\tSubrutina '" ++
            nombre ++ "' ya esta definida.\n\t"
            ++ concatMap show (fromJust info) ++ "\n"
    return ()
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Define el parametro en la tabla de simbolos
definirParam :: Vars -> MonadSymTab Nombre
definirParam (Param name t ref) = do
    (symtab, activeScopes@(activeScope:_), scope) <- get
    let info = [SymbolInfo t activeScope (Parametros ref) []]
    addToSymTab [name] info symtab activeScopes scope
    return name
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que llama a la subrutina
crearSubrutinaCall :: Nombre -> Parametros -> Posicion -> MonadSymTab Subrutina
crearSubrutinaCall nombre args p = do
    (symtab, _, _) <- get
    file <- ask
    let symbols = lookupInScopes [1] nombre  symtab
    
    if isJust symbols then do
        let sym =  fromJust symbols

        if getCategory sym `elem` [Procedimientos, Funciones] then do
            
            let nParams = fromJust $ getNParams (getExtraInfo sym )
                nArgs = length args
            
            if nArgs == nParams then
                return $ SubrutinaCall nombre args
            else
                error $ "\n\nError: " ++ file ++ ": " ++ show p ++ 
                    "\n\tSubrutina '" ++ nombre ++ "' espera '" ++ show nParams
                    ++ "' argumentos y se pasaron '" ++ show nArgs ++ "'.\n"
        else
            error $ "\n\nError: " ++ file ++ ": " ++ show p ++ "\n\t'" ++
                    nombre ++ "' no es una subrutina.\n"
    else
        error $ "\n\nError: " ++ file ++ ": " ++ show p ++ "\n\tSubrutina '" ++
                nombre ++ "' no definida.\n"
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que llama a la funcion
-- NOTA: Se supone que ya se verifico que la subrutina este definida con
--      crearSubrutinaCall, pues se ejecuta primero
crearFuncCall :: Subrutina -> MonadSymTab Expr
crearFuncCall subrutina@(SubrutinaCall nombre _) = do
    (symtab, _, _) <- get
    file <- ask
    let sym = fromJust $ lookupInScopes [1] nombre symtab
    
    if getCategory sym == Funciones then
        return $ FuncCall subrutina (getType sym)
    else
        error $ "\n\nError: " ++ file ++ ": (f,c)" ++ "\n\tSubrutina '" ++
            nombre ++ "' no es una funcion.\n"
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                   Definiciones de registros y uniones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Definicion de union
definirRegistro :: Nombre -> SecuenciaInstr -> Posicion -> MonadSymTab SecuenciaInstr
definirRegistro n decls  p = do
    (symTab@(SymTab table), activeScopes@(activeScope:_), scope) <- get
    file <- ask
    let infos = lookupInScopes [1] n symTab
    if isJust infos then
        error $ "\n\nError: " ++ file ++ ": " ++ show p ++ "\n\tInventory '" ++
                n ++ "' ya esta definido.\n"
    else do
        {- TODO: No se puede guardar en el estado el TIPO del scope en el que estoy?
        Lo que haría que cuando las inserto en la tabla de simbolos, ya sea con 
        su categoria y el campo extra adecuado -}
        let modifySym (SymbolInfo t s _ _) = SymbolInfo t s Campos [FromReg n]
            updtSym = 
                map (\sym -> if getScope sym == activeScope then modifySym sym else sym)

            newSymTab = SymTab $ M.map updtSym table
            info = [SymbolInfo TRegistro 1 Tipos []]

        addToSymTab [n] info newSymTab activeScopes scope
        return decls
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Definicion de union
definirUnion :: Nombre -> SecuenciaInstr -> Posicion -> MonadSymTab SecuenciaInstr
definirUnion n decls p = do
    (symTab@(SymTab table), activeScopes@(activeScope:_), scope) <- get
    file <- ask
    let infos = lookupInScopes [1] n symTab
    if isJust infos then
        error $ "\n\nError: " ++ file ++ ": " ++ show p ++ "\n\tItems '" ++
                n ++ "' ya esta definido.\n"
    else do
        let modifySym (SymbolInfo t s _ _) = SymbolInfo t s Campos [FromReg n]
            updSym = 
                map (\sym -> if getScope sym == activeScope then modifySym sym else sym)

            newSymTab = SymTab $ M.map updSym table
            info = [SymbolInfo TUnion 1 Tipos []]
        addToSymTab [n] info newSymTab activeScopes scope
        return decls
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de entrada y salida
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion Print
crearPrint :: Expr -> Posicion -> MonadSymTab Instr
crearPrint e p
    | tE /= TError = return $ Print e
    | otherwise = do
        file <- ask
        error ("\n\nError: " ++ file ++ ": " ++ show p ++
                "\nExpresion del 'print': '" ++
                show e ++ "', de tipo: " ++ show tE ++ "\n")
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
-- Crea el tipo que tenga uno o mas apuntadores a otro tipo
crearTApuntador :: Tipo -> Tipo -> Tipo
crearTApuntador (TApuntador TDummy) t = TApuntador t
crearTApuntador (TApuntador t') t = TApuntador $ crearTApuntador t' t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion Free
crearFree :: Nombre -> Posicion -> MonadSymTab Instr
crearFree var p = do
    (symtab, activeScope:_, _) <- get
    file <- ask
    let info = lookupInSymTab var symtab
    if isJust info then
        let scopeOk = activeScope `elem` map getScope (fromJust info) in
        if scopeOk then return $ Free var
        else error $ "Error: " ++ file ++ ": " ++ show p ++ "\n\tVariable '" ++
                    var ++ "' fuera de alcance.\n"
    else
        error $ "Error: " ++ file ++ ": " ++ show p ++ "\n\tVariable '" ++
                var ++ "' no definida.\n"
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Funciones auxiliares
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------