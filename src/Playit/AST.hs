{- |
 * Creates de abstract syntax tree with types check
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.AST where

import Control.Monad (void)
import Control.Monad.Trans.RWS
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)
import Playit.CheckAST
import Playit.Errors
import Playit.SymbolTable
import Playit.AuxFuncs
import Playit.Types


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                        Crear nodos del AST
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para identificadores de variables y verifica que estén declarados
crearIdvar :: Id -> Pos -> MonadSymTab Var
crearIdvar name p = do
    (symTab, scopes, _) <- get
    fileCode <- ask
    let infos = lookupInScopes scopes name symTab

    if isJust infos then do
        let vars = [Variables,Parameters Value,Parameters Reference]
            isVar si = getCategory si `elem` vars
            var = filter isVar (fromJust infos)

        if null var then
            error $ errorMessage "This is not a variable" fileCode p
        else
            return $ Var name (getType $ head var)

    else error $ errorMessage "Variable not declared in active scopes" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para variables de indexacion
crearVarIndex :: Var -> Expr -> Var
crearVarIndex v e = 
    let t = case typeVar v of 
                tipo@(TArray _ _) -> typeArrLst tipo
                tipo@(TList _) -> typeArrLst tipo
                tipo@(TPointer t) -> t
                _ -> TError
    in Index v e t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para los campos de los registros y uniones
crearCampo :: Var -> Id -> Pos -> MonadSymTab Var
crearCampo v campo p = do
    (symTab, _, _) <- get
    fileCode@(file,code) <- ask
    
    -- Verificar que 'v' tiene como tipo un reg
    let reg = case typeVar' v of 
            (TNew name) -> name
            _ -> ""
    
    if reg == "" then -- Error de tipo
        error ("\n\nError: " ++ file ++ ": " ++ show p ++ "\n\t'" ++ show v ++ 
            "' no es registro o union.\n")
    else do
        
        --chequearTipo reg p
        
        let info = lookupInSymTab campo symTab
        if isJust info then do

            let isInRegUnion (SymbolInfo _ _ c e) = c == Fields && getRegName e == reg
                symbols = filter isInRegUnion (fromJust info ) -- Debería tener un elemento o ninguno
                        
            if null symbols then
                error $ errorMessage ("Field not in '"++reg++"'") fileCode p
            else 
                return $ Field v campo (getType $ head symbols) 
        else
            error $ errorMessage "Field not declared" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crear el nodo para asignar a un identificador de variable una expresion
-- TODO: Modificar para que asigne el primer elemento de un arreglo/lista a la variable
crearAsignacion :: Var -> Expr -> Pos -> Instr
-- crearAsignacion lval (ArrLstExpr [] _)
crearAsignacion lval e (_,_) = Asing lval e
-- | tE == tV = Asing lval e
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
-- crearIncremento :: Var -> Pos -> Instr
-- crearIncremento lval (line, _) = Asing lval (crearSuma (Variable lval TInt) (Literal (Integer 1) TInt))
-- {-    | typeVar lval == TInt =
--         Asing lval (crearSuma (Variable lval TInt) (Literal (Integer 1) TInt))
--     | otherwise = error("Error semantico en el incremento, variable no es de tipo Integer, en la linea " ++ show line)
-- -}

-- crearDecremento :: Var -> Pos -> Instr
-- crearDecremento lval (line, _) = Asing lval (crearResta (Variable lval TInt) (Literal (Integer 1) TInt))
-- {-    | typeVar lval == TInt =
--         Asing lval (crearResta (Variable lval TInt) (Literal (Integer 1) TInt))
--     | otherwise = error("Error semantico en el decremento, variable no es de tipo Integer, en la linea " ++ show line)
-- -}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para un operador binario
crearOpBin :: BinOp -> Expr -> Expr -> Type -> Type -> Type -> Expr
crearOpBin op e1 e2 t1 t2 tOp = Binary op e1 e2 tOp
    -- Binary op e1 e2 (checkBin e1 e2 t1 t2 tOp)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para un operador unario
crearOpUn :: UnOp -> Expr -> Type -> Type -> Expr
crearOpUn op e t tOp = Unary op e tOp
    -- Unary op e (checkUn e t tOp)
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
    Binary op e1 e2 tr

    where
        t1 = typeE e1
        t2 = typeE e2
        tr = if t1 == t2 then case t1 of
                                (TList _) -> t1
                                _ -> TError
             else TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para el operador agregar un elemento al inicio de la lista
crearOpAnexo :: BinOp -> Expr -> Expr -> Expr
crearOpAnexo op e1 e2 =
    Binary op e1 e2 t

    where
        t2 = typeE e2
        t = if isList t2 then t2 else TError 
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para el operador tamaño de array o lista
crearOpLen :: UnOp -> Expr -> Expr
crearOpLen op e =
    Unary op e tr
    
    where
        t = typeE e
        tr = if isArray t || isList t then t else TError
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- TODO
-- Crea el nodo para representar arreglos o lista de expresiones del mismo tipo <---(*)
crearArrLstExpr :: [Expr] -> Expr
crearArrLstExpr [] = ArrLstExpr [] (TArray (Literal (Integer 0) TInt) TDummy)
crearArrLstExpr e =
    ArrLstExpr e (TArray (Literal (Integer $ length e) TInt) tipo)
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
crearGuardiaIF :: Expr -> InstrSeq -> Pos -> (Expr, InstrSeq)
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
crearIfSimple :: Expr -> Expr -> Expr -> Type ->  Pos -> Expr
crearIfSimple cond v f t p = IfSimple cond v f t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
crearIF :: [(Expr, InstrSeq)] -> Pos -> Instr
crearIF casos (line, col) = IF casos
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de iteraciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion For
crearFor :: Id -> Expr -> Expr -> InstrSeq -> SymTab -> Scope -> Pos 
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
crearForWhile :: Id -> Expr -> Expr -> Expr -> InstrSeq -> SymTab
    -> Scope -> Pos  -> MonadSymTab Instr
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
crearForEach :: Id -> Expr -> InstrSeq -> Pos -> MonadSymTab Instr
crearForEach var e i pos@(line,_) =
    return $ ForEach var e i
-------------------------------------------------------------------------------
    

-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion While
-- crearWhile' = observe "Que pasa con while " crearWhile
crearWhile :: Expr -> InstrSeq -> Pos -> Instr
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
definirSubrutina' :: Id -> InstrSeq -> Category -> MonadSymTab ()
definirSubrutina' n i c = void $ updateExtraInfo n c [AST i]
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Agrega el nombre de la subrutina a la tabla de símbolos.
definirSubrutina :: Id -> Category -> Pos -> MonadSymTab ()
definirSubrutina nombre categoria p = do
    (symTab, activeScopes, scope) <- get
    fileCode <- ask
    let info = lookupInSymTab nombre symTab

    if isNothing info then 
        let i = [SymbolInfo TDummy 1 categoria []]
        in addToSymTab [nombre] i symTab activeScopes scope
    else
        error $ errorMessage "Redefined subroutine" fileCode p
    return ()
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Define el parametro en la tabla de simbolos
definirParam :: Var -> MonadSymTab Id
definirParam (Param name t ref) = do
    (symtab, activeScopes@(activeScope:_), scope) <- get
    let info = [SymbolInfo t activeScope (Parameters ref) []]
    addToSymTab [name] info symtab activeScopes scope
    return name
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que llama a la subrutina
crearSubrutinaCall :: Id -> Params -> Pos
                    -> MonadSymTab (Subroutine,Pos)
crearSubrutinaCall nombre args p = do
    (symtab, _, _) <- get
    fileCode <- ask
    let symInfos = lookupInScopes [1,0] nombre symtab
    
    if isJust symInfos then do
        let isSubroutine si = getCategory si `elem` [Procedures,Functions]
            subroutine = filter isSubroutine (fromJust symInfos)

        if null subroutine then
            error $ errorMessage "This is not a subroutine" fileCode p
        else do
            let nParams = fromJust $ getNParams $ getExtraInfo $ head subroutine
                nArgs = length args
            
            if nArgs == nParams then
                return (Call nombre args,p)
            else
                let msj = "Amount of arguments: " ++ show nArgs ++
                        " not equal to spected:" ++ show nParams
                in error $ errorMessage msj fileCode p
    else
        error $ errorMessage "Not defined subroutine" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para la instruccion que llama a la funcion
-- NOTA: Se supone que ya se verifico que la subrutina este definida con
--      crearSubrutinaCall, pues se ejecuta primero
crearFuncCall :: Subroutine -> Pos -> MonadSymTab Expr
crearFuncCall subrutina@(Call nombre _) p = do
    (symtab, _, _) <- get
    fileCode <- ask
    let infos = fromJust $ lookupInScopes [1] nombre symtab
        isFunc si = getCategory si == Functions
        func = filter isFunc infos
    
    if null func then
        error $ errorMessage "This is not a function" fileCode p
    else
        return $ FuncCall subrutina (getType $ head func)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                   Definiciones de registros y uniones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Definicion de union
definirRegistro :: Id -> Pos -> MonadSymTab ()
definirRegistro reg p = do
    (symTab@(SymTab table), activeScopes@(activeScope:_), scope) <- get
    fileCode <- ask
    let regInfo = lookupInScopes [1] reg symTab

    if isJust regInfo then
        error $ errorMessage "Redefined Inventory" fileCode p
    else
        let modifySym (SymbolInfo t s _ _) = SymbolInfo t s Fields [FromReg reg]
            updtSym = 
                map (\sym -> if getScope sym == activeScope then modifySym sym else sym)

            newSymTab = SymTab $ M.map updtSym table
            info = [SymbolInfo TRegister 1 Types []]

        in void $ addToSymTab [reg] info newSymTab activeScopes scope
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Definicion de union
definirUnion :: Id -> Pos -> MonadSymTab ()
definirUnion reg p = do
    (symTab@(SymTab table), activeScopes@(activeScope:_), scope) <- get
    fileCode <- ask
    let regInfo = lookupInScopes [1] reg symTab

    if isJust regInfo then
        error $ errorMessage "Redefined Items" fileCode p
    else
        let modifySym (SymbolInfo t s _ _) = SymbolInfo t s Fields [FromReg reg]
            updSym = 
                map (\sym -> if getScope sym == activeScope then modifySym sym else sym)

            newSymTab = SymTab $ M.map updSym table
            info = [SymbolInfo TUnion 1 Types []]
        in void $ addToSymTab [reg] info newSymTab activeScopes scope
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de entrada y salida
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion Print
crearPrint :: Expr -> Pos -> MonadSymTab Instr
crearPrint e p
    | tE /= TError = return $ Print e
    | otherwise = do
        (file,code) <- ask
        error ("\n\nError: " ++ file ++ ": " ++ show p ++
                "\nExpresion del 'print': '" ++
                show e ++ "', de tipo: " ++ show tE ++ "\n")
    where
        tE = typeE e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion Read
crearRead :: Expr -> Pos -> Expr
crearRead e _ = Read e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--              Crear Nodos de las instrucciones de apuntadores
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el tipo que tenga uno o mas apuntadores a otro tipo
crearTApuntador :: Type -> Type -> Type
crearTApuntador (TPointer TDummy) t = TPointer t
crearTApuntador (TPointer t') t = TPointer $ crearTApuntador t' t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Crea el nodo para una instruccion Free
crearFree :: Id -> Pos -> MonadSymTab Instr
crearFree var p = do
    (symtab, activeScope:_, _) <- get
    fileCode <- ask
    let info = lookupInSymTab var symtab
    
    if isJust info then
        let scopeOk = activeScope `elem` map getScope (fromJust info) in
        
        if scopeOk then return $ Free var
        else error $ errorMessage "Variable out of scope" fileCode p
    else
        error $ errorMessage "Variable not defined" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Funciones auxiliares
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


