{- |
 * Creates de abstract syntax tree with type checks
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
--                           Create AST nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates variables ids node
var :: Id -> Pos -> MonadSymTab Var
var id p = do
    (symTab, activeScopes, _) <- get
    fileCode <- ask
    let infos = lookupInScopes activeScopes id symTab

    if isJust infos then do
        let isVar symInfo = getCategory symInfo == Variables
            v = filter isVar (fromJust infos)

        if null v then
            error $ errorMsg "This is not a variable" fileCode p
        else
            return $ Var id (getType $ head v)

    else error $ errorMsg "Variable not declared in active scopes" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates idexed variables node
index :: Var -> Expr -> Pos -> Pos -> MonadSymTab Var
index var expr (lV,cV) (lE,cE) = do
    let pVar  = (lV-1, cV-1)
        pExpr = (lE-1, cE-1)

    (ok,tVar) <- checkIndex var (typeE expr) pVar pExpr
    
    if ok then return $ Index var expr tVar
    else return $ Index var expr TError -- change when no exit with first error encounter
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the registers / unions fields
field :: Var -> Id -> Pos -> MonadSymTab Var
field var field p = do
    (symTab, _, _) <- get
    fileCode@(file,code) <- ask
    
    -- Verify type 'var' is register / union
    let reg = case baseTypeVar var of 
                (TNew name) -> name
                _           -> ""
    
    if reg == "" then -- Type error
        error $ errorMsg "Type of field isn't a register or union" fileCode p
    else do
        
        --chequearTipo reg p
        
        let info = lookupInSymTab field symTab

        if isJust info then do
            let isInRegUnion (SymbolInfo _ _ c e) = c == Fields && getReg e == reg
                symbols = filter isInRegUnion (fromJust info )
                        
            if null symbols then
                error $ errorMsg ("Field not in '"++reg++"'") fileCode p
            else 
                return $ Field var field (getType $ head symbols) 
        else
            error $ errorMsg "Field not declared" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the desreferentiation variable node
desref :: Var -> Pos -> MonadSymTab Var
desref var p = do
    (ok,tVar) <- checkDesref (typeVar var) p
    
    if ok then return $ Desref var tVar
    else return $ Desref var TError -- change when no exit with first error encounter
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the TNew type
newType :: Id -> Pos -> MonadSymTab Type
newType tName p = do
    ok <- checkNewType tName p
    
    if ok then return $ TNew tName
    else return TError -- change when no exit with first error encounter
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Create assignations instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates an assignation node
assig :: Var -> Expr -> Pos -> MonadSymTab Instr
assig lval expr p = do
    ok <- checkAssig (typeVar lval) (typeE expr) p
    
    if ok then return $ Assig lval expr
    else return $ Assig lval (Literal EmptyVal TError) -- change when no exit with first error encounter
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- crearIncremento :: Var -> Pos -> Instr
-- crearIncremento lval (line, _) = Assig lval (crearSuma (Variable lval TInt) (Literal (Integer 1) TInt))
--     | typeVar lval == TInt =
--         Assig lval (crearSuma (Variable lval TInt) (Literal (Integer 1) TInt))
--     | otherwise = error("Error semantico en el incremento, variable no es de tipo Integer")
--

-- crearDecremento :: Var -> Pos -> Instr
-- crearDecremento lval (line, _) = Assig lval (crearResta (Variable lval TInt) (Literal (Integer 1) TInt))
--     | typeVar lval == TInt =
--         Assig lval (crearResta (Variable lval TInt) (Literal (Integer 1) TInt))
--     | otherwise = error("Error semantico en el decremento, variable no es de tipo Integer")
--
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Create operators instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the binary operator node
binary :: BinOp -> Expr -> Expr -> Pos -> MonadSymTab Expr
binary op e1 e2 p = do
    (ok,tOp) <- checkBinary e1 e2 p
    
    if ok then return $ Binary op e1 e2 tOp
    else return $ Binary op e1 e2 TError -- change when no exit with first error encounter
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the unary operator node
unary :: UnOp -> Expr -> Type -> Pos -> MonadSymTab Expr
unary op expr tSpected p = do
    (ok,tOp) <- checkUnary (typeE expr) tSpected p
    
    if ok then return $ Unary op expr tOp
    else return $ Unary op expr TError -- change when no exit with first error encounter
-------------------------------------------------------------------------------
{-
crearOpBinComparable :: BinOp -> Expr -> Expr -> [Tipo] -> Tipo -> Posicion 
                        -> MonadSymTab Expr
crearOpBinComparable op e1 e2 tcomp tOp p
    -- | tE1 == TDummy || tE2 == TDummy = TDummy
    
    | tE1 `elem` allComps && tE2 == tE1  = return $ OpBinario op e1 e2 tOp
    
    | isOpComparable && isArray tE1 && isArray tE2 && tE1 == tE2 = 
        return $ OpBinario op e1 e2 tOp
    
    | isOpComparable && sonlistas && isJust (getTLists [tE1,tE2])  =  -- <<>> == <<2>>
        return $ OpBinario op e1 e2 tOp
    
    | isOpComparable && isPointer tE1 && isPointer tE2 && tE1 == tE2 = 
        return $ OpBinario op e1 e2 tOp
    
    --  TODO: | TRegistro,TUnion
    
    | otherwise = do
        file <- ask
        error $ "\n\nError: " ++ file ++ ": " ++ show p ++ "\n\tOperacion: '" ++
                show op ++ "', tipo de '" ++ show e1 ++ "' y de '" ++  show e2
                ++ "' no son comparables.\n"
    where
        tE1 = typeE e1
        tE2 = typeE e2
        sonlistas = isList tE1 && isList tE2
        isOpComparable = op == Igual || op == Desigual
        allComps = [TChar,TFloat,TInt,TStr] ++ tcomp
-------------------------------------------------------------------------------
-}



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Create arrays / lists instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates instert in list first index operator node
anexo :: BinOp -> Expr -> Expr -> Pos -> MonadSymTab Expr
anexo op e1 e2 p = do
    (ok,tOp) <- checkAnexo e1 e2 p
    
    if ok then return $ Binary op e1 e2 tOp
    else return $ Binary op e1 e2 TError -- change when no exit with first error encounter
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Create concat 2 lists operator node
concatLists :: BinOp -> Expr -> Expr -> Expr
concatLists op e1 e2 = 
    Binary op e1 e2 tr

    where
        t1 = typeE e1
        t2 = typeE e2
        tr = if t1 == t2 then case t1 of
                                (TList _) -> t1
                                _ -> TError
             else TError
-------------------------------------------------------------------------------
{-
-------------------------------------------------------------------------------
-- Crea el nodo para el operador concatenar 2 listas
crearOpConcat ::Expr -> Expr -> Posicion -> MonadSymTab Expr
crearOpConcat e1 e2 p  
    
    | isList te1 && isList te2 && isJust mbtypeList = -- <<2>>:: <<>>
        return $ OpBinario Concatenacion e1 e2 (fromJust mbtypeList)
    
    | otherwise = do
        file <- ask
        error $ "\n\nError: " ++ file ++ ": " ++ show p ++ "\n\t" ++
            "La operación " ++ show Concatenacion  ++ " requiere que expresion '" 
            ++ show e1 ++ "' y expresion '" ++ show e2 ++ "' sean listas del mismo tipo."
    where
        te1 = typeE e1
        te2 = typeE e2
        mbtypeList = getTLists [te1,te2]
-------------------------------------------------------------------------------
-}

-------------------------------------------------------------------------------
-- | Creates the length operator node
len :: UnOp -> Expr -> Expr
len op e =
    Unary op e tr
    
    where
        t = typeE e
        tr = if isArray t || isList t then t else TError
-------------------------------------------------------------------------------
{-
crearOpLen :: Expr -> Posicion -> MonadSymTab Expr
crearOpLen e p
    | isArray t || isList t = return $ OpUnario Longitud e TInt
    | otherwise = do     
        file <- ask
        error $ "\n\nError: " ++ file ++ ": " ++ show p ++ "\n\tOperacion: '"
            ++ show Longitud ++ "', espera que tipo de '" ++ show e ++ 
            "' sea arreglo o lista.\n"
    where
        t = typeE e
-------------------------------------------------------------------------------
-}

-------------------------------------------------------------------------------
-- TODO
-- | Creates the same type array / list node  <---(*)
arrayList :: [Expr] -> Expr
arrayList [] = ArrayList [] (TArray (Literal (Integer 0) TInt) TDummy)
arrayList e =
    ArrayList e (TArray (Literal (Integer $ length e) TInt) tipo)
    where
        mapaTipos = map typeE e
        tipoPrimero = head mapaTipos
        tipo = if all (== tipoPrimero) mapaTipos then tipoPrimero else TError
-------------------------------------------------------------------------------
{-
crearLista :: [Expr] -> Posicion -> MonadSymTab Expr
crearLista [] p = return $ ArrLstExpr [] (TLista TDummy) -- TODO : Recordar quitar el TDummy
crearLista e  p
    | isJust tipo =
        return $ ArrLstExpr e (TLista (fromJust tipo))
    | otherwise = do
        file <- ask
        error $ "\n\nError: " ++ file ++ ": " ++ show p ++ "\n\t" ++
            "Las expresiones de la lista deben ser del mismo tipo.\n"
    where
        mapaTipos   = map typeE e
        tipo = getTLists mapaTipos
-}

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Creates the selection instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the selection instruction node
if' :: [(Expr, InstrSeq)] -> Pos -> Instr
if' cases p = IF cases
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the guards of the selection instruction node
guard :: Expr -> InstrSeq -> Pos -> MonadSymTab (Expr, InstrSeq)
guard cond i p = do
    fileCode <- ask
    let tCond = typeE cond

    if tCond == TBool then return (cond, i)
    else
        error $ semmErrorMsg "Battle" (show tCond) fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the simple if instruction node
ifSimple :: Expr -> Expr -> Expr -> Pos -> MonadSymTab Expr
ifSimple cond true false p = do
    (ok,t) <- checkIfSimple (typeE cond) (typeE true) (typeE false) p
    return $ IfSimple cond true false t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                 Creates the iterations instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the determined iteration instruction node
for :: Id -> Expr -> Expr -> InstrSeq -> SymTab -> Scope -> Pos 
            -> MonadSymTab Instr
for var e1 e2 i st scope pos@(line,_) = return $ For var e1 e2 i
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
-- | Creates the determined conditional iteration instruction node
forWhile :: Id -> Expr -> Expr -> Expr -> InstrSeq -> SymTab -> Scope
                -> Pos -> MonadSymTab Instr
forWhile var e1 e2 e3 i st scope pos@(line,_) = return $ ForWhile var e1 e2 e3 i
{-forWhile var e1 e2 e3 i st scope pos@(line,_)
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
-- | Creates the determined iteration instruction node for arrays / list
forEach :: Id -> Expr -> InstrSeq -> Pos -> MonadSymTab Instr
forEach var e i p = return $ ForEach var e i
-------------------------------------------------------------------------------
    

-------------------------------------------------------------------------------
-- | Creates the indetermined iteration instruction node
while :: Expr -> InstrSeq -> Pos -> Instr
while cond i p = While cond i
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
--                   Procedures / Functions calls nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates subroutine call instruction node
call :: Id -> Params -> Pos -> MonadSymTab (Subroutine,Pos)
call subroutine args p = do
    (symTab, _, _) <- get
    fileCode <- ask
    let symInfos = lookupInScopes [1,0] subroutine symTab
    
    if isJust symInfos then do
        let isSubroutine si = getCategory si `elem` [Procedures, Functions]
            subroutine' = filter isSubroutine (fromJust symInfos)

        if null subroutine' then
            error $ errorMsg "This is not a subroutine" fileCode p
        else do
            let nParams = fromJust $ getNParams $ getExtraInfo $ head subroutine'
                nArgs = length args
            
            if nArgs == nParams then
                return (Call subroutine args,p)
            else
                let msj = "Amount of arguments: " ++ show nArgs ++
                        " not equal to spected:" ++ show nParams
                in error $ errorMsg msj fileCode p
    else
        error $ errorMsg "Not defined subroutine" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates function call expresion node
-- NOTE: Its already verified that subroutine's defined with 'call', because
--      its excuted first
funcCall :: Subroutine -> Pos -> MonadSymTab Expr
funcCall function@(Call name _) p = do
    (symTab, _, _) <- get
    fileCode <- ask
    let infos = fromJust $ lookupInScopes [1] name symTab
        isFunc symInfo = getCategory symInfo == Functions
        func = filter isFunc infos
    
    if null func then
        error $ errorMsg "This is not a function" fileCode p
    else
        return $ FuncCall function (getType $ head func)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                          I/O instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the print instruction node
print' :: Expr -> Pos -> MonadSymTab Instr
print' e p
    | tE /= TError = return $ Print e
    | otherwise = do
        fileCode <- ask
        error $ errorMsg "Invalid type of expression" fileCode p
    where
        tE = typeE e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the read instruction node
read' :: Expr -> Pos -> Expr
read' e _ = Read e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                        Pointers instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the free memory instruction node
free :: Id -> Pos -> MonadSymTab Instr
free var p = do
    (symTab, activeScopes, _) <- get
    fileCode <- ask
    let infos = lookupInScopes activeScopes var symTab
    
    if isJust infos then return $ Free var
    else error $ errorMsg "Variable not declared in active scopes" fileCode p
-------------------------------------------------------------------------------
