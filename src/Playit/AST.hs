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
-- | Creates variables ids node, verify ids declared
var :: Id -> Pos -> MonadSymTab Var
var id p = do
    (symTab, activeScopes, _) <- get
    fileCode <- ask
    let infos = lookupInScopes activeScopes id symTab

    if isJust infos then do
        let isVar symInfo = getCategory symInfo == Variables
            v = filter isVar (fromJust infos)

        if null v then
            error $ errorMessage "This is not a variable" fileCode p
        else
            return $ Var id (getType $ head v)

    else error $ errorMessage "Variable not declared in active scopes" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates idexed variables node
index :: Var -> Expr -> Var
index v e = 
    let t = case typeVar v of 
                tipo@(TArray _ _) -> typeArrLst tipo
                tipo@(TList _) -> typeArrLst tipo
                tipo@(TPointer t) -> t
                _ -> TError
    in Index v e t
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the registers / unions fields
field :: Var -> Id -> Pos -> MonadSymTab Var
field var field p = do
    (symTab, _, _) <- get
    fileCode@(file,code) <- ask
    
    -- Verify type 'var' is register / union
    let reg = case typeVar' var of 
            (TNew name) -> name
            _ -> ""
    
    if reg == "" then -- Type error
        error $ errorMessage "Type of field isn't a register or union" fileCode p
    else do
        
        --chequearTipo reg p
        
        let info = lookupInSymTab field symTab

        if isJust info then do
            let isInRegUnion (SymbolInfo _ _ c e) = c == Fields && getRegName e == reg
                symbols = filter isInRegUnion (fromJust info )
                        
            if null symbols then
                error $ errorMessage ("Field not in '"++reg++"'") fileCode p
            else 
                return $ Field var field (getType $ head symbols) 
        else
            error $ errorMessage "Field not declared" fileCode p
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates an assignation node
-- TODO: Modificar para que asigne el primer elemento de un arreglo/lista a la variable
assig :: Var -> Expr -> Pos -> Instr
-- assig lval (ArrayList [] _)
assig lval e (_,_) = Assig lval e
-- | tE == tV = Assig lval e
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
-- crearIncremento lval (line, _) = Assig lval (crearSuma (Variable lval TInt) (Literal (Integer 1) TInt))
-- {-    | typeVar lval == TInt =
--         Assig lval (crearSuma (Variable lval TInt) (Literal (Integer 1) TInt))
--     | otherwise = error("Error semantico en el incremento, variable no es de tipo Integer, en la linea " ++ show line)
-- -}

-- crearDecremento :: Var -> Pos -> Instr
-- crearDecremento lval (line, _) = Assig lval (crearResta (Variable lval TInt) (Literal (Integer 1) TInt))
-- {-    | typeVar lval == TInt =
--         Assig lval (crearResta (Variable lval TInt) (Literal (Integer 1) TInt))
--     | otherwise = error("Error semantico en el decremento, variable no es de tipo Integer, en la linea " ++ show line)
-- -}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the binary operator node
binary :: BinOp -> Expr -> Expr -> Type -> Type -> Type -> Expr
binary op e1 e2 t1 t2 tOp = Binary op e1 e2 tOp
    -- Binary op e1 e2 (checkBin e1 e2 t1 t2 tOp)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the unary operator node
unary :: UnOp -> Expr -> Type -> Type -> Expr
unary op e t tOp = Unary op e tOp
    -- Unary op e (checkUn e t tOp)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                 Create arrays / lists instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates instert in list first index operator node
anexo :: BinOp -> Expr -> Expr -> Expr
anexo op e1 e2 =
    Binary op e1 e2 t

    where
        t2 = typeE e2
        t = if isList t2 then t2 else TError 
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


-------------------------------------------------------------------------------
-- | Creates the length operator node
len :: UnOp -> Expr -> Expr
len op e =
    Unary op e tr
    
    where
        t = typeE e
        tr = if isArray t || isList t then t else TError
-------------------------------------------------------------------------------


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
guard :: Expr -> InstrSeq -> Pos -> (Expr, InstrSeq)
guard cond i p = (cond, i)
{-guard exprCond seqInstrs (line,_)
    | tExpreCondicional == TBool = IF [(exprCond, seqInstrs)]
    | otherwise = 
        error ("\n\nError semantico en la expresion del if: '" ++ showE exprCond
                ++ "', de tipo: " ++ showType tExpreCondicional ++ ". En la linea: "
                ++ show line ++ "\n")

    where
        tExpreCondicional = typeE exprCond-}
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the simple if instruction node
ifSimple :: Expr -> Expr -> Expr -> Type -> Pos -> Expr
ifSimple cond true false t p = IfSimple cond true false t
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
            subroutine = filter isSubroutine (fromJust symInfos)

        if null subroutine then
            error $ errorMessage "This is not a subroutine" fileCode p
        else do
            let nParams = fromJust $ getNParams $ getExtraInfo $ head subroutine
                nArgs = length args
            
            if nArgs == nParams then
                return (Call subroutine args,p)
            else
                let msj = "Amount of arguments: " ++ show nArgs ++
                        " not equal to spected:" ++ show nParams
                in error $ errorMessage msj fileCode p
    else
        error $ errorMessage "Not defined subroutine" fileCode p
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
        error $ errorMessage "This is not a function" fileCode p
    else
        return $ FuncCall function (getType $ head func)
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                      Registers / Unions definitions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

defineRegUnion :: Id -> Type -> Pos -> MonadSymTab ()
defineRegUnion reg regType p = do
    (symTab@(SymTab table), activeScopes@(activeScope:_), scope) <- get
    fileCode <- ask
    let regInfo = lookupInScopes [1] reg symTab

    if isJust regInfo then
        if regType == TRegister then
            error $ errorMessage "Redefined Inventory" fileCode p
        else
            error $ errorMessage "Redefined Items" fileCode p
    else
        let modifySym (SymbolInfo t s _ _) = SymbolInfo t s Fields [FromReg reg]
            updtSym = 
                map (\sym -> if getScope sym == activeScope then modifySym sym else sym)

            newSymTab = SymTab $ M.map updtSym table
            info = [SymbolInfo regType 1 Types []]

        in void $ addToSymTab [reg] info newSymTab activeScopes scope


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                          I/O instructions nodes
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the print instruction node
print :: Expr -> Pos -> MonadSymTab Instr
print e p
    | tE /= TError = return $ Print e
    | otherwise = do
        fileCode <- ask
        error $ errorMessage "Invalid type of expression" fileCode p
    where
        tE = typeE e
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | Creates the read instruction node
read :: Expr -> Pos -> Expr
read e _ = Read e
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
    else error $ errorMessage "Variable not declared in active scopes" fileCode p
-------------------------------------------------------------------------------
