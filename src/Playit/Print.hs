{-
Modulo para imprimir el AST y la Tabla de Simbolos

 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.Print (printAST) where

import qualified Data.Map as M
import Playit.Types


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                            Imprimir AST
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Impresion de Instrucciones
printAST :: Int -> Instr -> IO()
printAST n instr = 
    case instr of
        -- Asginacion de expresion
        (Asignacion vars exp) -> do
            putStrLn $ t ++ "Asignacion:"
            putStrLn (t ++ "\tDestino: ") >> printVar (n + 2) vars
            putStrLn (t ++ "\tValor: ") >> printExpr (n + 2) exp
        ------------------------------------------------------------------------
        -- Bloque de instrucciones
        (BloqueInstr seq symTab) -> do
            putStrLn (t ++ "Bloque:") >> printSymTab symTab t
            printSeq (n + 1) seq
        ------------------------------------------------------------------------
        -- Iteracion definida
        (For iter desd hast seq symTab) -> do
            putStrLn $ t ++ "Ciclo Definido:"
            printSymTab symTab t
            putStrLn (t ++ "\tVariable de Iteracion: ") >> printId (n + 2) iter
            putStrLn (t ++ "\tDesde:") >> printExpr (n + 2) desd
            putStrLn (t ++ "\tHasta:") >> printExpr (n + 2) hast
            putStrLn (t ++ "\tCiclo:") >> printSeq (n + 2) seq
        ------------------------------------------------------------------------
        -- Iteracion definida con saltos
        (ForEach nombre expr seq symTab) -> do
            putStrLn $ t ++ "Ciclo determinado:"
            printSymTab symTab t
            putStrLn (t ++ "\tVariable de Iteracion: ") >> printId (n + 2) nombre
            putStrLn (t ++ "\tCondicion:") >> printExpr (n + 2) expr
            putStrLn (t ++ "\tCiclo:") >> printSeq (n + 2) seq
        ------------------------------------------------------------------------
        -- Iteracion indefinida
        (While exp seq) -> do
            putStrLn $ t ++ "Ciclo Indefinido:"
            putStrLn (t ++ "\tGuardia: ") >> printExpr (n + 2) exp
            putStrLn (t ++ "\tCiclo:") >> printSeq (n + 2) seq
        ------------------------------------------------------------------------
        -- Condicional
        (ButtonIF bloques) -> do
            putStrLn $ t ++ "Condicional encontrado:"
            printSeqButtonGuardias (n + 1) bloques
--            putStrLn (t ++ "\tGuardia: ") >> printExpr (n + 2) exp
  --          putStrLn (t ++ "\tExito:") >> printSeq (n + 2) seq
        ------------------------------------------------------------------------
        -- Imprimir expresion
        (Print exp) -> putStrLn (t ++ "Impresion:") >> printExpr (n + 1) exp
        ------------------------------------------------------------------------
        -- Leer valor
        (Read vars) -> putStrLn (t ++ "Lectura:") >> printVar (n + 1) vars
        ------------------------------------------------------------------------
        -- Leer valor
        (SecuenciaDeclaraciones seq symTab) -> do
            --printSymTab symTab t
            putStrLn (t ++ "\tDeclaraciones:") >> printSeq (n + 2) seq
        ------------------------------------------------------------------------
    
    where t = replicate n '\t'
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Subrutina para imprimir secuencias de instrucciones
printSeq :: Int -> [Instr] -> IO()
printSeq n seq = 
    putStrLn (t ++ "Secuencia: ") >> sequence_ (map (printAST $ n + 1) seq)
    
    where t = replicate n '\t'
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Subrutina para imprimir secuencias de guardias
printButtonGuardia :: Int -> (Expr,SecuenciaInstr) -> IO()
printButtonGuardia n (cond,seq) =  do
    putStrLn (t ++ "Condicion: ") >> printExpr (n + 1) cond
    putStrLn (t ++ "Instrucciones: ") >> sequence_ (map (printAST $ n + 1) seq)
    
    where t = replicate n '\t'


printSeqButtonGuardias :: Int -> [(Expr,SecuenciaInstr)] -> IO()
printSeqButtonGuardias n bloques = 
    putStrLn (t ++ "Guardias IF: ") >> sequence_ (map (printButtonGuardia (n + 1)) bloques)
    
    where t = replicate n '\t'


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Subrutina para imprimir variables simples y arreglos
printVar :: Int -> Vars -> IO()
printVar n vars = 
    case vars of
        -- Variable simple
        (Var name _) -> putStrLn $ t ++ "Variable: " ++ name
        ------------------------------------------------------------------------
        -- Arreglo variable
        (VarIndex vars exp _) -> do
            putStrLn (t ++ "Variable de indexacion:") >> printVar (n + 1) vars
            putStrLn $ t ++ "\tIndice: " ++ showE exp
        ------------------------------------------------------------------------
    
    where t = replicate n '\t'
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Subrutina para imprimir identificadores
printId :: Int -> Nombre -> IO()
printId n name = putStrLn $ t ++ "Variable: " ++ name
    
    where t = replicate n '\t'
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Subrutina para imprimir expresiones
printExpr :: Int -> Expr -> IO()
printExpr n e = do
    case e of
        -- Expresion variable
        (Variables vars _) ->
            putStrLn (t ++ "Identificador:") >> printVar (n + 1) vars
        ------------------------------------------------------------------------
        -- Expresion literal
        (Literal l _) -> putStrLn (t ++ "Literal:") >> printLiteral (n + 1) l
        ------------------------------------------------------------------------
        -- Expresion de operacion binaria
        (OpBinario op exp1 exp2 _) -> do
            putStrLn $ t ++ "Operador Binario: " ++ show op
            putStrLn (t ++ "Operando Izquierdo: ") >> printExpr (n + 1) exp1
            putStrLn (t ++ "Operando Derecho: ") >> printExpr (n + 1) exp2
        ------------------------------------------------------------------------
        -- Expresion de operaicion unaria
        (OpUnario op exp _) -> do
            putStrLn $ t ++ "Operador Unario: " ++ show op
            putStrLn (t ++ "Operando: ") >> printExpr (n + 1) exp
        ------------------------------------------------------------------------
        -- Expresion de arreglo explicito
        (ListaExpr exps _) ->
            putStrLn (t ++ "Arreglo:") >> sequence_ (map (printExpr $ n + 1) exps)
        ------------------------------------------------------------------------
    
    where t = replicate n '\t'
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Subrutina para imprimir valores literales
printLiteral :: Int -> Literal -> IO()
printLiteral n l = let t = replicate n '\t' in putStrLn $ t ++ show l
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                      Imprimir Tabla de simbolos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

printSymTab :: SymTab -> String -> IO()
printSymTab (SymTab (table, father)) tab = do
    putStrLn $ tab ++ "------------------------------------------"
    putStrLn $ tab ++ "Tabla de simbolos del alcance actual"
    mapM_ (printSymbol tab) (M.toList table)
    putStrLn $ tab ++ "------------------------------------------\n"


printSymbol :: String -> (Nombre, IdInfo) -> IO()
printSymbol tab (n, (IdInfo t val _)) = 
    putStrLn $ tab ++ "Simbolo: " ++ n ++ "\t| Tipo: " ++ showType t

