{
{-
 * Representacion de la gramatica para el analisis sintactico
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.Parser (parse, error) where
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Playit.SymbolTable
import Playit.CheckAST
import Playit.Lexer
import Playit.Types
import Playit.AST

}

%name parse
%tokentype { Token }
%error { parseError }
%monad { MonadSymTab }


%token
  -- Palabras reservadas

  bool              { TkBATLE _ _ }
  null              { TkDeathZone _ _ }
  registro          { TkINVENTORY _ _ }
  union             { TkITEMS _ _ }
  list              { TkKIT _ _ }
  int               { TkPOWER _ _ }
  char              { TkRUNE _ _ }
  str               { TkRUNES _ _ }
  float             { TkSKILL _ _ }
  switch            { TkBUTTON _ _ }
  proc              { TkBOSS _ _ }
  for               { TkCONTROLLER _ _ }
  print             { TkDROP _ _ }
  else              { TkNotPressed _ _ }
  free              { TkFREE _ _ }
  break             { TkGameOver _ _ }
  input             { TkJOYSTICK _ _ }
  continue          { TkKeepPlaying _ _ }
  call              { TkKILL _ _ }
  while             { TkLOCK _ _ }
  function          { TkMONSTER _ _ }
  do                { TkPLAY _ _ }
  pointer           { TkPUFF _ _ }
  "."               { TkSPAWN _ _ }
  new               { TkSUMMON _ _ }
  return            { TkUNLOCK _ _ }
  world             { TkWORLD _ _ }
  of                { TkOF _ _ }
  endLine           { TkEndLine _ _}

  -- Literales booleanos

  true              { TkWIN _ _ }
  false             { TkLOSE _ _ }

  -- Identificadores

  programa          { TkProgramName _ _ }
  nombre            { TkID _ $$ }
  idtipo            { TkIDTipo _ $$ }

  -- Caracteres

  caracter          { TkCARACTER _ $$ }
  string            { TkSTRINGS _ $$ }
  
  -- Literares numericos
  
  entero            { TkINT _ $$ }
  flotante          { TkFLOAT _ $$ }

  -- Simbolos

  ".~"              { TkFIN _ _ }
  "//"              { TkDivEntera _ _ }
  "||"              { TkOR _ _ }
  "&&"              { TkAND _ _ }
  "<="              { TkLessEqual _ _ }
  "=="              { TkEQUAL _ _ }
  "!="              { TkNotEqual _ _ }
  ">="              { TkGreaterEqual _ _ }
  "<<"              { TkOpenList _ _ }
  ">>"              { TkCloseList _ _ }
  "++"              { TkINCREMENT _ _ }
  "--"              { TkDECREMENT _ _ }
  "<-"              { TkIN  _ _ }
  "->"              { TkTO  _ _ }
  "|}"              { TkOpenArray _ _ }
  "{|"              { TkCloseArray _ _ }
  "|>"              { TkOpenListIndex _ _ }
  "<|"              { TkCloseListIndex _ _ }
  "|)"              { TkOpenArrayIndex _ _ }
  "(|"              { TkCloseArrayIndex _ _ }
  "+"               { TkSUM _ _ }
  "-"               { TkMIN _ _ }
  "*"               { TkMULT _ _ }
  "/"               { TkDIV _ _ }
  "%"               { TkMOD _ _ }
  "#"               { TkLEN _ _ }
  "?"               { TkREF _ _ }
  "!"               { TkNOT _ _ }
  "<"               { TkLessThan _ _ }
  ">"               { TkGreaterThan _ _ }
  "("               { TkOpenParenthesis _ _ }
  ")"               { TkCloseParenthesis _ _ }
  "{"               { TkOpenBrackets _ _ }
  "}"               { TkCloseBrackets _ _ }
  ","               { TkCOMA _ _ }
  ":"               { TkANEXO _ _ }
  "::"              { TkCONCAT _ _}
  "|"               { TkGUARD _ _ }
  "="               { TkASING _ _ }
  upperCase         { TkUPPER _ _ }
  lowerCase         { TkLOWER _ _ }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                          Reglas de asociatividad
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


%nonassoc nombre of
%left "=" ":" "<-"
%right "."
%left "||"
%left "&&"
%nonassoc "==" "!="
%nonassoc ">" "<" ">=" "<="
%left "+" "-"
%left "*" "/" "//" "%"
%right negativo "!" upperCase lowerCase input
%left "++" "|}" "{|" "<<" ">>" "::" "|)" "(|" "|>" "<|"
%left "--"
%right "#" pointer
%left "?"

%%

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                          Reglas/Producciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

ProgramaWrapper :: { Instr }
  : EndLines Programa EndLines  { % return $2 }
  | EndLines Programa           { % return $2 }
  | Programa EndLines           { % return $1 }
  | Programa                    { % return $1 }

  
Programa :: { Instr }
  : PushNewScope world programa ":" EndLines Cosas EndLines ".~"  PopScope
    { Programa $6 }
  {-| PushNewScope world programa ":" EndLines EndLines ".~" PopScope
    { Programa Nada }-}


Cosas :: { Cosas }
  : Cosas EndLines Instruccion  { SecInstr [$3] }
  | Cosas EndLines Definicion   { Definiciones $3 }
  | Instruccion                 { SecInstr [$1] }
  | Definicion                  { Definiciones $1 }


Definicion :: { Definicion }
  : PushNewScope DefinirSubrutina PopScope  { $2 }
  | PushNewScope DefinirRegistro PopScope   { $2 }
  | PushNewScope DefinirUnion PopScope      { $2 }


EndLines :: { () }
  : EndLines endLine  {}
  | endLine           {}

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                            Declaraciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Declaraciones :: { SecuenciaInstr }
  : Declaracion                         { [$1] }
  | Declaraciones EndLines Declaracion  { $3 : $1 }

Declaracion :: { Instr }
  : Tipo Identificadores
    { % do
      let (ids, asigs) = $2
      insertDeclarations ids $1
      return $ SecDeclaraciones asigs
    }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Identificadores de las declaraciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Identificadores :: { ([Nombre], SecuenciaInstr) }
  : Identificador
    {
      let (id, asigs) = $1
      in ([id], asigs)
    }
  | Identificadores "," Identificador
    {
      let ((ids, asigs), (id, asig)) = ($1, $3) 
      in (ids ++ [id], asigs ++ asig)
    }

Identificador :: { (Nombre, SecuenciaInstr) }
  : nombre "=" Expresion  { ($1, [Asignacion (Var $1 TDummy) $3]) }
  | nombre                { ($1, []) }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                      Lvalues y Tipos de datos
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- Lvalues, contenedores que identifican a las variables
Lvalue :: { Vars }
  : Lvalue "." nombre           { % crearVarCompIndex $1 $3 }
  | Lvalue "|)" Expresion "(|"  { crearVarIndex $1 $3 }   -- Indexacion arreglo
  | Lvalue "|>" Expresion "<|"  { crearVarIndex $1 $3 }   -- Indexacion lista
  | pointer Lvalue              { PuffValue $2 (typeVar $2) }
  | nombre                      { % crearIdvar $1 }


-- Tipos de datos
Tipo :: { Tipo }
  : Tipo "|}" Expresion "{|" %prec "|}"   { TArray $3 $1 }
  | list of Tipo                          { TLista $3 }
  | int                                   { TInt }
  | float                                 { TFloat }
  | bool                                  { TBool }
  | char                                  { TChar }
  | str                                   { TStr }
  | idtipo                                { TDummy } -- No se sabe si es un Registro o Union
  | Tipo pointer                          { TApuntador $1 }
  | "(" Tipo ")"                          { $2 }


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                        Secuencia de instrucciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Instrucciones :: { SecuenciaInstr }
  : Instrucciones EndLines Instruccion  { $3 : $1 }
  | Instruccion                         { [$1] }

Instruccion :: { Instr }
  : Declaracion                       { $1 }
  | Asignacion                        { $1 }
  | PushNewScope Controller PopScope  { $2 }
  | PushNewScope Play PopScope        { $2 }
  | Button                            { $1 }
  | ProcCall                          { $1 }
  | EntradaSalida                     { $1 }
  | Free                              { $1 }
  | return Expresion                  { Return $2 }
  | break PopScope                    { Break }
  | continue                          { Continue }


--------------------------------------------------------------------------------
-- Instruccion de asignacion '='
Asignacion :: { Instr }
  : Lvalue "=" Expresion  { crearAsignacion $1 $3 (posicion $2) }
  | Lvalue "++"
    {
      let expr = OpBinario Suma (Variables $1 TInt) (Literal (Entero 1) TInt) TInt
      in crearAsignacion $1 expr (posicion $2)
    }
  | Lvalue "--"
    {
      let expr = OpBinario Resta (Variables $1 TInt) (Literal (Entero 1) TInt) TInt
      in crearAsignacion $1 expr (posicion $2)
    }
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instrucciones de condicionales 'Button', '|' y 'notPressed'
Button :: { Instr }
  : switch ":" EndLines Guardias ".~"
    { % crearSwitch (reverse $4) (posicion $1 )}

Guardias :: { [(Expr, SecuenciaInstr)] }
  : Guardias Guardia { $2 : $1 }
  | Guardia { [$1] }

Guardia :: { (Expr, SecuenciaInstr) }
  : "|" Expresion "}" EndLines Instrucciones EndLines
    { crearCasoSwitch $2 $5 (posicion $1) }
  | "|" Expresion "}" Instrucciones EndLines
    { crearCasoSwitch $2 $4 (posicion $1) }
  | "|" else "}" EndLines Instrucciones EndLines
    { crearCasoSwitch (Literal (Booleano True) TBool) $5 (posicion $1) }
  | "|" else "}" Instrucciones EndLines
    { crearCasoSwitch (Literal (Booleano True) TBool) $4 (posicion $1) }
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instruccion de iteracion determinada 'control'
Controller :: { Instr }
 : for InitVar1 "->" Expresion ":" EndLines Instrucciones EndLines ".~"
    {
      let (varIter, e1) = $2 in For varIter e1 $4 $7
    }
 | for InitVar1 "->" Expresion while Expresion ":" EndLines Instrucciones EndLines ".~"
    {
      let (varIter, e1) = $2 in ForWhile varIter e1 $4 $6 $9 
    }
 | for InitVar1 "->" Expresion ":" EndLines ".~"
    {
      let (varIter, e1) = $2 in For varIter e1 $4 []
    }
 | for InitVar1 "->" Expresion while Expresion ":" EndLines ".~"
    {
      let (varIter, e1) = $2 in ForWhile varIter e1 $4 $6 []
    }
 | for InitVar2 ":" EndLines Instrucciones EndLines ".~"
    {
      let (varIter, e1) = $2 in ForEach varIter e1 $5
    }
 | for InitVar2 ":" EndLines ".~"
    {
      let (varIter, e1) = $2 in ForEach varIter e1 []
    }

-- Se inserta la variable de iteracion en la tabla de simbolos junto con su
-- valor inicial, antes de construir el arbol de instrucciones del 'for'
InitVar1 :: { (Nombre, Expr) }
  : nombre "=" Expresion
    { % do
      -- TODO: Verificar que nombre este en la symtab, asignar valor y el scope concuerde con el actual
      -- Se supone que TDummy deberia cambiar por el tipo de Expresion
      let var = Var $1 TDummy
      insertDeclarations [$1] TDummy
      return $ crearAsignacion var $3 (posicion $2) 
      return ($1, $3)
    }
  | Tipo nombre "=" Expresion
    { % do
      let var = Var $2 $1
      insertDeclarations [$2] $1
      return $ crearAsignacion var $4 (posicion $3)
      return ($2, $4)
    }

InitVar2 :: { (Nombre, Expr) }
  : nombre "<-" Expresion %prec "<-"
    { % do
      -- TODO: Verificar que nombre este en la symtab, asignar valor y el scope concuerde con el actual
      -- Se supone que TDummy deberia cambiar por el tipo de Expresion
      let var = Var $1 TDummy
      insertDeclarations [$1] TDummy
      return $ crearAsignacion var $3 (posicion $2)
      return ($1, $3)
    }
  | Tipo nombre "<-" Expresion %prec "<-"
    { % do
      let var = Var $2 $1
      insertDeclarations [$2] $1
      return $ crearAsignacion var $4 (posicion $3)
      return ($2, $4)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Instruccion de iteracion indeterminada 'play lock'
Play :: { Instr }
  : do ":" EndLines Instrucciones EndLines while Expresion EndLines ".~"
    {
      % crearWhile $7 $4 (posicion $1)
    }
  | do ":" EndLines while Expresion EndLines ".~"
    {
      % crearWhile $5 [] (posicion $1)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Instrucciones de E/S 'drop' y 'joystick'
EntradaSalida :: { Instr }
  : print Expresiones       { crearPrint (crearArrLstExpr $2) (posicion $1) }
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Instrucciones para liberar la memoria de los apuntadores 'free'
Free :: { Instr }
  : free nombre             { % crearFree $2 }
  | free "|}" "{|" nombre   { % crearFree $4 }
  | free "<<" ">>" nombre   { % crearFree $4 }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Subrutinas
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

DefinirSubrutina :: { Definicion }
  : Firma ":" EndLines Instrucciones EndLines ".~" 
    { % do
      let (nombre, parametros, tipo) = $1
      crearSubrutina nombre (reverse parametros) tipo $4
    }
  | Firma ":" EndLines ".~"
    { % do
      let (nombre, parametros, tipo) = $1
      crearSubrutina nombre (reverse parametros) tipo []
    } 

-------------------------------------------------------------------------------
-- Definicion de los parametros de las subrutinas
Firma :: { (Nombre, [Expr], Tipo) }
  : proc nombre "(" Parametros ")" 
    { % do
      crearNombreSubrutina $2 TDummy Procedimientos
      return ($2, $4, TDummy)
    }
  | proc nombre "(" ")" 
    { % do
      crearNombreSubrutina $2 TDummy Procedimientos
      return ($2, [], TDummy)
    }
  | function nombre "(" Parametros ")" Tipo 
    { % do
      crearNombreSubrutina $2 $6 Funciones
      return ($2, $4, $6)
    }
  | function nombre "(" ")" Tipo 
    { % do
      crearNombreSubrutina $2 $5 Funciones
      return ($2, [], $5)
    }

-------------------------------------------------------------------------------
-- Definicion de los parametros de las subrutinas
Parametros :: { [Expr] }
  : Parametros "," Parametro  { $3 : $1 }
  | Parametro                 { [$1] }


Parametro :: { Expr }
  : Tipo nombre       { % crearParam (Param $2 $1 Valor) $1 }
  | Tipo "?" nombre   { % crearParam (Param $3 $1 Referencia) $1 }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Llamada a subrutinas
ProcCall :: { Instr }
  : SubrutinaCall     { ProcCall $1 }

FuncCall :: { Expr }
  : SubrutinaCall     { % crearFuncCall $1 }

SubrutinaCall :: { Subrutina }
  : call nombre "(" PasarParametros ")"   { % crearSubrutinaCall $2 (reverse $4) }
  | call nombre "(" ")"                   { % crearSubrutinaCall $2 [] }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Pasaje de los parametros a las subrutinas
PasarParametros :: { Parametros }
  : PasarParametros "," ParametroPasado   { $3 : $1 }
  | ParametroPasado                       { [$1] }

ParametroPasado :: { Expr }
  : Expresion       { $1 }
  | "?" Expresion   { $2 }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Expresiones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Expresiones::{ [Expr] }
  : Expresiones "," Expresion   { $3 : $1 }
  | Expresion                   { [$1] }

-- crearOpBin : 
--      TipoExpresion1 x TipoExpresion2 x TipoRetorno x Operacion x Expresion1 x Expresion2 =>
--      Chequea tipos
--      Crea la estructura de la operacion

Expresion :: { Expr }
  : Expresion "+" Expresion            { crearOpBin Suma $1 $3 TInt TInt TInt }
  | Expresion "-" Expresion            { crearOpBin Resta $1 $3 TInt TInt TInt }
  | Expresion "*" Expresion            { crearOpBin Multiplicacion $1 $3  TInt TInt TInt }
  | Expresion "%" Expresion            { crearOpBin Modulo $1 $3 TInt TInt TInt }
  | Expresion "/" Expresion            { crearOpBin Division $1 $3 TInt TInt TInt }
  | Expresion "//" Expresion           { crearOpBin DivEntera $1 $3 TInt TInt TInt }
  | Expresion "&&" Expresion           { crearOpBin And $1 $3 TBool TBool TBool }
  | Expresion "||" Expresion           { crearOpBin Or $1 $3 TBool TBool TBool }
  | Expresion "==" Expresion           { crearOpBin Igual $1 $3 TInt TInt TBool }
  | Expresion "!=" Expresion           { crearOpBin Desigual $1 $3 TInt TInt TBool }
  | Expresion ">=" Expresion           { crearOpBin MayorIgual $1 $3 TInt TInt TBool }
  | Expresion "<=" Expresion           { crearOpBin MenorIgual $1 $3 TInt TInt TBool }
  | Expresion ">" Expresion            { crearOpBin Mayor $1 $3 TInt TInt TBool }
  | Expresion "<" Expresion            { crearOpBin Menor $1 $3 TInt TInt TBool }
  | Expresion ":" Expresion %prec ":"  { crearOpAnexo Anexo $1 $3 }
  | Expresion "::" Expresion           { crearOpConcat Concatenacion $1 $3 }
  
  --
  | Expresion "?" Expresion ":" Expresion %prec "?"
    {
      crearIfSimple $1 $3 $5 TDummy (posicion $2)
    }
  | FuncCall                     { $1 }
  | "(" Expresion ")"            { $2 }
  | "{" Expresiones "}"          { crearArrLstExpr $2 }
  | "|}" Expresiones "{|"        { crearArrLstExpr $2 }
  | "<<" Expresiones ">>"        { crearArrLstExpr $2 }
  | "<<"  ">>"                   { crearArrLstExpr [] }
  | new Tipo                     { OpUnario New Null $2 }
  | input Expresion %prec input  { crearRead $2 (posicion $1) }
  | input
    {
      crearRead (Literal ValorVacio TStr) (posicion $1)
    }

  -- Operadores unarios
  | "#" Expresion                        { crearOpLen Longitud $2 }
  | "-" Expresion %prec negativo         { crearOpUn Negativo $2 TInt TInt }
  | "!" Expresion                        { crearOpUn Not $2 TBool TBool }
  | upperCase Expresion %prec upperCase  { crearOpUn UpperCase $2 TChar TChar }
  | lowerCase Expresion %prec lowerCase  { crearOpUn LowerCase $2 TChar TChar }
  
  -- Literales
  | true      { Literal (Booleano True) TBool }
  | false     { Literal (Booleano False) TBool }
  | entero    { Literal (Entero (read $1 :: Int)) TInt }
  | flotante  { Literal (Flotante (read (map (\c -> if c == '\'' then '.' else c) $1) :: Float)) TFloat }
  | caracter  { Literal (Caracter $ $1 !! 0) TChar }
  | string    { Literal (Str $1) TStr }
  | null      { Null }
  | Lvalue    { Variables $1 (typeVar $1) }


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                           Registros, Unions
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Registros
DefinirRegistro :: { Definicion }
  : registro idtipo ":" EndLines Declaraciones EndLines ".~" { % definirRegistro $2 $5 }
  | registro idtipo ":" EndLines ".~"                        { % definirRegistro $2 [] }
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Uniones
DefinirUnion :: { Definicion }
  : union idtipo ":" EndLines Declaraciones EndLines ".~" { % definirUnion $2 $5 }
  | union idtipo ":" EndLines ".~"                        { % definirUnion $2 [] }
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                      Empilar y desempilar de alcances
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Se desempila el alcance actual para pasar al anterior
PopScope  ::  { () }
          :   {- Lambda -}        { % popScope }


-- Empila el nuevo alcance al inicio del anterior
PushNewScope  ::  { () }
              :   {- Lambda -}    { % pushNewScope }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                   Fin de la declaracion de las producciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{
parseError :: [Token] -> a
parseError (h:rs) = 
    error $ "\n\nError sintactico del parser antes de " ++ (show h) ++ "\n"
}
