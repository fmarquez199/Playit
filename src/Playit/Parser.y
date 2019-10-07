{
{-
 * Representacion de la gramatica para el analisis sintactico
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.Parser (parse, {-parseRead,-} error) where
import Control.Monad.Trans.State
import Control.Monad.IO.Class
-- import SymbolTable
-- import CheckAST
import Playit.Lexer
import Playit.Types
-- import Eval
-- import AST

}

%name parse
-- %name parseRead Expr
%tokentype { Token }
%error { parseError }
-- %monad { MonadSymTab }


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
  if                { TkBUTTON _ _ }
  proc              { TkBOSS _ _ }
  for               { TkCONTROLLER _ _ }
  print             { TkDROP _ _ }
  else              { TkNotPressed _ _ }
  free              { TkFREE _ _ }
  break             { TkGameOver _ _ }
  input             { TkJOYSTICK _ _ }
  continue          { TkKeepPlaying _ _ }
  funcCall          { TkKILL _ _ }
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
  nombre            { TkID _ _ }

  -- Caracteres

  caracter          { TkCARACTER _ _ }
  string            { TkSTRINGS _ _ }
  
  -- Literares numericos
  
  entero            { TkINT _ _ }
  flotante          { TkFLOAT _ _ }

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
  "|>"              { TkOpenListIndex _ _ }
  "<|"              { TkCloseListIndex _ _ }
  "++"              { TkINCREMENT _ _ }
  "--"              { TkDECREMENT _ _ }
  "<-"              { TkIN  _ _ }
  "->"              { TkTO  _ _ }
  "|}"              { TkOpenArray _ _ }
  "{|"              { TkCloseArray _ _ }
  "|)"              { TkOpenArray _ _ }
  "(|"              { TkCloseArray _ _ }
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

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                          Reglas de asociatividad
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


%nonassoc nombre of pointer
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
%right "#"
%left "?"

%%

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                          Reglas/Producciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Programa
  :  EndLines world programa ":" endLine Instrucciones EndLines ".~" EndLines
    {}
  |  EndLines world programa ":" endLine Instrucciones EndLines ".~"
    {}

EndLines
  : endLine
    {}
  | EndLines endLine
    {}
  -- | {- Lamda -}
  --   {}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                            Declaraciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Declaraciones
  : Declaracion 
    {}
  | Declaraciones endLine Declaracion
    {}

Declaracion
  :  Tipo Identificadores
    {}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                  Identificadores de las declaraciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Identificadores
  : Identificador
    {}
  | Identificadores "," Identificador
    {}

Identificador
  : nombre "=" Expresion
    {}
  | nombre
    {}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                      Lvalues y Tipos de datos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Lvalues, contenedores que identifican a las variables
Lvalue
  : Lvalue "." nombre
    {}
    -- Tokens indexacion
  | Lvalue "|)" Expresion "(|"
    {}
  | Lvalue "|>" Expresion "<|"
    {}
  | pointer Lvalue
    {}
  | nombre
    {}


-- Tipos de datos
Tipo
  : Tipo "|}" Expresion "{|" %prec "|}"
    {}
  | list of Tipo
    {}
  | int
    {}
  | float
    {}
  | bool
    {}
  | char
    {}
  | str
    {}
  | nombre
    {}
  | Tipo pointer
    {}


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                        Secuencia de instrucciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Instrucciones
  : Instrucciones endLine Instruccion
    {}
  | Instruccion
    {}

Instruccion
  : Declaracion
    {}
  | DefinirSubrutina
    {}
  | DefinirRegistro
    {}
  | DefinirUnion
    {}
  | Controller
    {}
  | Play
    {}
  | Button
    {}
  | Asignacion
    {}
  | EntradaSalida
    {}
  | Free
    {}
  | FuncCall
    {}
  | return Expresion
    {}
  | break
    {}
  | continue
    {}


--------------------------------------------------------------------------------
-- Instruccion de asignacion '='
Asignacion
  : Lvalue "=" Expresion
    {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instrucciones de condicionales 'Button', '|' y 'notPressed'
Button
  : if ":" endLine Guardias ".~"
    {}

Guardias
  : Guardia
    {}
  | Guardias Guardia
    {}

Guardia
  : "|" Expresion "}" EndLines Instrucciones endLine
    {}
  | "|" else "}" EndLines Instrucciones endLine
    {}
  | "|" Expresion "}" Instrucciones endLine
    {}
  | "|" else "}" Instrucciones endLine
    {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instruccion de iteracion determinada 'control'
Controller
 : for InitVar1 "->" Expresion ":" endLine Instrucciones endLine ".~"
    {}
 | for InitVar1 "->" Expresion while Expresion ":" endLine Instrucciones endLine ".~"
    {}
 | for InitVar2 ":" endLine Instrucciones endLine ".~"
    {}
 | for InitVar1 "->" Expresion ":" endLine ".~"
    {}
 | for InitVar1 "->" Expresion while Expresion ":" endLine ".~"
    {}
 | for InitVar2 ":" endLine ".~"
    {}


-- Se inserta la variable de iteracion en la tabla de simbolos junto con su
-- valor inicial, antes de construir el arbol de instrucciones del 'for'
InitVar1
  : nombre "=" Expresion
    {}
  | Tipo nombre "=" Expresion
    {}

InitVar2
  : nombre "<-" Expresion %prec "<-"
    {}
  | Tipo nombre "<-" Expresion %prec "<-"
    {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instruccion de iteracion indeterminada 'play lock'
Play
  : do ":" endLine Instrucciones endLine while Expresion endLine ".~"
    {}
  | do ":" endLine while Expresion endLine ".~"
    {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instrucciones de E/S 'drop' y 'joystick'
EntradaSalida
  : print Expresiones
    {}
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Instrucciones para liberar la memoria de los apuntadores 'free'
Free
  : free nombre
    {}
  | free "|}" "{|" nombre
    {}
  | free "<<" ">>" nombre
    {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                           Subrutinas
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

DefinirSubrutina
  : Boss
    {}
  | Monster
    {}

--------------------------------------------------------------------------------
-- Procedimientos
Boss
  : proc nombre "(" Parametros ")" ":" endLine Instrucciones endLine ".~"
    {}
  | proc nombre "(" Parametros ")" ":" endLine ".~"
    {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Funciones
Monster
  : function nombre "(" Parametros ")" Tipo ":" endLine Instrucciones endLine ".~"
    {}
  | function nombre "(" Parametros ")" Tipo ":" endLine ".~"
    {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Definicion de los parametros de las subrutinas
Parametros
  : Parametros "," Parametro
    {}
  | Parametro
    {}


Parametro
  : Tipo nombre
    {}
  | Tipo "?" nombre
    {}
  | {- Lambda -}
    {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Llamada a subrutinas
FuncCall
: funcCall nombre "(" PasarParametros ")" 
{}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Pasaje de los parametros a las subrutinas
PasarParametros
  : PasarParametros "," ParametroPasado
    {}
  | ParametroPasado
    {}


ParametroPasado
  : Expresion
    {}
  | "?" Expresion
    {}
  | {- Lambda -}
    {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                           Expresiones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Expresiones
  : Expresiones "," Expresion
    {}
  | Expresion
    {}


Expresion
  : Expresion "+" Expresion
    {}
  | Expresion "-" Expresion
    {}
  | Expresion "*" Expresion
    {}
  | Expresion "%" Expresion
    {}
  | Expresion "/" Expresion
    {}
  | Expresion "//" Expresion
    {}
  | Expresion "&&" Expresion
    {}
  | Expresion "||" Expresion
    {}
  | Expresion "==" Expresion
    {}
  | Expresion "!=" Expresion
    {}
  | Expresion ">=" Expresion
    {}
  | Expresion "<=" Expresion
    {}
  | Expresion ">" Expresion
    {}
  | Expresion "<" Expresion
    {}
  | Expresion ":" Expresion %prec ":"
    {}
  | Expresion "::" Expresion
    {}
  
  --
  | Expresion "?" Expresion ":" Expresion %prec "?"
    {}
  | "(" Expresion ")"
    {}
  | "{" Expresiones "}"
    {}
  | "|}" Expresiones "{|"
    {}
  | "<<" Expresiones ">>"
    {}
  | "<<"  ">>"
    {}
  | FuncCall
    {}
  | new Tipo
    {}
  | input
    {}
  | input Expresion %prec input
    {}

  -- Operadores unarios
  | "-" Expresion %prec negativo
    {}
  | "#" Expresion
    {}
  | "!" Expresion
    {}
  | upperCase Expresion %prec upperCase
    {}
  | lowerCase Expresion %prec lowerCase
    {}
  | Expresion "++"
    {}
  | "++" Expresion 
    {}
  | Expresion "--"
    {}
  | "--" Expresion
    {}
  
  -- Literales
  | true
    {}
  | false
    {}
  | entero
    {}
  | flotante
    {}
  | caracter
    {}
  | string
    {}
  | null
    {}
  | Lvalue
    {}


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                           Registros, Unions
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Registros
DefinirRegistro
  : registro nombre ":" endLine Declaraciones endLine ".~"
    {}
  | registro nombre ":" endLine ".~"
    {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Uniones
DefinirUnion
  : union nombre ":" endLine Declaraciones endLine ".~"
    {}
  | union nombre ":" endLine ".~"
    {}
--------------------------------------------------------------------------------


{
parseError :: [Token] -> a
parseError (h:rs) = 
    error $ "\n\nError sintactico del parser antes de: " ++ (show h) ++ "\n"
}
