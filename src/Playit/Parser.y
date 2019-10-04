{
{-
 * Representacion de la gramatica para el analisis sintactico
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.Parser (parse, parseRead, error) where
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
--%name parseRead Expr
%tokentype { Token }
%error { parseError }
--%monad { MonadSymTab }


%token
  -- Palabras reservadas

  bool              { TkBTL _ _ }
  null              { TkDTZ _ _ }
  registro          { TkINV _ _ }
  union             { TkITM _ _ }
  list              { TkKIT _ _ }
  int               { TkPWR _ _ }
  char              { TkRNE _ _ }
  str               { TkRNS _ _ }
  float             { TkSKL _ _ }
  if                { TkBTN _ _ }
  proc              { TkBSS _ _ }
  for               { TkCTR _ _ }
  print             { TkDRP _ _ }
  else              { TkNPR _ _ }
  free              { TkFRE _ _ }
  break             { TkGMO _ _ }
  input             { TkJST _ _ }
  continue          { TkKPP _ _ }
  funcCall          { TkKLL _ _ }
  while             { TkLCK _ _ }
  function          { TkMST _ _ }
  do                { TkPLY _ _ }
  pointer           { TkAPT _ _ }
  "."               { TkSPW _ _ }
  new               { TkSMN _ _ }
  return            { TkNLK _ _ }
  world             { TkWRL _ _ }
  of                { TkOFK _ _ }
  endInstr          { TokenEndInstruction }

  -- Literales booleanos

  true              { TkWIN _ _ }
  false             { TkLOS _ _ }

  -- Identificadores

  programa          { TkNMB _ _ }
  nombre            { TkIDF _ _ }

  -- Caracteres

  caracter          { TkCHA _ _ }
  string            { TkSTG _ _ }
  
  -- Literares numericos
  
  entero            { TkINT _ _ }
  flotante          { TkFLT _ _ }

  -- Simbolos

  fin               { TkFIN _ _ }
  "//"              { TkIDV _ _ }
  "||"              { TkLOR _ _ }
  "&&"              { TkAND _ _ }
  "<="              { TkLET _ _ }
  "=="              { TkEQL _ _ }
  "!="              { TkNEQ _ _ }
  ">="              { TkGET _ _ }
  "<<"              { TkLSA _ _ }
  ">>"              { TkLSC _ _ }
  "++"              { TkINC _ _ }
  "--"              { TkDEC _ _ }
  "<-"              { TkIN  _ _ }
  "->"              { TkTO  _ _ }
  "|}"              { TkARA _ _ }
  "{|"              { TkARC _ _ }
  "+"               { TkSUM _ _ }
  "-"               { TkMIN _ _ }
  "*"               { TkTMS _ _ }
  "/"               { TkDVD _ _ }
  "%"               { TkMOD _ _ }
  "#"               { TkLEN _ _ }
  "?"               { TkREF _ _ }
  "!"               { TkEXC _ _ }
  "<"               { TkLTH _ _ }
  ">"               { TkGTH _ _ }
  "("               { TkPRA _ _ }
  ")"               { TkPRC _ _ }
  "["               { TkCRA _ _ }
  "]"               { TkCRC _ _ }
  "{"               { TkLLA _ _ }
  "}"               { TkLLC _ _ }
  ","               { TkCOM _ _ }
  ":"               { TkDSP _ _ }
  "::"              { TkCONCAT  } -- Concatenación de dos listas

  "|"               { TkCON _ _ }
  "="               { TkASG _ _ }
  upperCase         { TkUPP _ _ }
  lowerCase         { TkLOW _ _ }

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                          Reglas de asociatividad
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- VERIFICAR
--%nonassoc nombre
--%right "."
--%left "||"
--%left "&&"
--%nonassoc "==" "!="
--%nonassoc ">" "<" ">=" "<="
--%left "+" "-" "::"
--%left "*" "/" "//" "%"
%right negativo "!"
--%left "++" "|}" "{|" "<<" ">>"
--%left "--"
--%right "#"

%%

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                          Reglas/Producciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Programa :: {SecuenciaInstr}
  :  world programa ":" Instrucciones fin {$4}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                            Declaraciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Declaraciones :: {SecuenciaInstr}
Declaraciones :
    Declaracion {}
  | Declaraciones Declaracion {}

Declaracion ::  {SecuenciaInstr}
  :  Tipo Identificadores {}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                  Identificadores de las declaraciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--Identificadores :: 
Identificadores  : Identificador {}
    | Identificadores "," Identificador {}

--Identificador :: {}
Identificador  : 
    nombre {}
  | nombre "=" Expresion {}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                      Lvalues y Tipos de datos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Lvalues, contenedores que identifican a las variables
--Lvalue :: {}
Lvalue  :
    nombre {}
  | pointer nombre {}
  | Lvalue "." Lvalue {}
  | Lvalue "|}" Expresion "{|" {}
  | Lvalue "<<" Expresion ">>" {}


-- Tipos de datos
--Tipo :: {Tipo}
Tipo 
  : int {}
  | float {}
  | bool {}
  | char {}
  | str {}
  | Tipo "|}" Expresion "{|" {}
  | list of Tipo {}
  | Apuntador{}
  | Registros {}


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                        Secuencia de instrucciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Instrucciones ::  {SecuenciaInstr}
  :  Instrucciones Instruccion {}
  |  Instruccion {}

Instruccion ::  {Instr} 
  : Declaraciones {}
  | Controller {}
  | Play {}
  | Button {}
  | Asignacion {}
  | EntradaSalida {}
  | Free {}
  | Subrutina {}
  | FuncCall {}
  | return Expresion {}
  | break {}
  | continue {}
  | endInstr {}


--------------------------------------------------------------------------------
-- Instruccion de asignacion '='
Asignacion :: {Instr}
  : Lvalue "=" Expresion {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instrucciones de condicionales 'Button', '|' y 'notPressed'
Button :: {Instr}
  : if ":" Guardias fin {}

--Guardias :: {}
Guardias : Guardia {}
  | Guardia Guardias {}

--Guardia :: {}
Guardia  : 
    "|" Expresion "}" Instrucciones {}
  | "|" else "}" Instrucciones      {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instruccion de iteracion determinada 'control'
Controller :: {Instr}
 : for InitVar "=" entero "->" entero ":" Instrucciones fin   {}
 | for InitVar "=" entero "->" entero while Expresion ":" Instrucciones fin   {}
 | for InitVar "<-" nombre ":" Instrucciones fin   {}
 | for InitVar "<-" nombre ":" Instrucciones fin  {}


-- Se inserta la variable de iteracion en la tabla de simbolos junto con su
-- valor inicial, antes de construir el arbol de instrucciones del 'for'
InitVar :: {(Nombre, Expr)}
  : nombre  {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instruccion de iteracion indeterminada 'play unlock'
Play :: {Instr}
  : do ":" Instrucciones while Expresion fin {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instrucciones de E/S 'drop' y 'joystick'
EntradaSalida :: {Instr}
  : nombre "=" input {}
  | nombre "=" input str {}
  | print Expresiones {}
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Instrucciones para liberar la memoria de los apuntadores 'free'
Free :: {Instr}
  : free nombre {}
  | free "|" "}" "{" "|" nombre  {}
  | free "<<" ">>" nombre  {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                           Subrutinas
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

Subrutina :: {}
  : Boss    {}
  | Monster {}

--------------------------------------------------------------------------------
-- Procedimientos
--Boss :: {}
Boss : 
    function nombre "(" Parametros ")" Tipo ":" Instrucciones fin   {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Funciones
Monster :: {}
  : proc nombre "(" Parametros ")" Tipo ":" Instrucciones fin   {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Parametros de las subrutinas
Parametros :: {}
  : Parametro   {}
  | Parametros "," Parametro    {}


Parametro :: {} 
  : Tipo nombre {}
  | Tipo "?" nombre {}
  | {- Lambda -}    {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Llamada a subrutinas
FuncCall :: {Instr}
  : funcCall Subrutina {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                           Expresiones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--Expresiones :: {}
Expresiones : 
    Expresion   {}
  | Expresiones "," Expresion   {}


--Expresion :: {Expr}
Expresion  : 
    Expresion "+" Expresion     {}
  | Expresion "-" Expresion     {}
  | Expresion "*" Expresion     {}
  | Expresion "%" Expresion     {}
  | Expresion "/" Expresion     {}
  | Expresion "//" Expresion    {}
  | Expresion "&&" Expresion    {}
  | Expresion "||" Expresion    {}
  | Expresion "==" Expresion    {}
  | Expresion "!=" Expresion    {}
  | Expresion ">=" Expresion    {}
  | Expresion "<=" Expresion    {}
  | Expresion ">" Expresion     {}
  | Expresion "<" Expresion     {}
  | Expresion ":" Expresion     {}
  | Expresion "::" Expresion    {}
  
  --
  | Expresion "?" Expresion ":" Expresion   {}
  | "(" Expresion ")"       {}
  | "|}" Expresiones "{|"   {}
  | "<<" Expresiones ">>"   {}
  | FuncCall    {}
  | new Tipo    {}
  
  -- Operadores unarios
  | "-" Expresion %prec negativo    {}
  | "#" Expresion           {}
  | "!" Expresion           {}
  | upperCase Expresion     {}
  | lowerCase Expresion     {}
  | Expresion "++"          {}
  | Expresion "--"          {}
  
  -- Literales
  | true        {}
  | false       {}
  | entero      {}
  | flotante    {}
  | caracter    {}
  | string      {}
  | null        {}
  | Lvalue      {}


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                           Registros, Unions, Apuntadores
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Registros y uniones
Registros :: {}
  : registro nombre ":" Declaraciones fin   {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
Apuntador :: {}
  : Tipo pointer nombre {}
--------------------------------------------------------------------------------
