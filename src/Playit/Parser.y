{
{-
 * Representacion de la gramatica para el analisis sintactico
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

--module Playit.Parser (parse, parseRead, error) where
module Playit.Parser (parse, error) where
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

  bool              { TkBTL _ _}
  null              { TkDTZ _ _}
  registro          { TkINV _ _}
  union             { TkITM _ _}
  list              { TkKIT _ _}
  int               { TkPWR _ _}
  char              { TkRNE _ _}
  str               { TkRNS _ _}
  float             { TkSKL _ _}
  button            { TkBTN _ _}
  proc              { TkBSS _ _}
  for               { TkCTR _ _}
  print             { TkDRP _ _}
  else              { TkNPR _ _}
  free              { TkFRE _ _}
  break             { TkGMO _ _}
  input             { TkJST _ _}
  continue          { TkKPP _ _}
  funcCall          { TkKLL _ _}
  while             { TkLCK _ _}
  function          { TkMST _ _}
  do                { TkPLY _ _}
  pointer           { TkAPT _ _}
  "."               { TkSPW _ _}
  new               { TkSMN _ _}
  return            { TkNLK _ _}
  world             { TkWRL _ _}
  of                { TkOFK _ _}
  endInstr          { TokenEndInstruction _ _}

  -- Literales booleanos

  true              { TkWIN _ _}
  false             { TkLOS _ _}

  -- Identificadores

  programa          { TkNMB _ _}
  nombre            { TkIDF _ _}

  -- Caracteres

  caracter          { TkCHA _ _}
  string            { TkSTG _ _}
  
  -- Literares numericos
  
  entero            { TkINT _ _}
  flotante          { TkFLT _ _}

  -- Simbolos

  fin               { TkFIN _ _}
  "//"              { TkIDV _ _}
  "||"              { TkLOR _ _}
  "&&"              { TkAND _ _}
  "<="              { TkLET _ _}
  "=="              { TkEQL _ _}
  "!="              { TkNEQ _ _}
  ">="              { TkGET _ _}
  "<<"              { TkLSA _ _}
  ">>"              { TkLSC _ _}
  "++"              { TkINC _ _}
  "--"              { TkDEC _ _}
  "<-"              { TkIN  _ _}
  "->"              { TkTO  _ _}
  "|}"              { TkARA _ _}
  "{|"              { TkARC _ _}
  "+"               { TkSUM _ _}
  "-"               { TkMIN _ _}
  "*"               { TkTMS _ _}
  "/"               { TkDVD _ _}
  "%"               { TkMOD _ _}
  "#"               { TkLEN _ _}
  "?"               { TkREF _ _}
  "!"               { TkEXC _ _}
  "<"               { TkLTH _ _}
  ">"               { TkGTH _ _}
  "("               { TkPRA _ _}
  ")"               { TkPRC _ _}
  "{"               { TkLLA _ _}
  "}"               { TkLLC _ _}
  ","               { TkCOM _ _}
  ":"               { TkDSP _ _}
  "::"              { TkCONCAT  _ _} -- Concatenación de dos listas

  "|"               { TkCON _ _}
  "="               { TkASG _ _}
  upperCase         { TkUPP _ _}
  lowerCase         { TkLOW _ _}
  
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                          Reglas de asociatividad
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- VERIFICAR
--%nonassoc nombre
%right "."
%left "||"
%left "&&"
%nonassoc "==" "!="
%nonassoc ">" "<" ">=" "<="
%left "+" "-" "::"
%left "*" "/" "//" "%"
%right negativo "!"
%left "++" "|}" "{|" "<<" ">>"
%left "--"
%right "#"
--%nonassoc nombre
%%

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                          Reglas/Producciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--Programa :: {}
{-Bug raro donde los archivos siempre tienen un \n al final chequear-}
Programa :
    world programa ":" Instrucciones fin  endInstr {[$4]}

EndInstructs:
    endInstr {}
    | EndInstructs endInstr {}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                        Secuencia de instrucciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--Instrucciones ::  {SecuenciaInstr}
Instrucciones :  
    Instruccion Instrucciones    {}
  |  Instruccion                 {}

--Instruccion ::  {Instr} 
Instruccion  : 
  Declaracion       {}

  {-Creacion de tipos definidos por el usuario Registro,Union-}
  | registro nombre ":" EndInstructs Declaraciones fin      {}

  {-Asignacion de tipos definidos por el usuario Registro,Union-}
  | nombre nombre {}
  | nombre nombre "=" Expresion    {}
     {-Solo se permiten inicializaciones de arreglos cuando se declaran.
        Ejemplo:
            Contacto c = {2,3}
     -}
  | nombre nombre "=" "{" Expresiones "}" {} 
  | Controller          {}
  | Play                {}
  | Button              {}
  | Asignacion          {}
  | EntradaSalida       {}
  | Free                {}
  | FunctionCreate      {}
  | return Expresion    {}
  | break               {}
  | continue            {}
  | Expresion           {}
  | endInstr            {}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                            Declaraciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--Declaraciones :: {SecuenciaInstr}
Declaraciones :
    Declaracion                         {}
  | Declaracion EndInstructs Declaraciones  {}
  | {-empty-} {}

--Declaracion ::  {SecuenciaInstr}
Declaracion  :
    Tipo Identificadores        {}
    | Tipo pointer nombre {}
    | Tipo pointer nombre "=" Expresion {}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                  Identificadores de las declaraciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--Identificadores :: 
Identificadores  : 
    Identificador                       {}
    | Identificadores "," Identificador {}

--Identificador :: {}
Identificador  : 
    nombre                  {}
  | nombre "=" Expresion    {}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                      Lvalues y Tipos de datos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- Lvalues, contenedores que identifican a las variables
--Lvalue :: {}
Lvalue  :
    nombre                      {}
  | pointer nombre              {}
  | Lvalue "." Lvalue           {}
  | Lvalue "|}" Expresion "{|"  {}
  | Lvalue "<<" Expresion ">>"  {}


-- Tipos de datos
--Tipo :: {Tipo}
Tipo : 
    int                         {}
  | float                       {}
  | bool                        {}
  | char                        {}
  | str                         {}
  | Tipo "|}" Expresion "{|"    {}
  | list of Tipo                {}


--------------------------------------------------------------------------------
-- Instruccion de asignacion '='
--Asignacion :: {Instr}
Asignacion  : 
     Lvalue "=" Expresion      {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instrucciones de condicionales 'Button', '|' y 'notPressed'
--Button :: {Instr}
Button  :
    button ":" endInstr Guardias fin {}

--Guardias :: {}
Guardias : 
    Guardia             {}
  | Guardia Guardias    {}

--Guardia :: {}
Guardia  : 
    "|" Expresion "}" Instrucciones {}
  | "|" else "}" Instrucciones      {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instruccion de iteracion determinada 'control'
--Controller :: {Instr}
Controller : 
   for InitVar "=" entero "->" entero ":" Instrucciones fin     {}
 | for InitVar "=" entero "->" entero while Expresion ":" Instrucciones fin   {}
 | for InitVar "<-" nombre ":" Instrucciones fin                {}


-- Se inserta la variable de iteracion en la tabla de simbolos junto con su
-- valor inicial, antes de construir el arbol de instrucciones del 'for'
--InitVar :: {(Nombre, Expr)}
InitVar  : 
    nombre  {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instruccion de iteracion indeterminada 'play unlock'
--Play :: {Instr}
Play  : 
    do ":" Instrucciones while Expresion endInstr fin {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instrucciones de E/S 'drop' y 'joystick'
--EntradaSalida :: {Instr}
EntradaSalida  : 
  print Expresiones       {}
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Instrucciones para liberar la memoria de los apuntadores 'free'
--Free :: {Instr}
Free : 
    free nombre                 {}
  | free "|}" "{|" nombre {}
  | free "<<" ">>" nombre       {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                           Subrutinas
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--Subrutina :: {}
Subrutina :
    Boss    {}
  | Monster {}

--------------------------------------------------------------------------------
-- Procedimientos FALTA NO PERMITIR FUNCIONES ANIDADAS
--Boss :: {}
Boss : 
    function nombre "(" ParametrosFuncionDeclaracion ")" Tipo ":" Instrucciones fin   {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Funciones FALTA NO PERMITIR FUNCIONES ANIDADAS
--Monster :: {}
Monster  : 
    proc nombre "(" ParametrosFuncionDeclaracion ")" Tipo ":" Instrucciones fin       {}
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Parametros de las subrutinas cuando se crean
--Parametros :: {}
ParametrosFuncionDeclaracion  : 
    ParametroEnFuncionDeclaracion                                       {}
  | ParametroEnFuncionDeclaracion "," ParametroEnFuncionDeclaracion     {}


-- Parametro puntual de una subrutina cuando esta es creada
--Parametro :: {} 
ParametroEnFuncionDeclaracion  : 
    Tipo nombre         {}
  | Tipo "?" nombre     {}
  | nombre "?" nombre     {}
  | nombre "?" nombre     {}
  | {- Lambda -}        {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Llamada a subrutinas
--FunctionCreate :: {Instr}
FunctionCreate  : 
    Subrutina  {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                           Expresiones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--Expresiones :: {}
Expresiones : 
    Expresion                   {}
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

  | funcCall nombre         {}
  | funcCall nombre "(" Expresiones ")"       {}

  | new Tipo                {}

  | input        {}
  | input string    {}
  
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
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
--Apuntador :: {}
--------------------------------------------------------------------------------


{

parseError :: [Token] -> a
parseError (h:rs) = 
    error $ "\n\nError sintactico del parser antes de: '" ++ (show h) ++ "\n"
}
