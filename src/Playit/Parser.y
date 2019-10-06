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
%nonassoc "++" "--"
%left ":" 
%nonassoc  "="
%right "."
%left "||"
%left "&&"
%nonassoc "==" "!="
%nonassoc ">" "<" ">=" "<="
%left "+" "-" "::"
%left "*" "/" "//" "%"
%right  "!"

%right "<-"
%right upperCase lowerCase 
%nonassoc negativo QSTMRK
%nonassoc PRE_MM PRE_PP 
%right "#" 

%%

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                          Reglas/Producciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--Programa :: {}
{-Bug raro donde los archivos siempre tienen un \n al final chequear-}
Programa :
    EndInstructs world programa ":" endInstr InstruccionesPrincipal fin  endInstr {[$6]}

EndInstructs:
    endInstr {}
    | {-empty-} {}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                        Secuencia de instrucciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

InstruccionesPrincipal:
    InstruccionPrincipal endInstr InstruccionesPrincipal{}
    |endInstr InstruccionesPrincipal{}
    | {-empty-}   {}

InstruccionPrincipal :
   Instruccion {}
  | FunctionCreate       {}

--Instrucciones ::  {SecuenciaInstr}
Instrucciones :  
   Instruccion endInstr  Instrucciones              {}
   | endInstr Instrucciones {}
   | {-empty-}   {}


--Instruccion ::  {Instr} 
Instruccion  : 
  Declaracion       {}

  {-Creacion de tipos definidos por el usuario Registro,Union-}
  | registro nombre ":" EndInstructs Declaraciones fin      {}

  | Controller          {}
  | Play                {}
  | Button              {}
  | Asignacion          {}
  | print Expresiones   {}
  | Free                {}
  | return Expresion    {}
  | break               {}
  | continue            {}
  | Expresion           {}
    

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                            Declaraciones
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--Declaraciones :: {SecuenciaInstr}
Declaraciones :
   Declaracion endInstr  Declaraciones              {}
   | endInstr Declaraciones {}
   | {-empty-}   {}

--Declaracion ::  {SecuenciaInstr}
Declaracion  :
    Tipo VariablesEnDeclaracion        {}

    | DeclaracionTipoDefinidoUsuario {}
    
    | DeclaracionPuntero {}

DeclaracionPuntero:
     TipoPuntero pointer VariableEnDecl {}

TipoPuntero:
    TipoEscalar {}
    | TipoEscalar "|}" "{|" {}
    | TipoLista {}


{-Declaración de REGISTRO-}
DeclaracionTipoDefinidoUsuario:
    nombre nombre {}
    | nombre nombre "=" Expresion    {}
    | nombre nombre "=" "{" Expresiones "}" {} 

--------------------------------------------------------------------------------
--                  Identificadores de las declaraciones
--------------------------------------------------------------------------------

--Identificadores :: 
VariablesEnDeclaracion  : 
    VariableEnDecl                       {}
    | VariablesEnDeclaracion "," VariableEnDecl {}

--Identificador :: {}
VariableEnDecl  : 
    nombre                  {}
  | nombre "=" Expresion    {}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                      Lvalues y Tipos de datos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

TipoEscalar :
    int                         {}
  | float                       {}
  | bool                        {}
  | char                        {}
  | str                         {}


-- Tipos de datos
--Tipo :: {Tipo}
Tipo : 
    TipoEscalar                     {}
  | TipoEscalar "|}" Expresion "{|" {}
  | TipoLista                       {}

 
TipoLista :
  list of TipoEscalar               {}
  | list of TipoLista               {}
    



-- Lvalues, contenedores que identifican a las variables
--Lvalue :: {}
Lvalue  :
    nombre                      {}
  | pointer nombre              {}
  | Lvalue "." Lvalue           {}
  | Lvalue "|}" Expresion "{|"  {}


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
    "|" Expresion "}" Instrucciones     {}
  | "|" else      "}" Instrucciones     {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instruccion de iteracion determinada 'control'
--Controller :: {Instr}
Controller : 
   for InitVarTipoEscalarFor "=" Expresion "->" Expresion ":" Instrucciones fin     {}
 | for InitVarTipoEscalarFor "=" Expresion "->" Expresion while Expresion ":" Instrucciones fin   {}
 | for InitVarTipoCompuestoFor "<-" nombre ":" Instrucciones fin           %prec "<-"      {}



-- Se inserta la variable de iteracion en la tabla de simbolos junto con su
-- valor inicial, antes de construir el arbol de instrucciones del 'for'
--InitVar :: {(Nombre, Expr)}
InitVarTipoEscalarFor  : 
    nombre          {}
    | int nombre    {}

InitVarTipoCompuestoFor: 
    nombre  {}
    | nombre    nombre  {}
    | int       nombre  {}
    | float     nombre  {}
    | bool      nombre  {}
    | char      nombre  {}
    | str       nombre  {}
    
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Instruccion de iteracion indeterminada 'play unlock'
--Play :: {Instr}
Play  : 
    do ":" Instrucciones while Expresion endInstr fin {}
--------------------------------------------------------------------------------
  
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
-- Procedimientos
--Boss :: {}
Boss : 
    proc nombre "(" ListaParametrosFuncionDeclaracion ")" ":" Instrucciones fin   {}
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Funciones
--Monster :: {}
Monster  : 
    function nombre "(" ListaParametrosFuncionDeclaracion ")" TipoRetornoFuncion ":" Instrucciones fin       {}
--------------------------------------------------------------------------------

TipoRetornoFuncion:
    Tipo    {}
    | nombre    {}


--------------------------------------------------------------------------------
-- Parametros de las subrutinas cuando se crean

ListaParametrosFuncionDeclaracion :
     {-no params-}                 {}
    | ParametrosFuncionDeclaracion  {}

--Parametros :: {}
ParametrosFuncionDeclaracion  : 
    ParametroEnFuncionDeclaracion                                       {}
  | ParametroEnFuncionDeclaracion "," ParametrosFuncionDeclaracion     {}


-- Parametro puntual de una subrutina cuando esta es creada
--Parametro :: {} 
ParametroEnFuncionDeclaracion  : 
    Tipo nombre         {}
  | Tipo "?" nombre     {}
  | nombre nombre     {}
  | nombre "?" nombre     {}
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
  | Expresion "?" Expresion ":" Expresion  %prec QSTMRK {}
  | "(" Expresion ")"       {}
  | "|}" Expresiones "{|"   {}
  | "<<" Expresiones ">>"   {}

  | funcCall nombre         {}
  | funcCall nombre "(" Expresiones ")"       {}

  | new Tipo                {}
  | new nombre                {}

  | input        {}
  | input string    {}
  
  -- Operadores unarios
  | "-" Expresion %prec negativo    {}
  | "#" Expresion           {}
  | "!" Expresion           {}
  | upperCase Expresion %prec upperCase  {}
  | lowerCase Expresion %prec lowerCase    {}
    
  {- Notar que solo se permiten ++ y -- para variables por lo que
  --(a + 1) debería dar error pero --(--a) no!
  
    Se puede hacer que la expresion en vez de regresar el valor de a regrese 
  la misma a.
  
    o se puede poner abajo en vez de Expresion Lvalue y --(--a) no sería permitido
  -}
  | Expresion "++"          {}
  | "++" Expresion  %prec PRE_PP        {}
  | Expresion "--"          {}
  | "--" Expresion  %prec PRE_MM{}

  -- Para a = <<>>
  | "<<" ">>"   {}

  
  -- Literales
  | true        {}
  | false       {}
  | entero      {}
  | flotante    {}
  | caracter    {}
  | string      {}
  | null        {}
  | Lvalue      {}


{

parseError :: [Token] -> a
parseError (h:rs) = 
    error $ "\n\nError sintactico del parser antes de: '" ++ (show h) ++ "\n"
}
