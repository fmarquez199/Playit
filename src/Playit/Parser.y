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
  if                { TkBUTTON _ $$ }
  proc              { TkBOSS _ _ }
  for               { TkCONTROLLER _ _ }
  print             { TkDROP _ $$ }
  else              { TkNotPressed _ _ }
  free              { TkFREE _ _ }
  break             { TkGameOver _ _ }
  input             { TkJOYSTICK _ $$ }
  continue          { TkKeepPlaying _ _ }
  call              { TkKILL _ _ }
  while             { TkLOCK _ _ }
  function          { TkMONSTER _ _ }
  do                { TkPLAY _ $$ }
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
  idtipo            { TkIDTipo _ _ }

  -- Caracteres

  caracter          { TkCARACTER _ _ $$ }
  string            { TkSTRINGS $$ _ }
  
  -- Literares numericos
  
  entero            { TkINT _ _ $$ }
  flotante          { TkFLOAT _ _ $$ }

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
  "++"              { TkINCREMENT _ $$ }
  "--"              { TkDECREMENT _ $$ }
  "<-"              { TkIN  _ $$ }
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
  "?"               { TkREF _ $$ }
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
  "|"               { TkGUARD _ $$ }
  "="               { TkASING _ $$ }
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
%right "#" pointer endLine
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
  : PushScope Definiciones EndLines world programa ":" EndLines Instrucciones EndLines ".~"  PopScope
    { Programa $ reverse $8 }
  | PushScope Definiciones EndLines world programa ":" EndLines ".~" PopScope
    { Programa [] }
  | PushScope world programa ":" EndLines Instrucciones EndLines ".~"  PopScope
    { Programa $ reverse $6 }
  | PushScope world programa ":" EndLines ".~" PopScope
    { Programa [] }


Definiciones :: { SecuenciaInstr }
  : Definiciones EndLines Definicion { $1 ++ $3 }
  | Definicion                       { $1 }


Definicion :: { SecuenciaInstr }
  : DefinirSubrutina PopScope  { $1 }
  | DefinirRegistro PopScope   { $1 }
  | DefinirUnion PopScope      { $1 }


EndLines :: { () }
  : EndLines endLine  {}
  | endLine           {}

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                            Declaraciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Declaraciones :: { SecuenciaInstr }
  : Declaraciones EndLines Declaracion  { $1 ++ $3 }
  | Declaracion                         { $1 }

Declaracion :: { SecuenciaInstr }
  : Tipo Identificadores
    { % let (ids,asigs) = $2 in insertDeclarations (reverse ids) $1 asigs }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                  Identificadores de las declaraciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Identificadores :: { ([(Nombre, Posicion)], SecuenciaInstr) }
  : Identificadores "," Identificador
  { 
    let ((ids, asigs), (id, asig)) = ($1, $3) in (id : ids, asig ++ asigs)
  }
  | Identificador
  { 
    let (id, asigs) = $1 in ([id], asigs)
  }

Identificador :: { ((Nombre, Posicion), SecuenciaInstr) }
  : nombre "=" Expresion  { ((getTk $1,getPos $1), [Asignacion (Var (getTk $1) TDummy) $3]) }
  | nombre                { ((getTk $1,getPos $1), []) }
  -- Para puff (puff ...) var(si es que se permite apuntador de varios apuntadores),
  -- podemos colocar aqui Tipo nombre [= Expr], pero habria que verificar luego 
  -- en la regla Declaracion que todos los tipos sean coherentes?
  | pointer nombre "=" Expresion  { ((getTk $2,getPos $2), [Asignacion (Var (getTk $2) TDummy) $4]) }
  | pointer nombre                { ((getTk $2,getPos $2), []) }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                      Lvalues y Tipos de datos
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- Lvalues, contenedores que identifican a las variables
Lvalue :: { Vars }
  : Lvalue "." nombre           {% crearVarCompIndex $1 (getTk $3) (getPos $3) }
  | Lvalue "|)" Expresion "(|"  {% crearVarIndex $1 $3 (getPos $2)}   -- Indexacion arreglo
  | Lvalue "|>" Expresion "<|"  {% crearVarIndex $1 $3 (getPos $2)}   -- Indexacion lista
  | pointer Lvalue              {% crearDeferenciacion $2 (getPos $1)}
  | pointer "(" Lvalue ")"      {% crearDeferenciacion $3 (getPos $1)}
  | nombre                      {% crearIdvar (getTk $1) (getPos $1) }


-- Tipos de datos
Tipo :: { Tipo }
  : Tipo "|}" Expresion "{|" %prec "|}"   { TArray $3 $1 }
  | list of Tipo     %prec "?"                     { TLista $3 }
  | Tipo pointer                          { TApuntador $1 }
  | "(" Tipo ")"                          { $2 }
  | int                                   { TInt }
  | float                                 { TFloat }
  | bool                                  { TBool }
  | char                                  { TChar }
  | str                                   { TStr }
  | idtipo                                
  {% do
  
    chequearTipo (getTk $1) (getPos $1)
    return $ NuevoTipo (getTk $1)
  } 
  -- | pointer                               { TApuntador TDummy } 

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                        Secuencia de instrucciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Instrucciones :: { SecuenciaInstr }
  : Instrucciones EndLines Instruccion  { $3 : $1 }
  | Instruccion                         { [$1] }

Instruccion :: { Instr }
  : Asignacion            { $1 }
  | Declaracion           { Asignaciones $1 }
  | PushScope Controller PopScope   { $2 }
  | PushScope Play PopScope         { $2 }
  | Button                { $1 }
  | ProcCall              { $1 }
  | EntradaSalida         { $1 }
  | Free                  { $1 }
  | return Expresion      { Return $2 }
  | break {-PopScope-}    { Break    }
  | continue              { Continue }


-------------------------------------------------------------------------------
-- Instruccion de asignacion '='
Asignacion :: { Instr }
  : Lvalue "=" Expresion  {% crearAsignacion $1 $3 $2 }
  | Lvalue "++"
    {%
      let expr = OpBinario Suma (Variables $1 TInt) (Literal (Entero 1) TInt) TInt
      in crearAsignacion $1 expr $2
    }
  | Lvalue "--"
    {%
      let expr = OpBinario Resta (Variables $1 TInt) (Literal (Entero 1) TInt) TInt
      in crearAsignacion $1 expr $2
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Instrucciones de condicionales 'Button', '|' y 'notPressed'
Button :: { Instr }
  : if ":" EndLines Guardias ".~" PopScope { crearIF (reverse $4) $1 }

Guardias :: { [(Expr, SecuenciaInstr)] }
  : Guardias PopScope Guardia  { $3 : $1 }
  | Guardia           { [$1] }

Guardia :: { (Expr, SecuenciaInstr) }
  : "|" Expresion "}" EndLines PushScope Instrucciones EndLines
    { crearGuardiaIF $2 $6 $1 }
  | "|" Expresion "}" PushScope Instrucciones EndLines
    { crearGuardiaIF $2 $5 $1 }
  | "|" else "}" EndLines PushScope Instrucciones EndLines
    { crearGuardiaIF (Literal (Booleano True) TBool) $6 $1 }
  | "|" else "}" PushScope Instrucciones EndLines
    { crearGuardiaIF (Literal (Booleano True) TBool) $5 $1 }

-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
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
      var <- crearIdvar (getTk $1) (getPos $1) 
      return $ crearAsignacion var $3 $2
      return ((getTk $1), $3)
    }
  | Tipo nombre "=" Expresion
    { % do
      let var = Var (getTk $2) $1
      insertDeclarations [(getTk $2, getPos $2)] $1 []
      return $ crearAsignacion var $4 $3
      return ((getTk $2), $4)
    }

InitVar2 :: { (Nombre, Expr) }
  : nombre "<-" Expresion %prec "<-"
    { % do
      var <- crearIdvar (getTk $1) (getPos $1) 
      return $ crearAsignacion var $3 $2
      return ((getTk $1), $3)
    }
  | Tipo nombre "<-" Expresion %prec "<-"
    { % do
      let var = Var (getTk $2) $1
      insertDeclarations [(getTk $2, getPos $2)] $1 []
      return $ crearAsignacion var $4 $3
      return ((getTk $2), $4)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Instruccion de iteracion indeterminada 'play lock'
Play :: { Instr }
  : do ":" EndLines Instrucciones EndLines while Expresion EndLines ".~"
    {
      crearWhile $7 $4 $1
    }
  | do ":" EndLines while Expresion EndLines ".~"
    {
      crearWhile $5 [] $1
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Instrucciones de E/S 'drop' y 'joystick'
EntradaSalida :: { Instr }
  : print Expresiones       { % crearPrint (crearArrLstExpr $2) $1 }
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Instrucciones para liberar la memoria de los apuntadores 'free'
Free :: { Instr }
  : free nombre             { % crearFree (getTk $2) (getPos $2) }
  | free "|}" "{|" nombre   { % crearFree (getTk $4) (getPos $4) }
  | free "<<" ">>" nombre   { % crearFree (getTk $4) (getPos $4) }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Subrutinas
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

DefinirSubrutina :: { SecuenciaInstr }
  : Firma ":" EndLines Instrucciones EndLines ".~"
    { %
      let ((nombre,categoria), params,tipo) = $1
      in definirSubrutina' nombre params $4 categoria
    }
  | Firma ":" EndLines ".~" 
  { %
    let ((nombre,categoria), params, tipo) = $1
    in definirSubrutina' nombre params [] categoria
  }


-------------------------------------------------------------------------------
-- Firma de la subrutina, se agrega antes a la symtab por la recursividad
Firma :: { ((Nombre, Categoria),Int, Tipo) }
  : Nombre PushScope Params
    {($1, $3,TDummy)}
  | Nombre PushScope Params Tipo 
    {% do
        let (nombre,_) = $1
        updateType nombre 1 $4
        return ($1, $3,$4)
    }

-------------------------------------------------------------------------------
-- Nombre de la subrutina, se agrega antes a la symtab por la recursividad
Nombre :: { (Nombre, Categoria) }
  : proc nombre
    { % do
      definirSubrutina (getTk $2) Procedimientos (getPos $2)
      return ((getTk $2), Procedimientos)
    }
  | function nombre
    { % do
      definirSubrutina (getTk $2) Funciones (getPos $2)
      return ((getTk $2), Funciones)
    }
-------------------------------------------------------------------------------
-- Definicion de los parametros de las subrutinas
Params ::{ Int }
  : "(" Parametros ")" { length $2 }
  | "(" ")"            { 0 }

Parametros :: { [Expr] }
  : Parametros "," Parametro  { $3 : $1 }
  | Parametro                 { [$1] }


Parametro :: { Expr }
  : Tipo nombre       { % definirParam (Param (getTk $2) $1 Valor) }
  | Tipo "?" nombre   { % definirParam (Param (getTk $3) $1 Referencia) }



-------------------------------------------------------------------------------
-- Llamada a subrutinas
ProcCall :: { Instr }
  : SubrutinaCall     { ProcCall $1 }

FuncCall :: { Expr }
  : SubrutinaCall     { % crearFuncCall $1 }

SubrutinaCall :: { Subrutina }
  : call nombre "(" PasarParametros ")"
    { % crearSubrutinaCall (getTk $2) (reverse $4) (getPos $2) }
  | call nombre "(" ")"
    { % crearSubrutinaCall (getTk $2) [] (getPos $2) }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Pasaje de los parametros a las subrutinas
PasarParametros :: { Parametros }
  : PasarParametros "," ParametroPasado   { $3 : $1 }
  | ParametroPasado                       { [$1] }

ParametroPasado :: { Expr }
-- TODO: Verificar aqui que el parametro esta definido
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
  : Expresion "+" Expresion            {% crearOpBin Suma $1 $3 TInt TInt TInt (getPos $2)}
  | Expresion "-" Expresion            {% crearOpBin Resta $1 $3 TInt TInt TInt (getPos $2)}
  | Expresion "*" Expresion            {% crearOpBin Multiplicacion $1 $3  TInt TInt TInt (getPos $2)}
  | Expresion "%" Expresion            {% crearOpBin Modulo $1 $3 TInt TInt TInt (getPos $2)}
  | Expresion "/" Expresion            {% crearOpBin Division $1 $3 TInt TInt TInt (getPos $2)}
  | Expresion "//" Expresion           {% crearOpBin DivEntera $1 $3 TInt TInt TInt (getPos $2)}

  | Expresion "&&" Expresion           {% crearOpBin And $1 $3 TBool TBool TBool (getPos $2)}
  | Expresion "||" Expresion           {% crearOpBin Or $1 $3 TBool TBool TBool (getPos $2)}

  | Expresion "==" Expresion           
  {% do
    crearOpBinComparable Igual $1 $3 [TBool] TBool (getPos $2)
  }
  | Expresion "!=" Expresion
  {% do
    crearOpBinComparable Desigual $1 $3 [TBool] TBool (getPos $2)
  }

  | Expresion ">=" Expresion 
  {% do
    crearOpBinComparable MayorIgual $1 $3 [] TBool (getPos $2)
  }
  | Expresion "<=" Expresion    
  {% do
    crearOpBinComparable MenorIgual $1 $3 [] TBool (getPos $2)
  }
  | Expresion ">" Expresion
  {% do
    crearOpBinComparable Mayor $1 $3 [] TBool (getPos $2)
  }
  | Expresion "<" Expresion 
  {% do
    crearOpBinComparable Menor $1 $3 [] TBool (getPos $2)
  }

  | Expresion ":" Expresion %prec ":"  {% crearOpAnexo $1 $3 (getPos $2)}
  | Expresion "::" Expresion           {% crearOpConcat $1 $3 (getPos $2) }
  
  --
  | Expresion "?" Expresion ":" Expresion %prec "?"
    {% crearIfSimple $1 $3 $5 TDummy $2 }
  | FuncCall                     { $1 }
  | "(" Expresion ")"            { $2 }
  | "{" Expresiones "}"          { crearArrLstExpr $2}-- TODO : 
  | "|)" Expresiones "(|"        { crearArrLstExpr $2 } -- TODO : 
  | "<<" Expresiones ">>"        {% crearLista $2 (getPos $1)}
  | "<<"  ">>"                   {% crearLista [] (getPos $1)}
  | new Tipo                     { OpUnario New Null (TApuntador $2) }

  -- Falta la conversión automática de string a tipo de regreso según el rdm
  | input Expresion %prec input  { crearRead $2 $1 } 
  | input
    {
      crearRead (Literal ValorVacio TStr) $1
    }

  -- Operadores unarios
  | "#" Expresion                        {% crearOpLen $2 (getPos $1)}
  | "-" Expresion %prec negativo         {% crearOpUn Negativo $2 TInt TInt (getPos $1)}
  | "!" Expresion                        {% crearOpUn Not $2 TBool TBool (getPos $1)}
  | upperCase Expresion %prec upperCase  {% crearOpUn UpperCase $2 TChar TChar (getPos $1)}
  | lowerCase Expresion %prec lowerCase  {% crearOpUn LowerCase $2 TChar TChar (getPos $1)}
  
  -- Literales
  | true      { Literal (Booleano True) TBool }
  | false     { Literal (Booleano False) TBool }
  | entero    { Literal (Entero $1) TInt }
  | flotante  { Literal (Flotante $1) TFloat }
  | caracter  { Literal (Caracter $1) TChar }
  | string    { Literal (Str $1) TStr }
  | null      { Null }
  | Lvalue    { Variables $1 (typeVar $1) }


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Registros, Unions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Registros
DefinirRegistro :: { SecuenciaInstr }
  : registro idtipo ":" PushScope EndLines Declaraciones EndLines ".~"
    { %
      definirRegistro (getTk $2) $6 (getPos $2)
    }
  | registro idtipo ":" PushScope EndLines ".~"                        
    { %
      definirRegistro (getTk $2) [] (getPos $2)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Uniones
DefinirUnion :: { SecuenciaInstr }
  : union idtipo ":" PushScope EndLines Declaraciones EndLines ".~"
    { %
      definirUnion (getTk $2) $6 (getPos $2)
    }
  | union idtipo ":" PushScope EndLines ".~"                        
    { %
      definirUnion (getTk $2) [] (getPos $2)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                      Empilar y desempilar de alcances
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Se desempila el alcance actual para pasar al anterior
PopScope  ::  { () }
          :   {- Lambda -}        { % popScope }


-- Empila el nuevo alcance al inicio del anterior
PushScope  ::  { () }
              :   {- Lambda -}    { % pushNewScope }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                   Fin de la declaracion de las producciones
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

{
parseError :: [Token] -> MonadSymTab a
parseError [] =  error $ "\n\nPrograma inválido "
parseError (h:t) =  do
    fileName <- ask
    error $ "\n\n" ++ fileName ++ ": error sintactico del parser antes de: '" ++ token ++ "'. " ++
          show pos ++ "\n"
  where
      token = getTk h
      pos = getPos h
}

