{
{- |
 * Grammatical analizer
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.Parser (parse, error) where

import Control.Monad.Trans.RWS
import Playit.SymbolTable
import Playit.CheckAST
import Playit.Errors
import Playit.Lexer
import Playit.Types
import Playit.AST

}

%name parse
%tokentype { Token }
%error     { parseError }
%monad     { MonadSymTab }


%token
  endLine           { TkEndLine _ _}

  -- Reserved words
  world             { TkWORLD _ _ }
  -- Simple types
  bool              { TkBATLE _ _ }
  int               { TkPOWER _ _ }
  float             { TkSKILL _ _ }
  char              { TkRUNE _ _ }
  str               { TkRUNES _ _ }
  -- Compund types
  list              { TkKIT _ _ }
  of                { TkOF _ _ }
  register          { TkINVENTORY _ _ }
  union             { TkITEMS _ _ }
  "."               { TkSPAWN _ _ }
  new               { TkSUMMON _ _ }
  -- If statement
  if                { TkBUTTON _ $$ }
  else              { TkNotPressed _ _ }
  -- Subroutines
  call              { TkKILL _ _ }
  proc              { TkBOSS _ _ }
  func              { TkMONSTER _ _ }
  return            { TkUNLOCK _ _ }
  -- Iterations
  for               { TkCONTROLLER _ _ }
  do                { TkPLAY _ $$ }
  while             { TkLOCK _ _ }
  break             { TkGameOver _ _ }
  continue          { TkKeepPlaying _ _ }
  -- I/O
  input             { TkJOYSTICK _ $$ }
  print             { TkDROP _ $$ }
  -- Pointers
  null              { TkDeathZone _ _ }
  free              { TkFREE _ _ }
  pointer           { TkPUFF _ _ }

  -- Boolean literals
  true              { TkWIN _ _ }
  false             { TkLOSE _ _ }

  -- Ids
  program           { TkProgramName _ _ }
  id                { TkID _ _ }
  idType            { TkIDTipo _ _ }

  -- Characters
  character         { TkCARACTER _ _ $$ }
  string            { TkSTRINGS $$ _ }
  
  -- Numeric literals
  integer           { TkINT _ _ $$ }
  floats            { TkFLOAT _ _ $$ }

  -- Symbols
  ".~"              { TkFIN _ _ }
  "+"               { TkADD _ _ }
  "-"               { TkMIN _ _ }
  "*"               { TkMULT _ _ }
  "/"               { TkDIV _ _ }
  "//"              { TkDivEntera _ _ }
  "%"               { TkMOD _ _ }
  "++"              { TkINCREMENT _ $$ }
  "--"              { TkDECREMENT _ $$ }
  "#"               { TkLEN _ _ }
  "||"              { TkOR _ _ }
  "&&"              { TkAND _ _ }
  "<="              { TkLessEqual _ _ }
  "<"               { TkLessThan _ _ }
  ">="              { TkGreaterEqual _ _ }
  ">"               { TkGreaterThan _ _ }
  "=="              { TkEQUAL _ _ }
  "!="              { TkNotEqual _ _ }
  "!"               { TkNOT _ _ }
  upperCase         { TkUPPER _ _ }
  lowerCase         { TkLOWER _ _ }
  "<<"              { TkOpenList _ _ }
  ">>"              { TkCloseList _ _ }
  "|>"              { TkOpenListIndex _ _ }
  "<|"              { TkCloseListIndex _ _ }
  ":"               { TkANEXO _ _ }
  "::"              { TkCONCAT _ _}
  "|}"              { TkOpenArray _ _ }
  "{|"              { TkCloseArray _ _ }
  "|)"              { TkOpenArrayIndex _ _ }
  "(|"              { TkCloseArrayIndex _ _ }
  "{"               { TkOpenBrackets _ _ }
  "}"               { TkCloseBrackets _ _ }
  "<-"              { TkIN  _ $$ }
  "->"              { TkTO  _ _ }
  "?"               { TkREF _ $$ }
  "|"               { TkGUARD _ $$ }
  "="               { TkASING _ $$ }
  "("               { TkOpenParenthesis _ _ }
  ")"               { TkCloseParenthesis _ _ }
  ","               { TkCOMA _ _ }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                          Reglas de asociatividad
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


%nonassoc id of
%left "=" ":" "<-" ")"
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
--                             Rules / Productions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

ProgramWrapper :: { Instr }
  : EndLines Program EndLines  { $2 }
  | EndLines Program           { $2 }
  | Program EndLines           { $1 }
  | Program                    { $1 }

  
Program :: { Instr }
  : Definitions EndLines world program ":" EndLines Instructions EndLines ".~"  PopScope
    { Program $ reverse $7 }
  | Definitions EndLines world program ":" EndLines ".~" PopScope
    { Program [] }
  | world program ":" EndLines Instructions EndLines ".~"  PopScope
    { Program $ reverse $5 }
  | world program ":" EndLines ".~" PopScope
    { Program [] }


Definitions :: { () }
  : Definitions EndLines Definition { }
  | Definition                       { }


Definition :: { () }
  : DefineSubroutine PopScope { }
  | DefineRegister PopScope   { }
  | DefineUnion PopScope      { }


EndLines :: { () }
  : EndLines endLine  { }
  | endLine           { }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                              Declarations
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Declarations :: { () }
  : Declarations EndLines Declaration  { }
  | Declaration                        { }

Declaration :: { InstrSeq }
  : Type Identifiers
    { % let (ids,asigs) = $2 in insertDeclarations (reverse ids) $1 asigs }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                         Declaration's Identifiers
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Identifiers :: { ([(Id, Pos)], InstrSeq) }
  : Identifiers "," Identifier
  { 
    let ((ids,asigs), (id,asig)) = ($1,$3) in (id:ids, asig ++ asigs)
  }
  | Identifier
  { 
    let (id, asigs) = $1 in ([id], asigs)
  }

Identifier :: { ((Id, Pos), InstrSeq) }
  : id "=" Expression
    {
      ( (getTk $1,getPos $1), [Asing (Var (getTk $1) TDummy) $3] )
    }
  | id
    {
      ( (getTk $1,getPos $1), [] )
    }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Lvalues and data types
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- Lvalues
Lvalue :: { Var }
  : Lvalue "." id                { % crearCampo $1 (getTk $3) (getPos $3) }
  | Lvalue "|)" Expression "(|"  { crearVarIndex $1 $3 }
  | Lvalue "|>" Expression "<|"  { crearVarIndex $1 $3 }
  | pointer Lvalue               { Desref $2 (typeVar $2) }
  | pointer "(" Lvalue ")"       { Desref $3 (typeVar $3) }
  | id                           { % crearIdvar (getTk $1) (getPos $1) }


-- Data types
Type :: { Type }
  : Type "|}" Expression "{|"  %prec "|}"  { TArray $3 $1 }
  | list of Type  %prec "?"                { TList $3 }
  | Type pointer                           { TPointer $1 }
  | "(" Type ")"                           { $2 }
  | int                                    { TInt }
  | float                                  { TFloat }
  | bool                                   { TBool }
  | char                                   { TChar }
  | str                                    { TStr }
  | idType
    { % do
      return $ TNew (getTk $1) 
    }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                          Instructions sequence
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Instructions :: { InstrSeq }
  : Instructions EndLines Instruction  { $3 : $1 }
  | Instruction                        { [$1] }

Instruction :: { Instr }
  : Asignation                      { $1 }
  | Declaration                     { Asings $1 }
  | PushScope Controller PopScope   { $2 }
  | PushScope Play PopScope         { $2 }
  | Button                          { $1 }
  | ProcCall                        { $1 }
  | Out                             { $1 }
  | Free                            { $1 }
  | return Expression               { Return $2 }
  | break                           { Break }
  | continue                        { Continue }


-------------------------------------------------------------------------------
-- '='
Asignation :: { Instr }
  : Lvalue "=" Expression  { crearAsignacion $1 $3 $2 }
  | Lvalue "++"
    {
      let expr = Binary Add (Variable $1 TInt) (Literal (Integer 1) TInt) TInt
      in crearAsignacion $1 expr $2
    }
  | Lvalue "--"
    {
      let expr = Binary Minus (Variable $1 TInt) (Literal (Integer 1) TInt) TInt
      in crearAsignacion $1 expr $2
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Selection
Button :: { Instr }
  : if ":" EndLines Guards ".~" PopScope { crearIF (reverse $4) $1 }

Guards :: { [(Expr, InstrSeq)] }
  : Guards PopScope Guard  { $3 : $1 }
  | Guard                  { [$1] }

Guard :: { (Expr, InstrSeq) }
  : "|" Expression "}" EndLines PushScope Instructions EndLines
    { 
      crearGuardiaIF $2 $6 $1
    }
  | "|" Expression "}" PushScope Instructions EndLines
    { 
      crearGuardiaIF $2 $5 $1
    }
  | "|" else "}" EndLines PushScope Instructions EndLines
    { 
      crearGuardiaIF (Literal (Boolean True) TBool) $6 $1
    }
  | "|" else "}" PushScope Instructions EndLines
    { 
      crearGuardiaIF (Literal (Boolean True) TBool) $5 $1
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Determined iteration
Controller :: { Instr }
 : for InitVar1 "->" Expression ":" EndLines Instructions EndLines ".~"
    {
      let (varIter, e1) = $2 in For varIter e1 $4 (reverse $7)
    }
 | for InitVar1 "->" Expression while Expression ":" EndLines Instructions EndLines ".~"
    {
      let (varIter, e1) = $2 in ForWhile varIter e1 $4 $6 (reverse $9)
    }
 | for InitVar1 "->" Expression ":" EndLines ".~"
    {
      let (varIter, e1) = $2 in For varIter e1 $4 []
    }
 | for InitVar1 "->" Expression while Expression ":" EndLines ".~"
    {
      let (varIter, e1) = $2 in ForWhile varIter e1 $4 $6 []
    }
 | for InitVar2 ":" EndLines Instructions EndLines ".~"
    {
      let (varIter, e1) = $2 in ForEach varIter e1 (reverse $5)
    }
 | for InitVar2 ":" EndLines ".~"
    {
      let (varIter, e1) = $2 in ForEach varIter e1 []
    }

-- Add to symbol table the iteration variable with its initial value, before
-- build the instruction tree
InitVar1 :: { (Id, Expr) }
  : id "=" Expression
    { % do
      var <- crearIdvar (getTk $1) (getPos $1)
      return $ crearAsignacion var $3 $2
      return ((getTk $1), $3)
    }
  | Type id "=" Expression
    { % do
      let var = Var (getTk $2) $1
      insertDeclarations [(getTk $2, getPos $2)] $1 []
      return $ crearAsignacion var $4 $3
      return ((getTk $2), $4)
    }

InitVar2 :: { (Id, Expr) }
  : id "<-" Expression %prec "<-"
    { % do
      var <- crearIdvar (getTk $1) (getPos $1)
      return $ crearAsignacion var $3 $2
      return ((getTk $1), $3)
    }
  | Type id "<-" Expression %prec "<-"
    { % do
      let var = Var (getTk $2) $1
      insertDeclarations [(getTk $2, getPos $2)] $1 []
      return $ crearAsignacion var $4 $3
      return ((getTk $2), $4)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Indetermined iteration
Play :: { Instr }
  : do ":" EndLines Instructions EndLines while Expression EndLines ".~"
    {
      crearWhile $7 (reverse $4) $1
    }
  | do ":" EndLines while Expression EndLines ".~"
    {
      crearWhile $5 [] $1
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- 'drop'
Out :: { Instr }
  : print Expressions       { % crearPrint (crearArrLstExpr $ reverse $2) $1 }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
Free :: { Instr }
  : free id             { % crearFree (getTk $2) (getPos $2) }
  | free "|}" "{|" id   { % crearFree (getTk $4) (getPos $4) }
  | free "<<" ">>" id   { % crearFree (getTk $4) (getPos $4) }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                            Subroutines
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

DefineSubroutine :: { () }
  : Firma ":" EndLines Instructions EndLines ".~"
    { %
      let (id,categoria) = $1
      in definirSubrutina' id (reverse $4) categoria
    }
  | Firma ":" EndLines ".~"   { }


-------------------------------------------------------------------------------
Firma :: { (Id, Category) }
  : Nombre PushScope Params
  { % do
    let (id,cat) = $1
    updateExtraInfo id cat [Params (reverse $3)]
    updateType id 1 TVoid
    return $1
  }
  | Nombre PushScope Params Type 
    { % do
      let (id,cat) = $1
      updateExtraInfo id cat [Params (reverse $3)]
      updateType id 1 $4
      return $1
    }

-------------------------------------------------------------------------------
-- Subroutine name, add first to symbol table because of recursiveness
Nombre :: { (Id, Category) }
  : proc id
    { % do
      definirSubrutina (getTk $2) Procedures (getPos $2)
      return ((getTk $2), Procedures)
    }
  | func id
    { % do
      definirSubrutina (getTk $2) Functions (getPos $2)
      return ((getTk $2), Functions)
    }

-------------------------------------------------------------------------------
-- Subroutines parameters definitions
Params ::{ [Id] }
  : "(" DefineParams ")" { $2 }
  | "(" ")"            { [] }

DefineParams :: { [Id] }
  : DefineParams "," Param  { $3 : $1 }
  | Param                 { [$1] }


Param :: { Id }
  : Type id       { % definirParam (Param (getTk $2) $1 Value) }
  | Type "?" id   { % definirParam (Param (getTk $3) $1 Reference) }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Subroutines calls
ProcCall :: { Instr }
  : SubroutineCall     { ProcCall (fst $1) }

FuncCall :: { Expr }
  : SubroutineCall     { % crearFuncCall (fst $1) (snd $1) }

SubroutineCall :: { (Subroutine, Pos) }
  : call id "(" Arguments ")"
    { % crearSubrutinaCall (getTk $2) (reverse $4) (getPos $2) }
  | call id "(" ")"
    { % crearSubrutinaCall (getTk $2) [] (getPos $2) }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Arguments passed to subroutines
Arguments :: { Params }
  : Arguments "," Argument   { $3 : $1 }
  | Argument                 { [$1] }

Argument :: { Expr }
  : Expression       { $1 }
  | "?" Expression   { $2 }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                            Expressions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Expressions::{ [Expr] }
  : Expressions "," Expression   { $3 : $1 }
  | Expression                   { [$1] }

Expression :: { Expr }
  : Expression "+" Expression            { crearOpBin Add $1 $3 TInt TInt TInt }
  | Expression "-" Expression            { crearOpBin Minus $1 $3 TInt TInt TInt }
  | Expression "*" Expression            { crearOpBin Mult $1 $3  TInt TInt TInt }
  | Expression "%" Expression            { crearOpBin Module $1 $3 TInt TInt TInt }
  | Expression "/" Expression            { crearOpBin Division $1 $3 TInt TInt TInt }
  | Expression "//" Expression           { crearOpBin DivEntera $1 $3 TInt TInt TInt }
  | Expression "&&" Expression           { crearOpBin And $1 $3 TBool TBool TBool }
  | Expression "||" Expression           { crearOpBin Or $1 $3 TBool TBool TBool }
  | Expression "==" Expression           { crearOpBin Eq $1 $3 TInt TInt TBool }
  | Expression "!=" Expression           { crearOpBin NotEq $1 $3 TInt TInt TBool }
  | Expression ">=" Expression           { crearOpBin GreaterEq $1 $3 TInt TInt TBool }
  | Expression "<=" Expression           { crearOpBin LessEq $1 $3 TInt TInt TBool }
  | Expression ">" Expression            { crearOpBin Greater $1 $3 TInt TInt TBool }
  | Expression "<" Expression            { crearOpBin Less $1 $3 TInt TInt TBool }
  | Expression ":" Expression %prec ":"  { crearOpAnexo Anexo $1 $3 }
  | Expression "::" Expression           { crearOpConcat Concat $1 $3 }
  
  --
  | Expression "?" Expression ":" Expression %prec "?"
    {
      crearIfSimple $1 $3 $5 TDummy $2
    }
  | FuncCall               { $1 }
  | "(" Expression ")"     { $2 }
  | "{" Expressions "}"    { crearArrLstExpr $ reverse $2 } -- Inic de Reg/Union
  | "{" "}"                { crearArrLstExpr [] } -- Inic de Reg/Union por default
  | "|)" Expressions "(|"  { crearArrLstExpr $ reverse $2 }
  | "<<" Expressions ">>"  { crearArrLstExpr $ reverse $2 }
  | "<<" ">>"              { crearArrLstExpr [] }
  | new Type               { Unary New (IdType $2) (TPointer $2) }

  -- Falta la conversion automatica de string a tipo de regreso segun el rdm
  | input Expression %prec input  { crearRead $2 $1 }
  | input
    {
      crearRead (Literal EmptyVal TStr) $1
    }

  -- Unary operators
  | "#" Expression                        { crearOpLen Length $2 }
  | "-" Expression %prec negativo         { crearOpUn Negative $2 TInt TInt }
  | "!" Expression                        { crearOpUn Not $2 TBool TBool }
  | upperCase Expression %prec upperCase  { crearOpUn UpperCase $2 TChar TChar }
  | lowerCase Expression %prec lowerCase  { crearOpUn LowerCase $2 TChar TChar }
  
  -- Literals
  | true      { Literal (Boolean True) TBool }
  | false     { Literal (Boolean False) TBool }
  | integer   { Literal (Integer $1) TInt }
  | floats    { Literal (Floatt $1) TFloat }
  | character { Literal (Character $1) TChar }
  | string    { Literal (Str $1) TStr }
  | null      { Null }
  | Lvalue    { Variable $1 (typeVar $1) }


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Registers, Unions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
DefineRegister :: { () }
  : register idType ":" PushScope EndLines Declarations EndLines ".~"   
    { %
      definirRegistro (getTk $2) (getPos $2)
    }
  | register idType ":" PushScope EndLines ".~"                          
    { %
      definirRegistro (getTk $2) (getPos $2)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
DefineUnion :: { () }
  : union idType ":" PushScope EndLines Declarations EndLines ".~"  
    { %
      definirUnion (getTk $2) (getPos $2)
    }
  | union idType ":" PushScope EndLines ".~"                         
    { %
      definirUnion (getTk $2) (getPos $2)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Push / Pop scope
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

PopScope  ::  { () }
          :   {- Lambda -}    { % popScope }


PushScope ::  { () }
          :   {- Lambda -}    { % pushNewScope }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                       End of rules / productions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
