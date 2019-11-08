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
import Playit.AuxFuncs
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
  bool              { TkBATTLE _ _ }
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
  do                { TkPLAY _ _ }
  while             { TkLOCK _ $$ }
  break             { TkGameOver _ _ }
  continue          { TkKeepPlaying _ _ }
  -- I/O
  input             { TkJOYSTICK _ $$ }
  print             { TkDROP _ $$ }
  -- Pointers
  null              { TkDeathZone _ _ }
  free              { TkFREE _ _ }
  pointer           { TkPUFF _ $$ }

  -- Boolean literals
  true              { TkWIN _ _ }
  false             { TkLOSE _ _ }

  -- Ids
  program           { TkProgramName _ _ }
  id                { TkID _ _ }
  idType            { TkIDTipo _ _ }

  -- Characters
  character         { TkCHARACTER _ _ $$ }
  string            { TkSTRINGS $$ _ }
  
  -- Numeric literals
  integer           { TkINT _ _ $$ }
  floats            { TkFLOAT _ _ $$ }

  -- Symbols
  ".~"              { TkFIN _ _ }
  "+"               { TkADD _ $$ }
  "-"               { TkMIN _ $$ }
  "*"               { TkMULT _ $$ }
  "/"               { TkDIV _ $$ }
  "//"              { TkDivEntera _ $$ }
  "%"               { TkMOD _ $$ }
  "++"              { TkINCREMENT _ $$ }
  "--"              { TkDECREMENT _ $$ }
  "#"               { TkLEN _ _ }
  "||"              { TkOR _ $$ }
  "&&"              { TkAND _ $$ }
  "<="              { TkLessEqual _ $$ }
  "<"               { TkLessThan _ $$ }
  ">="              { TkGreaterEqual _ $$ }
  ">"               { TkGreaterThan _ $$ }
  "=="              { TkEQUAL _ $$ }
  "!="              { TkNotEqual _ $$ }
  "!"               { TkNOT _ $$ }
  upperCase         { TkUPPER _ $$ }
  lowerCase         { TkLOWER _ $$ }
  "<<"              { TkOpenList _ _ }
  ">>"              { TkCloseList _ $$ }
  "|>"              { TkOpenListIndex _ $$ }
  "<|"              { TkCloseListIndex _ $$ }
  ":"               { TkANEXO _ $$ }
  "::"              { TkCONCAT _ _}
  "|}"              { TkOpenArray _ _ }
  "{|"              { TkCloseArray _ _ }
  "|)"              { TkOpenArrayIndex _ $$ }
  "(|"              { TkCloseArrayIndex _ $$ }
  "{"               { TkOpenBrackets _ _ }
  "}"               { TkCloseBrackets _ _ }
  "<-"              { TkIN  _ $$ }
  "->"              { TkTO  _ _ }
  "?"               { TkREF _ _ }
  "|"               { TkGUARD _ $$ }
  "="               { TkASSIG _ $$ }
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
    { % do
      let (ids,assigs) = $2
          pos = snd $ head ids
      insertDeclarations (reverse ids) $1 assigs
      checkAssigs assigs $1 pos
    }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                         Declaration's Identifiers
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Identifiers :: { ([(Id, Pos)], InstrSeq) }
  : Identifiers "," Identifier
  { 
    let ((ids,assigs), (id,asig)) = ($1,$3) in (id:ids, asig ++ assigs)
  }
  | Identifier
  { 
    let (id, assigs) = $1 in ([id], assigs)
  }

Identifier :: { ((Id, Pos), InstrSeq) }
  : id "=" Expression
    {
      ( (getTk $1,getPos $1), [Assig (Var (getTk $1) TDummy) $3] )
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
  : Lvalue "." id                         { % field $1 (getTk $3) (getPos $3) }
  | Lvalue "|)" Expression "(|"           { % index $1 $3 $2 $4 }
  | Lvalue "|>" Expression "<|"           { % index $1 $3 $2 $4 }
  | pointer Lvalue                        { % desref $2 $1 }
  | pointer "(" Lvalue ")"                { % desref $3 $1 }
  | id                                    { % var (getTk $1) (getPos $1) }


-- Data types
Type :: { Type }
  : Type "|}" Expression "{|" %prec "|}"  { TArray $3 $1 }
  | list of Type  %prec "?"               { TList $3 }
  | Type pointer                          { TPointer $1 }
  | "(" Type ")"                          { $2 }
  | int                                   { TInt }
  | float                                 { TFloat }
  | bool                                  { TBool }
  | char                                  { TChar }
  | str                                   { TStr }
  | idType                                { % newType (getTk $1) (getPos $1) }

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
  | Declaration                     { Assigs $1 }
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
Asignation :: { Instr }
  : Lvalue "=" Expression  { % assig $1 $3 $2 }
  | Lvalue "++"
    { %
      let expr = Binary Add (Variable $1 TInt) (Literal (Integer 1) TInt) TInt
      in assig $1 expr $2
    }
  | Lvalue "--"
    { %
      let expr = Binary Minus (Variable $1 TInt) (Literal (Integer 1) TInt) TInt
      in assig $1 expr $2
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Selection
Button :: { Instr }
  : if ":" EndLines Guards ".~" PopScope { if' (reverse $4) $1 }

Guards :: { [(Expr, InstrSeq)] }
  : Guards PopScope Guard  { $3 : $1 }
  | Guard                  { [$1] }

Guard :: { (Expr, InstrSeq) }
  : "|" Expression "}" EndLines PushScope Instructions EndLines
    { %
      guard $2 $6 $1
    }
  | "|" Expression "}" PushScope Instructions EndLines
    { %
      guard $2 $5 $1
    }
  | "|" else "}" EndLines PushScope Instructions EndLines
    { %
      guard (Literal (Boolean True) TBool) $6 $1
    }
  | "|" else "}" PushScope Instructions EndLines
    { %
      guard (Literal (Boolean True) TBool) $5 $1
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Determined iteration
-- TODO: Verificar que no se modifique la variable de iteracion
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
      let id = (getTk $1)
      v <- var id (getPos $1)
      return $ assig v $3 $2
      return (id, $3)
    }
  | Type id "=" Expression
    { % do
      let id = (getTk $2)
          var = Var id $1
      insertDeclarations [(id, getPos $2)] $1 []
      return $ assig var $4 $3
      return (id, $4)
    }

InitVar2 :: { (Id, Expr) }
  : id "<-" Expression %prec "<-"
    { % do
      let id = (getTk $1)
      v <- var id (getPos $1)
      return $ assig v $3 $2
      return (id, $3)
    }
  | Type id "<-" Expression %prec "<-"
    { % do
      let id = (getTk $2)
          var = Var id $1
      insertDeclarations [(id, getPos $2)] $1 []
      return $ assig var $4 $3
      return (id, $4)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Indetermined iteration
Play :: { Instr }
  : do ":" EndLines Instructions EndLines while Expression EndLines ".~"
    {
      while $7 (reverse $4) $6
    }
  | do ":" EndLines while Expression EndLines ".~"
    {
      while $5 [] $4
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- 'drop'
-- TODO: Cambiar arrayList por una que haga un type cast auto a runes??? o que el programador lo hags???
Out :: { Instr }
  : print Expressions       { % print' (arrayList $ reverse $2) $1 }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
Free :: { Instr }
  : free id             { % free (getTk $2) (getPos $2) }
  | free "|}" "{|" id   { % free (getTk $4) (getPos $4) }
  | free "<<" ">>" id   { % free (getTk $4) (getPos $4) }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                            Subroutines
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

DefineSubroutine :: { () }
  : Firma ":" EndLines Instructions EndLines ".~"
    { %
      let (id,category) = $1
      in updateExtraInfo id category [AST (reverse $4)]
    }
  | Firma ":" EndLines ".~"   { }


-------------------------------------------------------------------------------
Firma :: { (Id, Category) }
  : Name PushScope Params
  { % do
    let (name,category) = $1
    updateExtraInfo name category [Params (reverse $3)]
    updateType name 1 TVoid
    return $1
  }
  | Name PushScope Params Type 
    { % do
      let (name,category) = $1
      updateExtraInfo name category [Params (reverse $3)]
      updateType name 1 $4
      return $1
    }

-------------------------------------------------------------------------------
-- Subroutine name, insert first into symbol table because of recursiveness
Name :: { (Id, Category) }
  : proc id
    { % do
      defineSubroutine (getTk $2) Procedures (getPos $2)
      return ((getTk $2), Procedures)
    }
  | func id
    { % do
      defineSubroutine (getTk $2) Functions (getPos $2)
      return ((getTk $2), Functions)
    }

-------------------------------------------------------------------------------
-- Subroutines parameters definitions
Params ::{ [(Type,Id)] }
  : "(" DefineParams ")" { $2 }
  | "(" ")"              { [] }

DefineParams :: { [(Type,Id)] }
  : DefineParams "," Param  { $3 : $1 }
  | Param                   { [$1] }

Param :: { (Type,Id) }
  : Type id       { % defineParameter (Param (getTk $2) $1 Value) (getPos $2) }
  | Type "?" id   { % defineParameter (Param (getTk $3) $1 Reference) (getPos $3) }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Subroutines calls
ProcCall :: { Instr }
  : SubroutineCall     { ProcCall (fst $1) }

FuncCall :: { Expr }
  : SubroutineCall     { % funcCall (fst $1) (snd $1) }

SubroutineCall :: { (Subroutine, Pos) }
  : call id "(" Arguments ")"
    { % call (getTk $2) (reverse $4) (getPos $2) }
  | call id "(" ")"
    { % call (getTk $2) [] (getPos $2) }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Arguments passed to subroutines
Arguments :: { Params }
  : Arguments "," Argument   { $3 : $1 }
  | Argument                 { [$1] }

Argument :: { Expr }
  : Expression               { $1 }
  | "?" Expression           { $2 }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                            Expressions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Expressions::{ [Expr] }
  : Expressions "," Expression           { $3 : $1 }
  | Expression                           { [$1] }

Expression :: { Expr }
  : Expression "+" Expression            { % binary Add $1 $3 $2 }
  | Expression "-" Expression            { % binary Minus $1 $3 $2 }
  | Expression "*" Expression            { % binary Mult $1 $3 $2 }
  | Expression "/" Expression            { % binary Division $1 $3 $2 }
  | Expression "//" Expression           { % binary DivEntera $1 $3 $2 }
  | Expression "%" Expression            { % binary Module $1 $3 $2 }
  | Expression "&&" Expression           { % binary And $1 $3 $2 }
  | Expression "||" Expression           { % binary Or $1 $3 $2 }
  | Expression "==" Expression           { % binary Eq $1 $3 $2 }
  | Expression "!=" Expression           { % binary NotEq $1 $3 $2 }
  | Expression ">=" Expression           { % binary GreaterEq $1 $3 $2 }
  | Expression "<=" Expression           { % binary LessEq $1 $3 $2 }
  | Expression ">" Expression            { % binary Greater $1 $3 $2 }
  | Expression "<" Expression            { % binary Less $1 $3 $2 }
  | Expression ":" Expression %prec ":"  { % anexo Anexo $1 $3 $2 }
  -- e1 && e2 TArray o excluve TList
  | Expression "::" Expression           { concatLists Concat $1 $3 }
  
  --
  | Expression "?" Expression ":" Expression %prec "?"
    { %
      ifSimple $1 $3 $5 $4
    }
  | FuncCall               { $1 }
  | "(" Expression ")"     { $2 }
 
  -- Registers / Unions initialization
  -- TODO: cambiar arrayList por una que inicialice los registros / uniones
  | "{" Expressions "}"    { arrayList $ reverse $2 }
  | "{" "}"                { arrayList [] } -- By default
  
  | "|)" Expressions "(|"  { arrayList $ reverse $2 }
  | "<<" Expressions ">>"  { % list (reverse $2) $3 }
  | "<<" ">>"              { % list [] $2 }
  | new Type               { Unary New (IdType $2) (TPointer $2) }

  | input Expression %prec input  { read' $2 $1 }
  | input
    {
      read' (Literal EmptyVal TStr) $1
    }

  -- Unary operators
  | "#" Expression                        { len Length $2 }
  | "-" Expression %prec negativo         { % unary Negative $2 TInt $1{- or TFloat-} }
  | "!" Expression                        { % unary Not $2 TBool $1 }
  | upperCase Expression %prec upperCase  { % unary UpperCase $2 TChar $1 }
  | lowerCase Expression %prec lowerCase  { % unary LowerCase $2 TChar $1 }
  
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
  : Register ":" PushScope EndLines Declarations EndLines ".~"
    { % updatesDeclarationsCategory $1 }
  | Register ":" PushScope EndLines ".~" { }

-- Add register name first for recursives registers
Register :: { Id }
  : register idType
    { %
      defineRegUnion (getTk $2) TRegister (getPos $2)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
DefineUnion :: { Id }
  : union idType ":" PushScope EndLines Declarations EndLines ".~"  
    { %
      defineRegUnion (getTk $2) TUnion (getPos $2)
    }
  | union idType ":" PushScope EndLines ".~"                         
    { %
      defineRegUnion (getTk $2) TUnion (getPos $2)
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
