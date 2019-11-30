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
import Playit.PromisesHandler

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
  listOf            { TkKitOf _ _ }
  register          { TkINVENTORY _ _ }
  union             { TkITEMS _ _ }
  "."               { TkSPAWN _ _ }
  new               { TkSUMMON _ $$ }
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
  while             { TkLOCK _ _ }
  break             { TkGameOver _ _ }
  continue          { TkKeepPlaying _ _ }
  -- I/O
  input             { TkJOYSTICK _ $$ }
  print             { TkDROP _ $$ }
  -- Pointers
  null              { TkDeathZone _ $$ }
  free              { TkFREE _ _ }
  pointer           { TkPUFF _ _ }

  -- Boolean literals
  true              { TkWIN _ $$ }
  false             { TkLOSE _ $$ }

  -- Ids
  program           { TkProgramName _ _ }
  id                { TkID _ _ }
  idType            { TkIDType _ _ }

  -- Characters
  character         { TkCHARACTER _ _ _ }
  string            { TkSTRINGS _ _ }
  
  -- Numeric literals
  integer           { TkINT _ _ _ }
  floats            { TkFLOAT _ _ _ }

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
  "!"               { TkNOT _ _ }
  upperCase         { TkUPPER _ _ }
  lowerCase         { TkLOWER _ _ }
  "<<"              { TkOpenList _ _ }
  ">>"              { TkCloseList _ $$ }
  "|>"              { TkOpenListIndex _ _ }
  "<|"              { TkCloseListIndex _ _ }
  ":"               { TkANEXO _ $$ }
  "::"              { TkCONCAT _ $$ }
  "|}"              { TkOpenArray _ _ }
  "{|"              { TkCloseArray _ _ }
  "|)"              { TkOpenArrayIndex _ $$ }
  "(|"              { TkCloseArrayIndex _ _ }
  "{"               { TkOpenBrackets _ _ }
  "}"               { TkCloseBrackets _ $$ }
  "<-"              { TkIN  _ _ }
  "->"              { TkTO  _ _ }
  "?"               { TkREF _ _ }
  "|"               { TkGUARD _ _ }
  "="               { TkASSIG _ _ }
  "("               { TkOpenParenthesis _ _ }
  ")"               { TkCloseParenthesis _ _ }
  ","               { TkCOMA _ _ }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                          Reglas de asociatividad
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


%nonassoc id
%left "=" ":" "<-" ")"
%right "."
%left "||"
%left "&&"
%nonassoc "==" "!="
%nonassoc ">" "<" ">=" "<="
%left "+" "-"
%left "*" "/" "//" "%"
%right negativo "!" upperCase lowerCase
%left "++" "|}" "{|" "<<" ">>" "::" "|)" "(|" "|>" "<|"
%left "--"
%right "#" pointer endLine input
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
  : ChekedDefinitions world program ":" EndLines Instructions EndLines ".~" PopScope
    { %
-- Checkeo aquí porque en main también se crean promesas (aunque es más un error de nosotros)
    checkPromises >> program (reverse $6)
    }
  | ChekedDefinitions world program ":" EndLines ".~" PopScope
    { %
      checkPromises >> return (Program [] TVoid)
    }
  | world program ":" EndLines Instructions EndLines ".~"  PopScope
    { %
      checkPromises >> program (reverse $5)
    }
  | world program ":" EndLines ".~" PopScope
      { Program [] TVoid }


ChekedDefinitions :: { () }
  : Definitions EndLines--      { % checkPromises }
  {  {-checkPromises comentado porque en main el usuario peude hacer llamadas a 
  funciones no definidas y estas no se detectarian, a menos que detectemos que estamos en main y cuando se llama a una
  función no se le crea una promesa, pero eso habría que modificar el estado so es un TODO -}}


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

Declarations :: { [([(Type,Id)], InstrSeq)] }
  : Declarations EndLines Declaration  { $3 : $1 }
  | Declaration                        { [$1] }

Declaration :: { ([(Type,Id)], InstrSeq) }
  : Type Identifiers
    { % do
      fileCode <- ask
      let
        (ids,assigs)   = $2
        ids'           = map fst $ reverse ids
        types          = replicate (length ids') $1
        pos            = snd $ head ids
        (assigs', msg) = checkAssigs assigs $1 pos fileCode

      insertDeclarations (reverse ids) $1 assigs

      if null msg then return (zip types ids', assigs')
      else tell [msg] >> return (zip types ids', assigs')
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
      let (e, _) = $3
      in ( (getTk $1,getPos $1), [Assig (Var (getTk $1) TDummy) e TVoid] )
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
Lvalue :: { (Var, Pos) }
  : Lvalue "." id                 { % field $1 (getTk $3) (getPos $3) }
  | Lvalue "|)" Expression "(|"   { % index $1 $3 }
  | Lvalue "|>" Expression "<|"   { % index $1 $3 }
  | pointer Lvalue                { % desref $2 }
  | pointer "(" Lvalue ")"        { % desref $3 }
  | id                            { % var (getTk $1) (getPos $1) }


-- Data types
Type :: { Type }
  : Type "|}" Expression "{|" %prec "|}"  { % tArray $3 $1 }
  | listOf Type  %prec "?"                { TList $2 }
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
  : Asignation                         { $1 }
  | Declaration                        { Assigs (snd $1) TVoid }
  | PushScope Controller PopScope      { $2 }
  | PushScope Play PopScope            { $2 }
  | Button                             { $1 }
  | ProcCall                           { $1 }
  | Out                                { $1 }
  | Free                               { $1 }
  | return Expression                  { Return (fst $2) TVoid } -- TODO: check tipo para regreso de func
  | break                              { Break TVoid }
  | continue                           { Continue TVoid }


-------------------------------------------------------------------------------
Asignation :: { Instr }
  : Lvalue "=" Expression  { % assig $1 $3 }
  | Lvalue "++"
    { %
      let expr = Binary Add (Variable (fst $1) TInt) (Literal (Integer 1) TInt) TInt
      in assig $1 (expr, $2)
    }
  | Lvalue "--"
    { %
      let expr = Binary Minus (Variable (fst $1) TInt) (Literal (Integer 1) TInt) TInt
      in assig $1 (expr, $2)
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
      guard $2 $6
    }
  | "|" Expression "}" PushScope Instructions EndLines
    { %
      guard $2 $5
    }
  | "|" else "}" EndLines PushScope Instructions EndLines
    { %
      return ((Literal (Boolean True) TBool), $6)
    }
  | "|" else "}" PushScope Instructions EndLines
    { %
      return ((Literal (Boolean True) TBool), $5)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Determined iteration
Controller :: { Instr }
 : for InitVar1 "->" Expression ":" EndLines Instructions EndLines ".~"
    { %
      let (varIter, e1) = $2 in for varIter e1 $4 (reverse $7)
    }
 | for InitVar1 "->" Expression while Expression ":" EndLines Instructions EndLines ".~"
    { %
      let (varIter, e1) = $2 in forWhile varIter e1 $4 $6 (reverse $9)
    }
 | for InitVar1 "->" Expression ":" EndLines ".~"
    { %
      let (varIter, e1) = $2 in for varIter e1 $4 []
    }
 | for InitVar1 "->" Expression while Expression ":" EndLines ".~"
    { %
      let (varIter, e1) = $2 in forWhile varIter e1 $4 $6 []
    }
 | for InitVar2 ":" EndLines Instructions EndLines ".~"
    { %
      let (varIter, e1) = $2 in forEach varIter e1 (reverse $5)
    }
 | for InitVar2 ":" EndLines ".~"
    { %
      let (varIter, e1) = $2 in forEach varIter e1 []
    }

-- Add to symbol table the iteration variable with its initial value, before
-- build the instruction tree
InitVar1 :: { (Id, (Expr,Pos) ) }
  : id "=" Expression
    { % do
      (symtab, activeScopes, scope, promises) <- get
      let (e, _) = $3
          id = getTk $1
          t = (\s -> if s == TInt then TInt else TError) $ typeE e
          varInfo = [SymbolInfo t scope IterationVariable []]
          newSymTab = insertSymbols [id] varInfo symtab

      v <- var id (getPos $1)
      put (newSymTab, activeScopes, scope, promises)
      -- return $ assig (v, getPos $1) $3
      return (id, $3)
    }
  | Type id "=" Expression
    { % do
      (symtab, activeScopes, scope, promises) <- get
      let id = getTk $2
          t = if $1 == TInt then TInt else TError
          var = Var id t
          varInfo = [SymbolInfo t scope IterationVariable []]
          newSymTab = insertSymbols [id] varInfo symtab
      
      -- insertDeclarations [(id, getPos $2)] $1 []
      put (newSymTab, activeScopes, scope, promises)
      -- return $ assig (var, getPos $2) $4
      return (id, $4)
    }

InitVar2 :: { (Id, (Expr,Pos) ) }
  : id "<-" Expression %prec "<-"
    { % do
      (symtab, activeScopes, scope, promises) <- get
      let (e, _) = $3
          id = getTk $1
          tID = typeE e
          t = if isArray tID || isList tID then tID else TError
          varInfo = [SymbolInfo t scope IterationVariable []]
          newSymTab = insertSymbols [id] varInfo symtab
      
      v <- var id (getPos $1)
      put (newSymTab, activeScopes, scope, promises)
      -- return $ assig (v, getPos $1) $3
      return (id, $3)
    }
  | Type id "<-" Expression %prec "<-"
    { % do
      (symtab, activeScopes, scope, promises) <- get
      let id = getTk $2
          tE = typeE $ fst $4
          baseTE = baseTypeT tE
          -- isArryList
          t = if isArray tE || isList tE then $1 else TError -- Check tipo base de Expr == Type, y Expr es Array/List
          var = Var id $1
          varInfo = [SymbolInfo $1 scope IterationVariable []]
          newSymTab = insertSymbols [id] varInfo symtab

      -- insertDeclarations [(id, getPos $2)] $1 []
      put (newSymTab, activeScopes, scope, promises)
      return (id, $4)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Indetermined iteration
Play :: { Instr }
  : do ":" EndLines Instructions EndLines while Expression EndLines ".~"
    { %
      while $7 (reverse $4)
    }
  | do ":" EndLines while Expression EndLines ".~"
    { %
      while $5 []
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- 'drop'
Out :: { Instr }
  : print Expressions       { % print' (reverse $2) $1 }
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
      -- TODO: check existe al menos un return
      let (id,category) = $1 in updateExtraInfo id category [AST (reverse $4)]
    }
  | Firma ":" EndLines ".~"   { }


-------------------------------------------------------------------------------
Firma :: { (Id, Category) }
  : Name PushScope Params
  { %
    let (name,category) = $1
    in updateInfoSubroutine name category $3 TVoid >> return $1
  }
  | Name PushScope Params Type 
    { %
      let (name,category) = $1
      in updateInfoSubroutine name category $3 $4 >> return $1
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
  : SubroutineCall                { % procCall $1 }

FuncCall :: { (Expr,Pos) }
  : SubroutineCall                { % funcCall $1 }

SubroutineCall :: { (Subroutine, Pos) }
  : call id "(" Arguments ")"     { % call (getTk $2) (reverse $4) (getPos $2) }
  | call id "(" ")"               { % call (getTk $2) [] (getPos $2) }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Arguments passed to subroutines
Arguments :: { Params }
  : Arguments "," Argument   { $3 : $1 }
  | Argument                 { [$1] }

Argument :: { (Expr,Pos) }
  : Expression               { $1 }
  | "?" Expression           { $2 }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                            Expressions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Expressions::{ [(Expr,Pos)] }
  : Expressions "," Expression           { $3 : $1 }
  | Expression                           { [$1] }

Expression :: { (Expr,Pos) }
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
  | Expression ":" Expression %prec ":"  { % anexo $1 $3 $2 }
  -- e1 && e2 TArray o excluve TList
  | Expression "::" Expression           { % concatLists $1 $3 $2 }
  
  --
  | Expression "?" Expression ":" Expression %prec "?"  { % ifSimple $1 $3 $5 }
  | FuncCall                                            { $1 }
  | "(" Expression ")"                                  { $2 }
 
  -- Registers / Unions initialization
  | idType "{" Expressions "}" { % regUnion (getTk $1, getPos $1) (reverse $3) }
  | idType "{" "}"             { % regUnion (getTk $1, getPos $1) [] } -- By default
  
  | "|)" Expressions "(|"         { % array (reverse $2) $1 }
  | "<<" Expressions ">>"         { % list (reverse $2) $3 }
  | "<<" ">>"                     { % list [] $2 }
  | new Type                      { (Unary New (IdType $2) (TPointer $2), $1) }

  | input Expression %prec input  { % read' $2 }
  | input                         { % read' (Literal EmptyVal TStr, $1) }

  -- Unary operators
  | "#" Expression                        { % unary Length $2 TVoid }
  | "-" Expression %prec negativo         { % unary Negative $2 TVoid }
  | "!" Expression                        { % unary Not $2 TBool }
  | upperCase Expression %prec upperCase  { % unary UpperCase $2 TChar }
  | lowerCase Expression %prec lowerCase  { % unary LowerCase $2 TChar }
  
  -- Literals
  | true      { (Literal (Boolean True) TBool, $1) }
  | false     { (Literal (Boolean False) TBool, $1) }
  | integer   { (Literal (Integer $ getTkInt $1) TInt, getPos $1) }
  | floats    { (Literal (Floatt $ getTkFloat $1) TFloat, getPos $1) }
  | character { (Literal (Character $ getTkChar $1) TChar, getPos $1) }
  | string    { (Literal (Str $ getTk $1) TStr, getPos $1) }
  | null      { (Null, $1) }
  | Lvalue    { let (v,p) = $1 in (Variable v (typeVar v), p) }


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Registers, Unions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
DefineRegister :: { () }
  : Register ":" PushScope EndLines Declarations EndLines ".~"
    { %
      let extraI = [AST (concatMap snd $ reverse $5), Params (concatMap fst $ reverse $5)]
      in updatesDeclarationsCategory $1 >> updateExtraInfo $1 TypeConstructors extraI
    }
  | Register ":" PushScope EndLines ".~" { }

-- Add register name first for recursives registers
Register :: { Id }
  : register idType
    { %
      defineRegUnion (getTk $2) TRegister [] (getPos $2)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
DefineUnion :: { Id }
  : union idType ":" PushScope EndLines Declarations EndLines ".~"  
    { %
      let extraInfo = [AST (concatMap snd $6), Params (concatMap fst $ reverse $6)]
      in defineRegUnion (getTk $2) TUnion extraInfo (getPos $2)
    }
  | union idType ":" PushScope EndLines ".~"                         
    { %
      let extraInfo = [AST [], Params []]
      in defineRegUnion (getTk $2) TUnion extraInfo (getPos $2)
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