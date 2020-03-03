{
{- |
 * Grammatical analizer
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.FrontEnd.Parser (parse, error) where

import Control.Monad           (void,when)
import Control.Monad.Trans.RWS
import Data.Maybe              (fromJust)
import Playit.FrontEnd.AST
import Playit.FrontEnd.Utils
import Playit.FrontEnd.CheckAST
import Playit.FrontEnd.Errors
import Playit.FrontEnd.Lexer
import Playit.FrontEnd.Promises.Handler
import Playit.FrontEnd.SymbolTable
import Playit.FrontEnd.Types

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
  if                { TkBUTTON _ _ }
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
  string            { TkSTRINGS _ _ _ }
  
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
  "<<"              { TkOpenList _ $$ }
  ">>"              { TkCloseList _ _ }
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
-------------------------------Grammar Errors----------------------------------
  | ChekedDefinitions world program EndLines Instructions EndLines ".~"  PopScope
    { %
      errorProg "After the program's name you have to put a colon" $5 $3 0
    }
  | ChekedDefinitions world program ":" Instructions EndLines ".~" PopScope
    { %
      errorProg "You have to write your program in a new line" $5 $3 1
    }
  | ChekedDefinitions world program ":" EndLines Instructions ".~" PopScope
    { %
      errorProg "End marker must be at the end, in its own line" $6 $7 0
    }
  -- | ChekedDefinitions world program ":" EndLines Instructions EndLines PopScope
  --   { %
  --     errorProg "You forgot putting '.~' at the end the program" $6 $3 0
  --   }
  | ChekedDefinitions world program EndLines ".~" PopScope
    { %
      errorProg "After the program's name you have to put a colon" [] $3 1
    }
  | ChekedDefinitions world program ":" ".~" PopScope
    { %
      errorProg "Empty Program but end marker must be in its own line" [] $3 0
    }
  -- | ChekedDefinitions world program ":" EndLines PopScope
  --   { %
  --     errorProg "You forgot putting '.~' at the end the program" [] $3 0
  --   }
  | world program EndLines Instructions EndLines ".~"  PopScope
    { %
      errorProg "After the program's name you have to put a colon" $4 $2 1
    }
  | world program ":" Instructions EndLines ".~"  PopScope
    { %
      errorProg "You have to write your program in a new line" $4 $2 1
    }
  | world program ":" EndLines Instructions ".~"  PopScope
    { %
      errorProg "End marker must be at the end, in its own line" $5 $6 0
    }
  -- | world program ":" EndLines Instructions EndLines PopScope
  --   { %
  --     errorProg "You forgot putting '.~' at the end the program" $6 $2 0
  --   }
  | world program EndLines ".~" PopScope
    { %
      errorProg "After the program's name you have to put a colon" [] $2 1
    }
  | world program ":" ".~" PopScope
    { %
      errorProg "Empty Program but end marker must be in its own line" [] $2 1
    }
  -- | world program ":" EndLines PopScope
  --   { %
  --     errorProg "You forgot putting '.~' at the end the program" [] $2 0
  --   }


ChekedDefinitions :: { () }
  : Definitions EndLines      --{ % checkPromises }
  {
    {-
      checkPromises comentado porque en main el usuario puede hacer llamadas a 
      funciones no definidas y estas no se detectarian, a menos que detectemos
      que estamos en main y cuando se llama a una función no se le crea una
      promesa, pero eso habría que modificar el estado so es un TODO
    -}
  }
  | Definitions
    { % tell ["After definitions must be one at least line break"] }

Definitions :: { () }
  : Definitions EndLines Definition { }
  | Definitions Definition
    { %
      tell ["Definitions must be separated by at least one line break"]
    }
  | Definition                      { }


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
  -- | Declarations Declaration ESTO DA SHIFT REDUCE EN LA REGLA DEL INPUT WTF
  --  { % tell ["Declarations must be separated by at least one line break"] }
  | Declaration                        { [$1] }

Declaration :: { ([(Type,Id)], InstrSeq) }
  : Type Identifiers
    { % do
      fileCode <- ask
      let
        (ids,assigs) = $2
        ids'         = map fst $ reverse ids
        pos          = map snd $ reverse ids
        types        = replicate (length ids') $1
        typeId       = zip types ids'

      (assigs',msg) <- checkAssigs assigs $1 fileCode
      initAssigs    <- insertDeclarations (reverse ids) $1 (zip assigs' pos)

      if null msg then return (typeId, initAssigs)
      else tell [msg] >> return (typeId, initAssigs)
    }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                         Declaration's Identifiers
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Identifiers :: { ([(Id, Pos)], [(Instr,Pos)]) }
  : Identifiers "," Identifier
  { 
    let ((ids,assigs), (id,asig)) = ($1,$3) in (id:ids, asig ++ assigs)
  }
  | Identifier
  { 
    let (id, assigs) = $1 in ([id], assigs)
  }

Identifier :: { ((Id, Pos), [(Instr,Pos)]) }
  : id "=" Expression
    {
      let ((e, pE), n, pId)  = ($3, getTk $1, getPos $1)
      in ( (n, pId), [(Assig (Var n TDummy) e TVoid, pE)] )
    }
  | id "<-" Expression
    { % do
      fileCode <- ask
      let ((e, pE), n, pId)  = ($3, getTk $1, getPos $1)
      tell [errorMsg "Did you mean '=' ?" fileCode pId]
      return ((n, pId), [(Assig (Var n TDummy) e TError, pE)])
    }
  | id
    {
      let (n, p, e) = (getTk $1, getPos $1, Literal EmptyVal TDummy)
      in ( (n, p), [(Assig (Var n TDummy) e TVoid, p)] )
    }

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                           Lvalues and data types
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------


-- Lvalues
Lvalue :: { (Var, Pos) }
  : Lvalue "." id                 { % field $1 (getTk $3) (getPos $3) }
  | Lvalue "|)" Expression "(|"   { % indexArray $1 $3 }
  | Lvalue "|>" Expression "<|"   { % indexList $1 $3 }
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
  -- | Instructions Instruction DA SHIFT REDUCE
  -- { % tell ["Instructions must be separated by at least one line break"] }
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
  | Lvalue "++"            { % increDecreVar Add $1 }
  | Lvalue "--"            { % increDecreVar Minus $1 }
-------------------------------Grammar Errors----------------------------------
  | Lvalue "<-" Expression
    { % do
      fileCode <- ask
      let (v, p) = $1
          (e, _) = $3
      tell [errorMsg "Did you mean `=` ?" fileCode p]
      return $ Assig v e TError
    }
  | "++" Lvalue
    { % do
      fileCode <- ask
      let (v, p) = $2
          e      = Binary Add (Variable v TInt) (Literal (Integer 1) TInt) TError
      tell [errorMsg "'++' must be at the right" fileCode p]
      return $ Assig v e TError
    }
  | "--" Lvalue
    { % do
      fileCode <- ask
      let (v, p) = $2
          e      = Binary Minus (Variable v TInt) (Literal (Integer 1) TInt) TError
      tell [errorMsg "'--' must be at the right" fileCode p]
      return $ Assig v e TError
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Selection
Button :: { Instr }
  : if ":" EndLines Guards ".~" PopScope { if' (reverse $4) (getPos $1) }
-------------------------------Grammar Errors----------------------------------
  | if EndLines Guards ".~" PopScope
    { %
      errorIf "After button you have to put a colon" $3 $1
    }
  | if ":" Guards ".~" PopScope
    { %
      errorIf "Guards must be written in its own line" $3 $1
    }
  -- | if ":" EndLines Guards PopScope { % errorIf "You forgot putting '.~' at the end the Button" $4 $1 } DA SHIFT REDUCE

    
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
-------------------------------Grammar Errors----------------------------------
  -- | Expression "}" EndLines PushScope Instructions EndLines
  --   { %
  --     let (l, _) = getPos $2
  --     in errorGuard "Guards must begin with '|'" $1 $5 (l, 1)
  --   } DAN DEMASIADOS SHIFT REDUCE
  -- | "|" Expression EndLines PushScope Instructions EndLines
  --   { %
  --     guard $2 $5
  --   }
  -- | Expression "}" PushScope Instructions EndLines
  --   { %
  --     guard $1 $4
  --   }
  -- | "|" Expression PushScope Instructions EndLines
  --   { %
  --     guard $2 $4
  --   }
  -- | else "}" EndLines PushScope Instructions EndLines
  --   { %
  --     return ((Literal (Boolean True) TBool), $6)
  --   }
  -- | "|" else EndLines PushScope Instructions EndLines
  --   { %
  --     return ((Literal (Boolean True) TBool), $6)
  --   }
  -- | else "}" PushScope Instructions EndLines
  --   { %
  --     return ((Literal (Boolean True) TBool), $5)
  --   }
  -- | "|" else PushScope Instructions EndLines
  --   { %
  --     return ((Literal (Boolean True) TBool), $5)
  --   }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Determined iteration
Controller :: { Instr }
 : for InitVar1 "->" Expression ":" EndLines Instructions EndLines ".~"
    { %
      let (varIter, e1) = $2 in for varIter e1 $4 (reverse $7)
    }
  | for InitVar1 "->" Expression ":" EndLines ".~"
    { %
      let (varIter, e1) = $2 in for varIter e1 $4 []
    }
  | for InitVar1 "->" Expression while Expression ":" EndLines Instructions EndLines ".~"
    { %
      let (varIter, e1) = $2 in forWhile varIter e1 $4 $6 (reverse $9)
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
-------------------------------Grammar Errors----------------------------------
  -- | for InitVar1 Expression ":" EndLines Instructions EndLines ".~"
  --   { %
  --     let (v, (e, _)) = $2 in errorFor "'->'" v e (fst $3) (reverse $6) $1
  --   } DA SHIFT REDUCE
  | for InitVar1 "->" Expression EndLines Instructions EndLines ".~"
    { %
      let (v, (e, _)) = $2 in errorFor "':'" v e (fst $4) (reverse $6) $1
    }
  -- | for InitVar1 "->" Expression ":" Instructions EndLines ".~"
  --   { %
  --     let (v, (e, _)) = $2
  --     in errorFor "a break line before statements" v e (fst $4) (reverse $6) $1
  --   } ESTO DA REDUCE REDUCE
  | for InitVar1 "->" Expression ":" EndLines Instructions ".~"
    { %
      let (v, (e, _)) = $2
      in errorFor "a break line after statements" v e (fst $4) (reverse $7) $1
    }
  -- | for InitVar1 Expression ":" EndLines ".~"
  --   { %
  --     let (v, (e, _)) = $2 in errorFor "'->'" v e (fst $3) [] $1
  --   } DA SHIFT REDUCE
  | for InitVar1 "->" Expression EndLines ".~"
    { %
      let (v, (e, _)) = $2 in errorFor "':'" v e (fst $4) [] $1
    }
  | for InitVar1 "->" Expression ":" ".~"
    { %
      let (v, (e, _)) = $2
      in errorFor "'.~' in its own line" v e (fst $4) [] $1
    }
  -- | for InitVar1 Expression while Expression ":" EndLines Instructions EndLines ".~"
  --   { %
  --     let (v, (e, _)) = $2
  --     in errorForWhile "'->'" v e (fst $4) (fst $6) (reverse $8) $1
  --   } DA SHIFT REDUCE
  | for InitVar1 "->" Expression while Expression EndLines Instructions EndLines ".~"
    { %
      let (v, (e, _)) = $2 in
      errorForWhile "':'" v e (fst $4) (fst $6) (reverse $8) $1
    }
  -- | for InitVar1 "->" Expression while Expression ":" Instructions EndLines ".~"
  --   { %
  --     let ((v, (e, _)), m) = ($2, "a break line before statements")
  --     in errorForWhile m v e (fst $4) (fst $6) (reverse $8) $1
  --   } DA REDUCE REDUCE
  | for InitVar1 "->" Expression while Expression ":" EndLines Instructions ".~"
    { %
      let ((v, (e, _)), m) = ($2, "a break line after statements")
      in errorForWhile m v e (fst $4) (fst $6) (reverse $9) $1
    }
  -- | for InitVar1 Expression while Expression ":" EndLines ".~"
  --   { %
  --     let (v, (e, _)) = $2
  --     in errorForWhile "'->'" v e (fst $4) (fst $6) [] $1
  --   } DA SHIFT REDUCE
  | for InitVar1 "->" Expression while Expression EndLines ".~"
    { %
      let (v, (e, _)) = $2 in errorForWhile "':'" v e (fst $4) (fst $6) [] $1
    }
  | for InitVar1 "->" Expression while Expression ":" ".~"
    { %
      let (v, (e, _)) = $2
      in errorForWhile "'.~' in its own line" v e (fst $4) (fst $6) [] $1
    }
  | for InitVar2 EndLines Instructions EndLines ".~"
    { %
      let (v, (e, _)) = $2 in errorForEach "':'" v e (reverse $4) $1
    }
  | for InitVar2 ":" Instructions EndLines ".~"
    { %
      let (v, (e, _)) = $2
      in errorForEach "a break line before statements" v e (reverse $4) $1
    }
  | for InitVar2 ":" EndLines Instructions ".~"
    { %
      let (v, (e, _)) = $2
      in errorForEach "a break line after statements" v e (reverse $5) $1
    }
  | for InitVar2 EndLines ".~"
    { %
      let (v, (e, _)) = $2 in errorForEach "You miss a ':'" v e [] $1
    }
  | for InitVar2 ":" ".~"
    { %
      let (v, (e, _)) = $2
      in errorForEach "'.~' in its own line" v e [] $1
    }


-- Add to symbol table the iteration variable with its initial value, before
-- build the instruction tree
InitVar1 :: { (Id, (Expr,Pos) ) }
  : id "=" Expression
    { % do
      state@SymTabState{symTab = st, currS = s} <- get
      let
        (e, _) = $3
        id     = getTk $1
      v <- var id (getPos $1) -- This have to be here
      let
        t       = (\s -> if s == TInt then TInt else TError) $ typeE e
        varInfo = [SymbolInfo id t s IterationVariable []]
        newST   = insertSymbols [id] varInfo st

      put state{symTab = newST}
      -- return $ assig (v, getPos $1) $3
      return (id, $3)
    }
  | Type id "=" Expression
    { % do
      state@SymTabState{symTab = st, currS = s} <- get
      let
        id      = getTk $2
        t       = if $1 == TInt then TInt else TError
        var     = Var id t
        varInfo = [SymbolInfo id t s IterationVariable []]
        newST   = insertSymbols [id] varInfo st
      
      -- insertDeclarations [(id, getPos $2)] $1 []
      put state{symTab = newST}
      -- return $ assig (var, getPos $2) $4
      return (id, $4)
    }

InitVar2 :: { (Id, (Expr,Pos) ) }
  : id "<-" Expression %prec "<-"
    { % do
      state@SymTabState{symTab = st, currS = s} <- get
      let id = getTk $1
      v <- var id (getPos $1) -- This have to be here
      let
        tE      = typeE (fst $3)
        baseTE  = baseTypeArrLst tE
        arrayL  = isArray tE || isList tE
        t       = if arrayL then baseTE else if tE == TPDummy then TPDummy else TError
        varInfo = [SymbolInfo id t s IterationVariable []]
        newST   = insertSymbols [id] varInfo st

      when (t /= TPDummy) (put state{symTab = newST})
      
      -- return $ assig (v, getPos $1) $3
      return (id, $3)
    }
  | Type id "<-" Expression %prec "<-"
    { % do
      state@SymTabState{symTab = st, currS = s} <- get
      let
        id      = getTk $2
        tE      = typeE $ fst $4
        matchT  = $1 == (baseTypeArrLst tE)
        arrayL  = isArray tE || isList tE
        t       = if arrayL && matchT then $1 else TError
        var     = Var id $1
        varInfo = [SymbolInfo id $1 s IterationVariable []]
        newST   = insertSymbols [id] varInfo st

      -- insertDeclarations [(id, getPos $2)] $1 []
      put state{symTab = newST}
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
-------------------------------Grammar Errors----------------------------------
  | do EndLines Instructions EndLines while Expression EndLines ".~"
    { %
      let (e, _) = $6 in errorWhile "':'" e (reverse $3) $1
    }
  | do ":" Instructions EndLines while Expression EndLines ".~"
    { %
      let (e, _) = $6
      in errorWhile "a break line before statements" e (reverse $3) $1
    }
  | do ":" EndLines Instructions while Expression EndLines ".~"
    { %
      let (e, _) = $6
      in errorWhile "a break line after statements" e (reverse $4) $1
    }
  -- | do ":" EndLines Instructions EndLines Expression EndLines ".~"
  --   { %
  --     let (e, _) = $6 in errorWhile "lock" e (reverse $4) $1
  --   } DA REDUCE REDUCE
  | do ":" EndLines Instructions EndLines while Expression ".~"
    { %
      let (e, _) = $7
      in errorWhile "'.~' in its own line" e (reverse $4) $1
    }
    | do EndLines while Expression EndLines ".~"
    { %
      let (e, _) = $4 in errorWhile "':'" e [] $1 
    }
  | do ":" while Expression EndLines ".~"
    { %
      let (e, _) = $4 in errorWhile "a break line before unlock" e [] $1 
    }
  -- | do ":" EndLines Expression EndLines ".~"
  --   { %
  --     let (e, _) = $6 in errorWhile "lock" e [] $1
  --   } DA REDUCE REDUCE
  | do ":" EndLines while Expression ".~"
    { %
      let (e, _) = $5 in errorWhile "'.~' in its own line" e [] $1
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- 'drop'
Out :: { Instr }
  : print Expressions       { % print' (reverse $2) $1 }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- TODO : Checks para que 
--        1 free id sea solo para punteros a una variable
--        2 free "|}" "{|" id  sea solo para punteros a arreglos
--        3 free "<<" ">>" id  sea solo para punteros a listas
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
      let ((id', category, p), tF) = $1
          allReturn               = filter isReturn $ concatMap getInstrSeq $4
          rightTypeReturns        = all (== tF) $ map typeReturn allReturn
          func                    = category == Functions
      in
        if (func && (not $ null allReturn) && rightTypeReturns) || not func then
          updateExtraInfo id' category [AST (reverse $4)]
        else do
          fileCode <- ask

          if null allReturn then
            tell [errorMsg "This monster doesn't unlock anything, what's the deal in defeat it then?" fileCode p]
          else
            tell [returnErrorMsg tF fileCode p]

          updateExtraInfo id' category [AST (reverse $4)]
    }
  | Firma ":" EndLines ".~"
    { %
      let ((id', category, p), tF) = $1
      in
        if category == Procedures then updateExtraInfo id' category [AST []]
        else do
          fileCode <- ask
          tell [errorMsg "This monster doesn't unlock anything, what's the deal in defeat it then?" fileCode p]
          updateExtraInfo id' category [AST []]
    }
-------------------------------Grammar Errors----------------------------------
  | Firma EndLines Instructions EndLines ".~"
    { % do
      fileCode <- ask
      let
        ((id, category, p), _) = $1
        msg                    = id ++ "seems like it left ':'"
      
      if category == Functions then
        tell [errorMsg ("This monster " ++ msg) fileCode p]
      else
        tell [errorMsg ("This boss " ++ msg) fileCode p]
    }
  | Firma ":" Instructions EndLines ".~"
    { % do
      fileCode <- ask
      let
        ((id, category, p), _) = $1
        msg = id ++ "seems like it left a break line before statements"
      
      if category == Functions then
        tell [errorMsg ("This monster " ++ msg) fileCode p]
      else
        tell [errorMsg ("This boss " ++ msg) fileCode p]
    }
  | Firma ":" EndLines Instructions ".~"
    { % do
      fileCode <- ask
      let
        ((id, category, p), _) = $1
        msg = id ++ "seems like it left a break line after statements"
      
      if category == Functions then
        tell [errorMsg ("This monster " ++ msg) fileCode p]
      else
        tell [errorMsg ("This boss " ++ msg) fileCode p]
    }


-------------------------------------------------------------------------------
Firma :: { ((Id, Category, Pos), Type) }
  : Name PushScope Params
  { %
    let (name, category, _) = $1
    in updateInfoSubroutine name category $3 TVoid >> return ($1, TVoid)
  }
  | Name PushScope Params Type 
    { %
      let (name, category, _) = $1
      in updateInfoSubroutine name category $3 $4 >> return ($1, $4)
    }

-------------------------------------------------------------------------------
-- Subroutine name, insert first into symbol table because of recursiveness
Name :: { (Id, Category, Pos) }
  : proc id
    { % do
      defineSubroutine (getTk $2) Procedures (getPos $2)
      return ((getTk $2), Procedures, (getPos $2))
    }
  | func id
    { % do
      defineSubroutine (getTk $2) Functions (getPos $2)
      return ((getTk $2), Functions, (getPos $2))
    }

-------------------------------------------------------------------------------
-- Subroutines parameters definitions
Params ::{ [(Type, Id)] }
  : "(" DefineParams ")" { $2 }
  | "(" ")"              { [] }

DefineParams :: { [(Type, Id)] }
  : DefineParams "," Param  { $3 : $1 }
  | Param                   { [$1] }

Param :: { (Type, Id) }
  : Type id       { % defineParameter (Param (getTk $2) $1 Value) (getPos $2) }
  | Type "?" id   { % defineParameter (Param (getTk $3) $1 Reference) (getPos $3) }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Subroutines calls
ProcCall :: { Instr }
  : SubroutineCall                { % procCall $1 }

FuncCall :: { (Expr, Pos) }
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

Argument :: { (Expr, Pos) }
  : Expression               { $1 }
  | "?" Expression           { $2 }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                            Expressions
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Expressions::{ [(Expr, Pos)] }
  : Expressions "," Expression           { $3 : $1 }
  | Expression                           { [$1] }

Expression :: { (Expr, Pos) }
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
  | "<<" Expressions ">>"         { % list (reverse $2) $1 }
  | "<<" ">>"                     { % list [] $1 }
  | new Type                      { (Unary New (IdType $2) (TPointer $2), $1) }

  | input Expression %prec input  { % read' $2 $1 }
  | input                         { % read' (Literal EmptyVal TStr, $1) $1 }

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
  | string    { (Literal (Str $ getTkStr $1) TStr, getPos $1) }
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
      let
        reg    = fst $1
        decls  = reverse $5
        extraI = [AST (concatMap snd decls), Params (concatMap fst decls)]
      in
        updatesDeclarationsCategory reg >> updateExtraInfo reg TypeConstructors extraI
    }
  | Register ":" PushScope EndLines ".~" { }
-------------------------------Grammar Errors----------------------------------
  | Register PushScope EndLines Declarations EndLines ".~"
    { % do
      fileCode <- ask
      tell [errorMsg "This Inventory seems like it left a ':'" fileCode (snd $1)]
    }
  | Register ":" PushScope Declarations EndLines ".~"
    { % do
      fileCode <- ask
      let
        msg = "This Inventory seems like it left a break line before declarations"

      tell [errorMsg msg fileCode (snd $1)]
    }
  -- | Register ":" PushScope Declarations EndLines ".~"
  --   { %
  --     let msg = "Inventory/Items " ++ $1 ++ "seems like it left a "
  --     in tell ["break line after declarations"]
  --   } DA SHIFT REDUCE
  | Register PushScope EndLines ".~"
    { % do
      fileCode <- ask
      tell [errorMsg "This Inventory seems like it left a ':'" fileCode (snd $1)]
    }
  -- | Register ":" PushScope EndLines ".~"
  --   { %
  --     let msg = "Inventory " ++ $1 ++ "seems like it left a "
  --     in tell [msg ++ "break line before declarations"]
  --   } DA SHIFT REDUCE
  | Register ":" PushScope ".~"
    { % do
      fileCode <- ask
      let
        msg = "This Inventory seems like it left a break line before ending"

      tell [errorMsg msg fileCode (snd $1)]
    }


-- Add register name first for recursives registers
Register :: { (Id, Pos) }
  : register idType
    { %
      defineRegUnion (getTk $2) TRegister [] (getPos $2)
    }
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
DefineUnion :: { () }
  : Union ":" PushScope EndLines Declarations EndLines ".~"  
    { %
      let
        union  = fst $1
        decls  = reverse $5
        extraI = [AST (concatMap snd decls), Params (concatMap fst decls)]
      in
        updatesDeclarationsCategory union >> updateExtraInfo union TypeConstructors extraI
    }
  | Union ":" PushScope EndLines ".~" { }
-------------------------------Grammar Errors----------------------------------
  | Union PushScope EndLines Declarations EndLines ".~"
    { % do
      fileCode <- ask
      tell [errorMsg "This Items seems like it left a ':'" fileCode (snd $1)]
    }
  | Union ":" PushScope Declarations EndLines ".~"
    { % do
      fileCode <- ask
      let
        msg = "This Items seems like it left a break line before declarations"

      tell [errorMsg msg fileCode (snd $1)]
    }
  | Union ":" PushScope EndLines Declarations ".~"
    { % do
      fileCode <- ask
      let
        msg = "This Items seems like it left a break line after declarations"

      tell [errorMsg msg fileCode (snd $1)]
    }
  | Union PushScope EndLines ".~"
    { % do
      fileCode <- ask
      tell [errorMsg "This Items seems like it left a ':'" fileCode (snd $1)]
    }
  | Union ":" PushScope ".~"
    { % do
      fileCode <- ask
      let
        msg = "This Items seems like it left a break line before ending"
        
      tell [errorMsg msg fileCode (snd $1)]
    }

Union :: { (Id, Pos) }
  : union idType
    { %
      defineRegUnion (getTk $2) TUnion [] (getPos $2)
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
