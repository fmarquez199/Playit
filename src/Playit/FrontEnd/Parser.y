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

import qualified Control.Monad.Trans.RWS    as RWS
import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Playit.Errors              as E

import qualified Playit.FrontEnd.AST        as AST
import qualified Playit.FrontEnd.Lexer      as Lex
import qualified Playit.FrontEnd.ParserM    as Pars
import qualified Playit.FrontEnd.SymTable   as ST
import qualified Playit.FrontEnd.Syntax     as S
import qualified Playit.FrontEnd.TypeCheck  as TC
-- import qualified Playit.FrontEnd.Utils     as U
}

%name        parse
%tokentype { Lex.Token    }
%error     { Pars.parseError }
%monad     { Pars.ParserM }

%token
  endLine           { Lex.Token Lex.TkEndLine _ _          }

  -- Reserved words
  world             { Lex.Token Lex.TkWORLD _ _            }
  -- Simple types
  bool              { Lex.Token Lex.TkBATTLE _ _           }
  int               { Lex.Token Lex.TkPOWER _ _            }
  float             { Lex.Token Lex.TkSKILL _ _            }
  char              { Lex.Token Lex.TkRUNE _ _             }
  -- Compund types
  str               { Lex.Token Lex.TkRUNES _ _            }
  listOf            { Lex.Token Lex.TkKitOf _ _            }
  record            { Lex.Token Lex.TkINVENTORY _ _        }
  union             { Lex.Token Lex.TkITEMS _ _            }
  "."               { Lex.Token Lex.TkSPAWN _ _            }
  new               { Lex.Token Lex.TkSUMMON _ _           }
  -- Selection
  if                { Lex.Token Lex.TkBUTTON _ _           }
  else              { Lex.Token Lex.TkNotPressed _ _       }
  quest             { Lex.Token Lex.TkQUEST _ _            }
  loot              { Lex.Token Lex.TkLOOT _ _             }
  -- Subroutines
  call              { Lex.Token Lex.TkKILL _ _             }
  proc              { Lex.Token Lex.TkBOSS _ _             }
  func              { Lex.Token Lex.TkMONSTER _ _          }
  return            { Lex.Token Lex.TkUNLOCK _ _           }
  -- Loops
  for               { Lex.Token Lex.TkFARM _ _             }
  do                { Lex.Token Lex.TkDUNGEON _ _          }
  while             { Lex.Token Lex.TkCLEARED _ _          }
  break             { Lex.Token Lex.TkGameOver _ $$        }
  continue          { Lex.Token Lex.TkKeepPlaying _ $$     }
  -- I/O
  input             { Lex.Token Lex.TkJOYSTICK _ _         }
  print             { Lex.Token Lex.TkDROP _ _             }
  -- Pointers
  null              { Lex.Token Lex.TkDeathZone _ _        }
  free              { Lex.Token Lex.TkFREE _ _             }
  deref             { Lex.Token Lex.TkPUFF _ _             }

  -- Boolean literals
  true              { Lex.Token Lex.TkWIN _ _              }
  false             { Lex.Token Lex.TkLOSE _ _             }

  -- Ids
  program           { Lex.Token Lex.TkProgramName _ _      }
  id                { Lex.Token Lex.TkID _ _               }
  tStruct           { Lex.Token Lex.TkTStruct _ _          }

  -- Characters
  character         { Lex.Token Lex.TkCHARACTER _ _        }
  string            { Lex.Token Lex.TkSTRINGS _ _          }
  
  -- Numeric literals
  integer           { Lex.Token Lex.TkINT _ _              }
  floats            { Lex.Token Lex.TkFLOAT _ _            }

  -- Symbols
  ".~"              { Lex.Token Lex.TkFIN _ _              }
  "+"               { Lex.Token Lex.TkADD _ _              }
  "-"               { Lex.Token Lex.TkMIN _ _              }
  "*"               { Lex.Token Lex.TkMULT _ _             }
  "/"               { Lex.Token Lex.TkDIV _ _              }
  "//"              { Lex.Token Lex.TkDivEntera _ _        }
  "%"               { Lex.Token Lex.TkMOD _ _              }
  "++"              { Lex.Token Lex.TkINCREMENT _ _        }
  "--"              { Lex.Token Lex.TkDECREMENT _ _        }
  "#"               { Lex.Token Lex.TkLEN _ _              }
  "||"              { Lex.Token Lex.TkOR _ _               }
  "&&"              { Lex.Token Lex.TkAND _ _              }
  "<="              { Lex.Token Lex.TkLessEqual _ _        }
  "<"               { Lex.Token Lex.TkLessThan _ _         }
  ">="              { Lex.Token Lex.TkGreaterEqual _ _     }
  ">"               { Lex.Token Lex.TkGreaterThan _ _      }
  "=="              { Lex.Token Lex.TkEQUAL _ _            }
  "!="              { Lex.Token Lex.TkNotEqual _ _         }
  "!"               { Lex.Token Lex.TkNOT _ _              }
  upperCase         { Lex.Token Lex.TkUPPER _ _            }
  lowerCase         { Lex.Token Lex.TkLOWER _ _            }
  "<<"              { Lex.Token Lex.TkOpenList _ _         }
  ">>"              { Lex.Token Lex.TkCloseList _ _        }
  "|>"              { Lex.Token Lex.TkOpenListIndex _ _    }
  "<|"              { Lex.Token Lex.TkCloseListIndex _ _   }
  ":"               { Lex.Token Lex.TkANEXO _ _            }
  "::"              { Lex.Token Lex.TkCONCAT _ _           }
  "|}"              { Lex.Token Lex.TkOpenArray _ _        }
  "{|"              { Lex.Token Lex.TkCloseArray _ _       }
  "|)"              { Lex.Token Lex.TkOpenArrayIndex _ _   }
  "(|"              { Lex.Token Lex.TkCloseArrayIndex _ _  }
  "{"               { Lex.Token Lex.TkOpenBrackets _ _     }
  "}"               { Lex.Token Lex.TkCloseBrackets _ _    }
  "<-"              { Lex.Token Lex.TkIN _ _               }
  "->"              { Lex.Token Lex.TkUntil _ _            }
  "?"               { Lex.Token Lex.TkREF _ _              }
  "|"               { Lex.Token Lex.TkGUARD _ _            }
  "="               { Lex.Token Lex.TkASSIG _ _            }
  "("               { Lex.Token Lex.TkOpenParenthesis _ _  }
  ")"               { Lex.Token Lex.TkCloseParenthesis _ _ }
  ","               { Lex.Token Lex.TkCOMA _ _             }

{-
 * -----------------------------------------------------------------------------
 *                               Asociativity rules
 * -----------------------------------------------------------------------------
-}

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
%right "#" deref endLine input
%left loot
%left "?" quest

%%

{-
 * -----------------------------------------------------------------------------
 *                               Rules / Productions
 * -----------------------------------------------------------------------------
-}

-- -----------------------------------------------------------------------------
ProgramWrapper :: { S.Instruction }
  : EndLn PROGRAM EndLn            { $2 }
  | EndLn PROGRAM                  { $2 }
  | PROGRAM EndLn                  { $1 }
  | PROGRAM                        { $1 }

PROGRAM :: { S.Instruction }
    -- Checkeo aquí porque en main también se crean promesas (aunque es más un error de nosotros)
  : ChekedDefs world program ":" EndLn InstrSeq EndLn ".~" PopScope { % AST.nodeProgram $6 }
  | ChekedDefs world program ":" EndLn                ".~" PopScope { % AST.nodeProgram [] }
  |            world program ":" EndLn InstrSeq EndLn ".~" PopScope { % AST.nodeProgram $5 }
  |            world program ":" EndLn                ".~" PopScope { S.Program []         }


-- -----------------------------------------------------------------------------
ChekedDefs :: { () }
  : Definitions EndLn { } -- { % checkPromises }
    {-
      checkPromises comentado porque en main el usuario puede hacer llamadas a 
      funciones no definidas y estas no se detectarian, a menos que detectemos
      que estamos en main y cuando se llama a una función no se le crea una
      promesa, para eso habría que modificar el estado, so es un TODO
    -}
  -- | Definitions       { % RWS.tell ["After definitions must be one at least line break"] }

Definitions :: { () }
  : Definitions EndLn Definition { }
  -- | Definitions Definition       { % RWS.tell ["Definitions must be separated by at least one line break"] }
  | Definition                   { }

Definition :: { () }
  : DefineSubroutine PopScope { }
  | DefineRecord     PopScope { }
  | DefineUnion      PopScope { }


EndLn :: { () }
  : EndLn endLine { }
  | endLine       { }

{-
 * -----------------------------------------------------------------------------
 *                              Instruction sequence
 * -----------------------------------------------------------------------------
-}

InstrSeq :: { S.InstrSeq }
  : InstrSeq EndLn INSTR  { $3 : $1 }
  | INSTR                 { [$1]    }

INSTR :: { S.Instruction }
  : DECLARATION              { S.Decl $1                        }
  | ASSIGNMENT               { $1                               }
  | SELECTION                { $1                               }
  | PushScope WHILE PopScope { $2                               }
  | PushScope FOR PopScope   { $2                               }
  | ProcCall                 { $1                               }
  | OUT                      { $1                               }
  | FREE                     { $1                               }
  | return EXPR              { AST.nodeReturn $2                }
  | break                    { % AST.nodeControlLoop S.Break $1 }
  | continue                 { % AST.nodeControlLoop S.Continue $1 }


-- -----------------------------------------------------------------------------
DECLARATIONS :: { [S.Declaration] }
  : DECLARATIONS EndLn DECLARATION { $3 : $1 }
  | DECLARATION                       { [$1]    }

DECLARATION  :: { S.Declaration }
  : TYPE INITS                        { % AST.nodeDeclaration $1 $2 }

INITS        :: { [S.Initialization] }
  : INITS "," INIT                    { $3 : $1 }
  | INIT                              { [$1]    }

INIT         :: { S.Initialization }
  : ID "=" EXPR                       { AST.nodeIdInit $1 $3           }
  | ID "<-" EXPR                      { AST.nodeIdInit $1 $3           }
  | ID                                { S.Initialization $1 (S.Expression S.EmptyVal S.TDummy (Lex.tkPosn $ S.idTk $1)) }

ID :: { S.Id }
  : id        { {- % ST.getCurrentScope >>= -} S.Id $1 {- >>= return -} }


-- -----------------------------------------------------------------------------
ASSIGNMENT :: { S.Instruction }
  : Lvalue "=" EXPR            { % AST.nodeAssign $1 $3 }
  | Lvalue "++"                { % AST.nodeIncrement $1 }
  | Lvalue "--"                { % AST.nodeDecrement $1 }


-- -----------------------------------------------------------------------------
SELECTION :: { S.Instruction }
  : if ":" EndLn Guards ".~" PopScope { AST.nodeSelection (reverse $4) (Lex.tkPosn $1) }

Guards :: { [S.Guard] }
  : Guards PopScope Guard { $3 : $1 }
  | Guard                 { [$1]    }

Guard :: { S.Guard }
  : "|" EXPR "}" EndLn PushScope InstrSeq EndLn { % AST.nodeGuard $2 (reverse $6) }
  | "|" EXPR "}" PushScope InstrSeq EndLn          { % AST.nodeGuard $2 (reverse $5) }
  | "|" else "}" EndLn PushScope InstrSeq EndLn
    {
      S.Guard (S.Expression (S.Boolean $ BLC.pack "True") S.TBool (Lex.tkPosn $2)) (reverse $6)
    }
  | "|" else "}" PushScope InstrSeq EndLn
    {
      S.Guard (S.Expression (S.Boolean $ BLC.pack "True") S.TBool (Lex.tkPosn $2)) (reverse $5)
    }


-- -----------------------------------------------------------------------------
WHILE :: { S.Instruction }
  : do ":" EndLn InstrSeq EndLn while EXPR EndLn ".~" { % AST.nodeWhile $7 (reverse $4) }
  | do ":" EndLn while EXPR EndLn ".~"                { % AST.nodeWhile $5 []           }


-- -----------------------------------------------------------------------------
FOR :: { S.Instruction }
  : for IterVar "->" EXPR ":" EndLn InstrSeq EndLn ".~" { S.Continue }
    {- %
      let (varIter, e1) = $2 in AST.nodeFor varIter e1 $4 (reverse $7)
    -}
  | for IterVar "->" EXPR ":" EndLn ".~" { S.Continue }
    {- %
      let (varIter, e1) = $2 in AST.nodeFor varIter e1 $4 []
    -}
  | for IterVar "->" EXPR while EXPR ":" EndLn InstrSeq EndLn ".~" { S.Continue }
    {- %
      let (varIter, e1) = $2 in AST.nodeForWhile varIter e1 $4 $6 (reverse $9)
    -}
  | for IterVar "->" EXPR while EXPR ":" EndLn ".~" { S.Continue }
    {- %
      let (varIter, e1) = $2 in AST.nodeForWhile varIter e1 $4 $6 []
    -}
  | ForEach ":" EndLn InstrSeq EndLn ".~" { S.Continue }
    {- %
      let (varIter, e1) = $2 in AST.nodeForEach varIter e1 (reverse $5)
    -}
  | ForEach ":" EndLn ".~" { S.Continue }
    {- %
      let (varIter, e1) = $2 in AST.nodeForEach varIter e1 []
    -}

-- Add to symbol table the iteration variable with its initial value, before build the instruction tree
IterVar :: { (S.Id, S.Expression ) }
  : id "=" EXPR { (S.Id $1, S.Expression S.EmptyVal S.TVoid (Lex.tkPosn $1))}
    {- % do
      state@S.ParserS{symTab = st, currS = s} <- get
      let
        (e, _) = $3
        id     = getTk $1
      v <- var id (getPos $1) -- This have to be here
      let
        t       = (\s -> if s == TInt then TInt else TError) $ typeE e
        varInfo = [SymbolInfo id t s IterationVariable []]
        newST   = ST.insertSymbols [id] varInfo st

      put state{symTab = newST}
      -- return $ assig (v, getPos $1) $3
      return (id, $3)
    -}
  | TYPE id "=" EXPR { (S.Id $2, S.Expression S.EmptyVal S.TVoid (Lex.tkPosn $2))}
    {- % do
      state@S.ParserS{symTab = st, currS = s} <- get
      let
        id      = getTk $2
        t       = if $1 == TInt then TInt else TError
        var     = S.Var id t
        varInfo = [SymbolInfo id t s IterationVariable []]
        newST   = ST.insertSymbols [id] varInfo st
      
      -- insertDeclarations [(id, getPos $2)] $1 []
      put state{symTab = newST}
      -- return $ assig (var, getPos $2) $4
      return (id, $4)
    -}

ForEach :: { (S.Id, S.Expression ) }
  : for id "<-" EXPR %prec "<-" { (S.Id $2, S.Expression S.EmptyVal S.TVoid (Lex.tkPosn $2))}
    {- % do
      state@S.ParserS{symTab = st, currS = s} <- get
      let id = getTk $1
      v <- var id (getPos $1) -- This have to be here
      let
        tE      = typeE (fst $3)
        baseTE  = baseTypeArrLst tE
        arrayL  = isArray tE || isList tE
        t       = if arrayL then baseTE else if tE == TPDummy then TPDummy else TError
        varInfo = [SymbolInfo id t s IterationVariable []]
        newST   = ST.insertSymbols [id] varInfo st

      when (t /= TPDummy) (put state{symTab = newST})
      
      -- return $ assig (v, getPos $1) $3
      return (id, $3)
    -}
  | for TYPE id "<-" EXPR %prec "<-" { (S.Id $3, S.Expression S.EmptyVal S.TVoid (Lex.tkPosn $3))}
    {- % do
      state@S.ParserS{symTab = st, currS = s} <- get
      let
        id      = getTk $2
        tE      = typeE $ fst $4
        matchT  = $1 == (baseTypeArrLst tE)
        arrayL  = isArray tE || isList tE
        t       = if arrayL && matchT then $1 else TError
        var     = S.Var id $1
        varInfo = [SymbolInfo id $1 s IterationVariable []]
        newST   = ST.insertSymbols [id] varInfo st

      -- insertDeclarations [(id, getPos $2)] $1 []
      put state{symTab = newST}
      return (id, $4)
    -}


-- -----------------------------------------------------------------------------
OUT :: { S.Instruction }
  : print EXPRS         { % AST.nodePrint (reverse $2) $1 }


-- -----------------------------------------------------------------------------
{- TODO : Checks para que 
       1 free id sea solo para punteros a una variable
       2 free "|}" "{|" id  sea solo para punteros a arreglos
       3 free "<<" ">>" id  sea solo para punteros a listas -}
FREE :: { S.Instruction }
  : free id             { % AST.nodeFree $2 }
  | free "|}" "{|" id   { % AST.nodeFree $4 }
  | free "<<" ">>" id   { % AST.nodeFree $4 }


{-
 * -----------------------------------------------------------------------------
 *                                Expressions
 * -----------------------------------------------------------------------------
-}

EXPRS :: { [S.Expression] }
  : EXPRS "," EXPR         { $3 : $1 }
  | EXPR                   { [$1]    }

EXPR :: { S.Expression }
  : EXPR "+" EXPR            { % AST.nodeBinary S.Add $1 $3 $2 }
  | EXPR "-" EXPR            { % AST.nodeBinary S.Minus $1 $3 $2 }
  | EXPR "*" EXPR            { % AST.nodeBinary S.Mult $1 $3 $2 }
  | EXPR "/" EXPR            { % AST.nodeBinary S.Div $1 $3 $2 }
  | EXPR "//" EXPR           { % AST.nodeBinary S.IntDiv $1 $3 $2 }
  | EXPR "%" EXPR            { % AST.nodeBinary S.Mod $1 $3 $2 }
  | EXPR "&&" EXPR           { % AST.nodeBinary S.And $1 $3 $2 }
  | EXPR "||" EXPR           { % AST.nodeBinary S.Or $1 $3 $2 }
  | EXPR "==" EXPR           { % AST.nodeBinary S.Eq $1 $3 $2 }
  | EXPR "!=" EXPR           { % AST.nodeBinary S.Neq $1 $3 $2 }
  | EXPR ">=" EXPR           { % AST.nodeBinary S.Gte $1 $3 $2 }
  | EXPR "<=" EXPR           { % AST.nodeBinary S.Lte $1 $3 $2 }
  | EXPR ">" EXPR            { % AST.nodeBinary S.Gt $1 $3 $2 }
  | EXPR "<" EXPR            { % AST.nodeBinary S.Lt $1 $3 $2 }
  | EXPR ":" EXPR %prec ":"  { % AST.nodeAnexo $1 $3 $2 }
  -- e1 && e2 TArray o excluve TList
  | EXPR "::" EXPR           { % AST.nodeConcatLists $1 $3 $2 }
  
  --
  | EXPR quest EXPR loot EXPR %prec "?" { % AST.nodeIfSimple $1 $3 $5 $2 }
  | FuncCall                            { $1 }
  | "(" EXPR ")"                        { $2 }
 
  -- Records / Unions initialization
  | tStruct "{" EXPRS "}"   { % AST.nodeStruct $1 (reverse $3) }
  | tStruct "{" "}"         { % AST.nodeStruct $1 []           } -- By default
  
  | "|)" EXPRS "(|"         { % AST.nodeArray (reverse $2) $1 }
  | "<<" EXPRS ">>"         { % AST.nodeList  (reverse $2) $1 }
  | "<<" ">>"               { % AST.nodeList  [] $1 }
  | new TYPE                { S.Expression (S.Unary S.New S.PtrInit) (S.TPointer $2) (Lex.tkPosn $1) }

  | input EXPR %prec input  { % AST.nodeRead $2 $1                                  }
  | input                   { % AST.nodeRead (S.Expression S.EmptyVal S.TStr (Lex.tkPosn $1)) $1 }

  -- Unary operators
  | "#" EXPR                       { % AST.nodeUnary S.Len       $2 S.TVoid $1 }
  | "-" EXPR       %prec negativo  { % AST.nodeUnary S.Negate    $2 S.TVoid $1 }
  | "!" EXPR                       { % AST.nodeUnary S.Not       $2 S.TBool $1 }
  | upperCase EXPR %prec upperCase { % AST.nodeUnary S.UpperCase $2 S.TChar $1 }
  | lowerCase EXPR %prec lowerCase { % AST.nodeUnary S.LowerCase $2 S.TChar $1 }
  
  -- Literals
  | Lvalue    { S.Expression (S.Rval $1) (S.varType $1)  (Lex.tkPosn . S.idTk $ S.varId $1) }
  | true      { S.Expression (S.Boolean $ Lex.tkInput $1) S.TBool  (Lex.tkPosn $1) }
  | false     { S.Expression (S.Boolean $ Lex.tkInput $1) S.TBool  (Lex.tkPosn $1) }
  | integer   { S.Expression (S.Integer $ Lex.tkInput $1) S.TInt   (Lex.tkPosn $1) }
  | floats    { S.Expression (S.Floatt  $ Lex.tkInput $1) S.TFloat (Lex.tkPosn $1) }
  | character { S.Expression (S.Chr     $ Lex.tkInput $1) S.TChar  (Lex.tkPosn $1) }
  | string    { S.Expression (S.Str     $ Lex.tkInput $1) S.TStr   (Lex.tkPosn $1) }
  | null      { S.Expression  S.Null                      S.TNull  (Lex.tkPosn $1) }


{-
 * -----------------------------------------------------------------------------
 *                              Lvalues and data types
 * -----------------------------------------------------------------------------
-}

Lvalue :: { S.Var }
  : Lvalue "." id          { % AST.nodeField $1 $3 }
  | Lvalue "|)" EXPR "(|"  { % AST.nodeIndex $1 $3 }
  | Lvalue "|>" EXPR "<|"  { % AST.nodeIndex $1 $3 }
  | deref Lvalue         { % AST.nodeDeref $2    }
  | deref "(" Lvalue ")" { % AST.nodeDeref $3    }
  | id                     { % AST.nodeVar $1      }

-- Data types
TYPE :: { S.Type }
  : TYPE "|}" EXPR "{|" %prec "|}" { % AST.nodeTArray $3 $1 }
  | listOf TYPE  %prec "?"         { S.TList $2       }
  | TYPE deref                     { S.TPointer $1    }
  | "(" TYPE ")"                   { $2             }
  | int                            { S.TInt           }
  | float                          { S.TFloat         }
  | bool                           { S.TBool          }
  | char                           { S.TChar          }
  | str                            { S.TStr           }
  | tStruct                        { % AST.nodeTStruct $1 }


{-
 * -----------------------------------------------------------------------------
 *                                Subroutines
 * -----------------------------------------------------------------------------
-}

DefineSubroutine :: { () }
  : Firma ":" EndLn InstrSeq EndLn ".~" { }
    {- %
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
    -}
  | Firma ":" EndLn ".~" { }
    {- %
      let ((id', category, p), tF) = $1
      in
        if category == Procedures then updateExtraInfo id' category [AST []]
        else do
          fileCode <- ask
          tell [errorMsg "This monster doesn't unlock anything, what's the deal in defeat it then?" fileCode p]
          updateExtraInfo id' category [AST []]
    -}


-- -----------------------------------------------------------------------------
Firma :: { ((S.Id, Pars.Category), S.Type) }
  : Name PushScope Params { ($1, S.TVoid) }
    {- %
      let (name, category) = $1
      in updateInfoSubroutine name category $3 TVoid >> return ($1, TVoid)
    -}
  | Name PushScope Params TYPE { ($1, S.TVoid) } 
    {- %
      let (name, category) = $1
      in updateInfoSubroutine name category $3 $4 >> return ($1, $4)
    -}


-- -----------------------------------------------------------------------------
-- Subroutine name, insert first into symbol table because of recursiveness
Name :: { (S.Id, Pars.Category) }
  : proc id { (S.Id $2, Pars.Procedures) }
    {- % do
      ST.defineSubroutine (getTk $2) Procedures (getPos $2)
      return ((getTk $2), Procedures, (getPos $2))
    -}
  | func id { (S.Id $2, Pars.Functions) }
    {- % do
      ST.defineSubroutine (getTk $2) Functions (getPos $2)
      return ((getTk $2), Functions, (getPos $2))
    -}


-------------------------------------------------------------------------------
-- Subroutines parameters definitions
Params ::{ [S.Var] }
  : "(" DefineParams ")"     { $2 }
  | "(" ")"                  { [] }

DefineParams :: { [S.Var] }
  : DefineParams "," Param          { $3 : $1 }
  | Param                           { [$1] }

Param :: { S.Var }
  : TYPE id       { % AST.nodeParameter $2 $1 S.Value }
  | TYPE "?" id   { % AST.nodeParameter $3 $1 S.Reference }


-- -----------------------------------------------------------------------------
-- Subroutines calls
ProcCall :: { S.Instruction }
  : SubroutineCall                { % AST.nodeProcCall $1 }

FuncCall :: { S.Expression }
  : SubroutineCall                { % AST.nodeFuncCall $1 }

SubroutineCall :: { S.Subroutine }
  : call id "(" Arguments ")"     { % AST.nodeCall $2 (reverse $4) }
  | call id "(" ")"               { % AST.nodeCall $2 [] }


-- -----------------------------------------------------------------------------
-- Arguments passed to subroutines
Arguments :: { S.Params }
  : Arguments "," Argument  { $3 : $1 }
  | Argument                { [$1] }

Argument :: { S.Expression }
  : EXPR                    { $1 }
  | "?" EXPR                { $2 }


{-
 * -----------------------------------------------------------------------------
 *                                   Structs
 * -----------------------------------------------------------------------------
-}

-- -----------------------------------------------------------------------------
DefineRecord :: { () }
  : Record ":" PushScope EndLn DECLARATIONS EndLn ".~" { }
    {-- %
      let
        reg    = fst $1
        decls  = reverse $5
        extraI = [AST (concatMap snd decls), Params (concatMap fst decls)]
      in
        updatesDeclarationsCategory reg >> updateExtraInfo reg TypeConstructors extraI
    --}
  | Record ":" PushScope EndLn ".~" { }

-- Add record name first for recursive records
Record :: { S.Id }
  : record tStruct   { % AST.defineTStruct $2 S.TRecord }


-- -----------------------------------------------------------------------------
DefineUnion :: { () }
  : Union ":" PushScope EndLn DECLARATIONS EndLn ".~"  { }
    {-- %
      let
        union  = fst $1
        decls  = reverse $5
        extraI = [AST (concatMap snd decls), Params (concatMap fst decls)]
      in
        updatesDeclarationsCategory union >> updateExtraInfo union TypeConstructors extraI
    --}
  | Union ":" PushScope EndLn ".~" { }


Union :: { S.Id }
  : union tStruct   { % AST.defineTStruct $2 S.TUnion }


{-
 * -----------------------------------------------------------------------------
 *                                  Scopes
 * -----------------------------------------------------------------------------
-}

PopScope  :: { () }
  : {- Lambda -}   { % ST.popScope     }

PushScope :: { () }
  : {- Lambda -}   { % ST.pushNewScope }
