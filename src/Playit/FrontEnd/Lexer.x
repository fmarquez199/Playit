{
{- |
 * Lexical analizer
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.FrontEnd.Lexer (
  MyToken(..),
  LexerResult(..), 
  alexScanTokens,
) where

import           Playit.FrontEnd.Types
import           Playit.Utils
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
}

%wrapper "monadUserState-bytestring"

-- Characters set
$digits    = [0-9]
$abc_minus = [a-z]
$abc_mayus = [A-Z]
$abc       = [a-zA-Z]
$symbols   = [\! \" \# \$ \% \& \' \( \) \* \+ \, \- \. \/ \: \; \< \= \> \? \@
          \[ \\ \] \^ \_ \` \{ \| \} \~ '\0' '\t' '\n' '\\' '\'' '\"' '\~' '\*']
$valids    = [$digits $abc $symbols  $white]
$char_text = [$valids # [\* \~ \\]]
$char_id   = [$digits $abc \_ \']

-- Regular expresions
@scape    = "\\" | "\0" | "\n" | "\t" | "\~" | "\*"
@chars    = $char_text | @scape
@text     = @chars*
@char     = "*" @chars "*"
@strings  = \~ @text \~
@id_type  = $abc_mayus $char_id*
@id       = $abc_minus $char_id*
@programs = \% $char_id+ \%
@endLine  = ($white* \n)+ 
@float    = $digits+ \' $digits+
@comments = \"\' ( . # [\'\"] | \n)* \'\"
@comment  = \@ [. # \n]*

-- token :: (AlexInput -> Int64 -> token) -> AlexAction token
tokens :-

  ([$white # \n])+    ;
  @endLine            { makeToken TkEndLine }

  -- Reserved words
  
  -- Definition of the program begin
  world               { makeToken TkWORLD }
  -- Simple types
  Battle              { makeToken TkBATTLE }
  Power               { makeToken TkPOWER  }
  Skill               { makeToken TkSKILL  }
  Rune                { makeToken TkRUNE   }
  Runes               { makeToken TkRUNES  }
  -- Compund types
  "Kit of"            { makeToken TkKitOf     }
  Inventory           { makeToken TkINVENTORY }
  Items               { makeToken TkITEMS     }
  spawn               { makeToken TkSPAWN     }
  summon              { makeToken TkSUMMON    }
  -- If statement
  Button              { makeToken TkBUTTON     }
  notPressed          { makeToken TkNotPressed }
  -- Subroutines
  kill                { makeToken TkKILL    }
  boss                { makeToken TkBOSS    }
  monster             { makeToken TkMONSTER }
  unlock              { makeToken TkUNLOCK  }
  -- Iterations
  farm                { makeToken TkFARM        }
  dungeon             { makeToken TkDUNGEON     }
  cleared             { makeToken TkCLEARED     }
  gameOver            { makeToken TkGameOver    }
  keepPlaying         { makeToken TkKeepPlaying }
  -- I/O
  joystick            { makeToken TkJOYSTICK }
  drop                { makeToken TkDROP     }
  -- Pointers
  DeathZone           { makeToken TkDeathZone }
  free                { makeToken TkFREE      }
  puff                { makeToken TkPUFF      }

  -- Boolean literals
  Win                 { makeToken TkWIN  }
  Lose                { makeToken TkLOSE }

  -- Ids
  @programs           { makeToken TkProgramName }
  @id                 { makeToken TkID          }
  @id_type            { makeToken TkIDType      }

  -- Characters
  @char               { makeToken TkCHARACTER }
  @strings            { makeToken TkSTRINGS   }
  
  -- Numeric literals
  $digits+            { makeToken TkINT   }
  @float              { makeToken TkFLOAT }

  -- Symbols

  -- End of block
  ".~"                { makeToken TkFIN }
  -- Numeric operators
  "+"                 { makeToken TkADD       }
  "-"                 { makeToken TkMIN       }
  "*"                 { makeToken TkMULT      }
  "/"                 { makeToken TkDIV       }
  "//"                { makeToken TkDivEntera }
  "%"                 { makeToken TkMOD       }
  "level up"          { makeToken TkINCREMENT }
  "use item"          { makeToken TkDECREMENT }
  "#"                 { makeToken TkLEN       }
  -- Comparison operators
  "||"                { makeToken TkOR           }
  "&&"                { makeToken TkAND          }
  "<="                { makeToken TkLessEqual    }
  "<"                 { makeToken TkLessThan     }
  ">="                { makeToken TkGreaterEqual }
  ">"                 { makeToken TkGreaterThan  }
  "=="                { makeToken TkEQUAL        }
  "!="                { makeToken TkNotEqual     }
  "!"                 { makeToken TkNOT          }
  -- Chars operators
  "buff"              { makeToken TkUPPER }
  "debuff"            { makeToken TkLOWER }
  -- Pointers
  "?"                 { makeToken TkREF }
  -- Lists
  "<<"                { makeToken TkOpenList       }
  ">>"                { makeToken TkCloseList      }
  "|>"                { makeToken TkOpenListIndex  }
  "<|"                { makeToken TkCloseListIndex }
  ":"                 { makeToken TkANEXO          }
  "::"                { makeToken TkCONCAT         }
  -- Arrays
  "|}"                { makeToken TkOpenArray       }
  "{|"                { makeToken TkCloseArray      }
  "|)"                { makeToken TkOpenArrayIndex  }
  "(|"                { makeToken TkCloseArrayIndex }
  -- Registers inicialization
  "{"                 { makeToken TkOpenBrackets  }
  "}"                 { makeToken TkCloseBrackets }
  -- Determined iterations
  in                  { makeToken TkIN }
  until               { makeToken TkTO }
  -- Guards
  "|"                 { makeToken TkGUARD }
  -- Assig
  equip               { makeToken TkASSIG }
  -- Exprs
  "("                 { makeToken TkOpenParenthesis  }
  ")"                 { makeToken TkCloseParenthesis }
  ","                 { makeToken TkCOMA             }
  -- Comments
  "\"'"               { makeToken TkOpenComments  }
  "'\""               { makeToken TkCloseComments }
  '@'                 { makeToken TkCOMMENT       }
  @comments           ;
  @comment            ;
  -- Invalid characters
  .                   { lexError }

{
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
--                                Tokens
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

data Token = 
  TkEndLine         |
  TkWORLD           |
  TkBATTLE          | TkPOWER      | TkSKILL | TkRUNE  | TkRUNES  |
  TkKitOf           | TkINVENTORY  | TkITEMS | TkSPAWN | TkSUMMON |
  TkBUTTON          | TkNotPressed |
  TkKILL            | TkBOSS       | TkMONSTER | TkUNLOCK   |
  TkFARM            | TkDUNGEON    | TkCLEARED | TkGameOver | TkKeepPlaying |
  TkJOYSTICK        | TkDROP       |
  TkDeathZone       | TkFREE       | TkPUFF |
  TkWIN             | TkLOSE       |
  TkProgramName     | TkID         | TkIDType |
  TkCHARACTER       | TkSTRINGS    |
  TkINT             | TkFLOAT      |
  TkFIN             |
  TkADD             | TkMIN   | TkMULT | TkDIV | TkDivEntera | TkMOD | TkINCREMENT | TkDECREMENT | TkLEN |
  TkOR              | TkAND   | TkLessEqual | TkLessThan | TkGreaterEqual | TkGreaterThan | TkEQUAL | TkNotEqual | TkNOT |
  TkUPPER           | TkLOWER |
  TkREF             |
  TkOpenList        | TkCloseList     | TkOpenListIndex  | TkCloseListIndex  | TkANEXO | TkCONCAT |
  TkOpenArray       | TkCloseArray    | TkOpenArrayIndex | TkCloseArrayIndex |
  TkOpenBrackets    | TkCloseBrackets |
  TkIN              | TkTO            |
  TkGUARD           |
  TkASSIG           |
  TkOpenParenthesis | TkCloseParenthesis | TkCOMA    |
  TkOpenComments    | TkCloseComments    | TkCOMMENT |
  TkError
  
  deriving (Eq)

instance Show Token where
  show _ = BLC.unpack $ BL.concat [red, italic, bold, underline]

data MyToken = MyToken
  {
    tkA     :: Token,         -- ^ Token abstraction
    tkInput :: BL.ByteString, -- ^ Input readed from source
    tkPos   :: Pos            -- ^ Position of input
  }
  deriving (Eq)

instance Show MyToken where
  show t = BLC.unpack $ BL.concat [(BLC.pack . show $ tkA t), tkInput t, nocolor]
  
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------
--                             Create tokens
-- -----------------------------------------------------------------------------
-- -----------------------------------------------------------------------------

data LexerResult   = LexerResult {errs :: [Error], tokens :: [MyToken]}
type AlexUserState = LexerResult

-- -----------------------------------------------------------------------------
-- AlexAction = AlexInput -> Int64 -> Alex
-- AlexAction = (AlexPosn, Char, ByteString, Int64) -> Int64 -> Either String (AlexState, a)
-- AlexAction = ((AlexPn !Int !Int !Int), Char, ByteString, Int64) -> Int64 -> Either String (AlexState, AlexUserState)
makeToken :: Token -> AlexAction AlexUserState
makeToken token ((AlexPn _ r c), prevChar, inputStr, bytesConsumedSoFar) len = do
  let
    str = BL.take len inputStr
    input = case token of
      TkCHARACTER -> BL.tail $ BL.init str
      TkSTRINGS   -> BL.tail $ BL.init str
      TkFLOAT     -> BL.pack $ BL.head str : BL.head (BLC.pack ".") : BL.last str : []
      TkEndLine   -> BL.empty
      _           -> str
    tk = MyToken token input (r, c)
  -- Add token to state, get + put
  Alex $ 
    \s@AlexState{alex_ust = ust} ->
      Right (s{alex_ust = LexerResult (errs ust) (tk : tokens ust)}, ())
  alexMonadScan

-- -----------------------------------------------------------------------------
lexError :: AlexAction AlexUserState
lexError ((AlexPn _ r c), prevChar, inputStr, bytesConsumedSoFar) len = do
  let
    str = BL.take len inputStr
    err = Error str (r, c)
  Alex $ 
    \s@AlexState{alex_ust = ust} ->
      Right (s{alex_ust = LexerResult (err : errs ust) (tokens ust)}, ())
  alexMonadScan

-- -----------------------------------------------------------------------------
-- This isn't on the documentation
alexEOF :: Alex AlexUserState
alexEOF = Alex $ \s@AlexState{alex_ust = ust} -> Right (s, ust)

alexInitUserState :: AlexUserState
alexInitUserState = LexerResult [] []

alexScanTokens :: BL.ByteString -> LexerResult
alexScanTokens source =
  case runAlex source alexMonadScan of
    Left msg  -> LexerResult [Error (BLC.pack msg) (-1,-1)] [MyToken TkError (BLC.pack "Alex error") (-1,-1)]
    Right ust -> ust
}
