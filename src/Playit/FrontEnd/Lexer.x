{
{- |
 * Lexical analizer
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}
module Playit.FrontEnd.Lexer
  ( Token(..)
  , TokenKind(..)
  , LexerResult(..)
  , alexScanTokens
  ) where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import qualified Playit.Errors              as E
import qualified Playit.Utils               as U
}

%wrapper "monadUserState-bytestring"

-- Characters set
$digits    = [0-9]
$abc_minus = [a-z]
$abc_mayus = [A-Z]
$abc       = [a-zA-Z]
$symbols   = [\! \# \$ \% \& \' \( \) \* \+ \, \- \. \/ \: \; \< \= \> \? \@ '"'
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
@tStruct  = $abc_mayus $char_id*
@id       = $abc_minus $char_id*
@programs = \% $char_id+ \%
@endLine  = ($white* \n)+ 
@float    = $digits+ \' $digits+
@comments = \" \' ( . # [\' \"] | \n)* \' \"
-- Close "
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
  -- Assig
  equip               { makeToken TkASSIG }
  -- If statement
  Button              { makeToken TkBUTTON     }
  notPressed          { makeToken TkNotPressed }
  quest               { makeToken TkQUEST      }
  loot                { makeToken TkLOOT       }
  -- Subroutines
  kill                { makeToken TkKILL    }
  boss                { makeToken TkBOSS    }
  monster             { makeToken TkMONSTER }
  unlock              { makeToken TkUNLOCK  }
  -- Loops
  farm                { makeToken TkFARM        }
  dungeon             { makeToken TkDUNGEON     }
  cleared             { makeToken TkCLEARED     }
  gameOver            { makeToken TkGameOver    }
  keepPlaying         { makeToken TkKeepPlaying }
  in                  { makeToken TkIN          }
  until               { makeToken TkUntil       }
  -- I/O
  joystick            { makeToken TkJOYSTICK }
  drop                { makeToken TkDROP     }
  -- Pointers
  DeathZone           { makeToken TkDeathZone }
  free                { makeToken TkFREE      }
  puff                { makeToken TkPUFF      }

  -- Chars operators
  buff                { makeToken TkUPPER     }
  debuff              { makeToken TkLOWER     }

  -- Boolean literals
  Win                 { makeToken TkWIN  }
  Lose                { makeToken TkLOSE }

  -- Ids, define reserved words first
  @programs           { makeToken TkProgramName }
  @id                 { makeToken TkID          }
  @tStruct            { makeToken TkTStruct     }

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
  -- Guards
  "|"                 { makeToken TkGUARD }
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
{-
 * -----------------------------------------------------------------------------
 *                                  Tokens
 * -----------------------------------------------------------------------------
-}

data TokenKind = 
  TkEndLine         |
  TkWORLD           |
  TkBATTLE          | TkPOWER      | TkSKILL   | TkRUNE     | TkRUNES  |
  TkKitOf           | TkINVENTORY  | TkITEMS   | TkSPAWN    | TkSUMMON |
  TkBUTTON          | TkNotPressed | TkQUEST   | TkLOOT     |
  TkKILL            | TkBOSS       | TkMONSTER | TkUNLOCK   |
  TkFARM            | TkDUNGEON    | TkCLEARED | TkGameOver | TkKeepPlaying |
  TkJOYSTICK        | TkDROP       |
  TkDeathZone       | TkFREE       | TkPUFF |
  TkWIN             | TkLOSE       |
  TkProgramName     | TkID         | TkTStruct |
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
  TkIN              | TkUntil            |
  TkGUARD           |
  TkASSIG           |
  TkOpenParenthesis | TkCloseParenthesis | TkCOMA    |
  TkOpenComments    | TkCloseComments    | TkCOMMENT |
  TkError
  
  deriving (Eq, Ord)

instance Show TokenKind where
  show tk = (BLC.unpack . BL.concat) [U.red, U.italic, U.bold, U.underline]

data Token = Token
  { tkToken :: !TokenKind
  , tkInput :: !BL.ByteString -- ^ Input readed from source
  , tkPosn  :: !U.Position    -- ^ Position of input
  } deriving(Ord)

instance Show Token where
  show token = (BLC.unpack . BL.concat)
    [(BLC.pack . show $ tkToken token), tkInput token, U.nocolor]

instance Eq Token where
  tk1 == tk2 = (tkToken tk1 == tkToken tk2) && (tkInput tk1 == tkInput tk2)


{-
 * -----------------------------------------------------------------------------
 *                               Token creation
 * -----------------------------------------------------------------------------
-}

data LexerResult   = LexerResult {lrErrors :: [E.Error], lrTokens :: [Token]}
type AlexUserState = LexerResult


-- -----------------------------------------------------------------------------
-- AlexAction = AlexInput -> Int64 -> Alex
-- AlexAction = (AlexPosn, Char, ByteString, Int64) -> Int64 -> Either String (AlexState, a)
-- AlexAction = ((AlexPn !Int !Int !Int), Char, ByteString, Int64) -> Int64 -> Either String (AlexState, AlexUserState)
makeToken :: TokenKind -> AlexAction AlexUserState
makeToken token ((AlexPn _ r c), prevChar, inputStr, bytesConsumedSoFar) len = do
  let
    str = BL.take len inputStr
    input = case token of
      TkCHARACTER -> BL.tail $ BL.init str
      TkSTRINGS   -> BL.tail $ BL.init str
      TkFLOAT     -> BL.pack $ BL.head str : 46 : BL.last str : [] -- '.' == 46 in Word8
      TkEndLine   -> BL.empty
      _           -> str
    tk = Token token input (r, c)
  -- Add token to state, get + put
  Alex $ 
    \s@AlexState{alex_ust = ust} ->
      Right (s{alex_ust = LexerResult (lrErrors ust) (tk : lrTokens ust)}, ())
  alexMonadScan


-- -----------------------------------------------------------------------------
lexError :: AlexAction AlexUserState
lexError ((AlexPn _ r c), prevChar, inputStr, bytesConsumedSoFar) len = do
  let
    str = BL.take len inputStr
    err = E.Error (BLC.pack "Bug Found") [str] "filename" (r, c)
  Alex $ 
    \s@AlexState{alex_ust = ust} ->
      Right (s{alex_ust = LexerResult (err : lrErrors ust) (lrTokens ust)}, ())
  alexMonadScan


-- -----------------------------------------------------------------------------
alexInitUserState :: AlexUserState
alexInitUserState = LexerResult [] []

alexScanTokens :: String -> BL.ByteString -> LexerResult
alexScanTokens filename source =
  case runAlex source alexMonadScan of
    Right ust -> ust
    Left msg  -> LexerResult [E.Error (BLC.pack msg) [BLC.pack "Alex error"] filename (-1,-1)] 
                             [Token TkError (BLC.pack "Alex error") (-1,-1)]


-- -----------------------------------------------------------------------------
-- This isn't on the documentation
alexEOF :: Alex AlexUserState
alexEOF = Alex $ \s@AlexState{alex_ust = ust} -> Right (s, ust)

}
