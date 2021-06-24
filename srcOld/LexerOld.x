{
{- |
 * Lexical analizer
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.FrontEnd.LexerOld (
    Token(..),
    -- AlexPosn(..), 
    alexScanTokens,
) where

import Playit.FrontEnd.Types
}

%wrapper "posn"

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
@error    = .

tokens :-

  ([$white # \n])+    ;
  @endLine            { tok (\(AlexPn _ f c) tk -> TkEndLine tk (f,c)) }

  -- Reserved words
  
  -- Definition of the program begin
  world               { tok (\(AlexPn _ f c) tk -> TkWORLD tk (f,c)) }
  -- Simple types
  Battle              { tok (\(AlexPn _ f c) tk -> TkBATTLE tk (f,c)) }
  Power               { tok (\(AlexPn _ f c) tk -> TkPOWER tk (f,c)) }
  Skill               { tok (\(AlexPn _ f c) tk -> TkSKILL tk (f,c)) }
  Rune                { tok (\(AlexPn _ f c) tk -> TkRUNE tk (f,c)) }
  Runes               { tok (\(AlexPn _ f c) tk -> TkRUNES tk (f,c)) }
  -- Compund types
  "Kit of"            { tok (\(AlexPn _ f c) tk -> TkKitOf tk (f,c)) }
  Inventory           { tok (\(AlexPn _ f c) tk -> TkINVENTORY tk (f,c)) }
  Items               { tok (\(AlexPn _ f c) tk -> TkITEMS tk (f,c)) }
  spawn               { tok (\(AlexPn _ f c) tk -> TkSPAWN tk (f,c)) }
  summon              { tok (\(AlexPn _ f c) tk -> TkSUMMON tk (f,c)) }
  -- If statement
  Button              { tok (\(AlexPn _ f c) tk -> TkBUTTON tk (f,c)) }
  notPressed          { tok (\(AlexPn _ f c) tk -> TkNotPressed tk (f,c)) }
  -- Subroutines
  kill                { tok (\(AlexPn _ f c) tk -> TkKILL tk (f,c)) }
  boss                { tok (\(AlexPn _ f c) tk -> TkBOSS tk (f,c)) }
  monster             { tok (\(AlexPn _ f c) tk -> TkMONSTER tk (f,c)) }
  unlock              { tok (\(AlexPn _ f c) tk -> TkUNLOCK tk (f,c)) }
  -- Iterations
  controller          { tok (\(AlexPn _ f c) tk -> TkCONTROLLER tk (f,c)) }
  play                { tok (\(AlexPn _ f c) tk -> TkPLAY tk (f,c)) }
  lock                { tok (\(AlexPn _ f c) tk -> TkLOCK tk (f,c)) }
  gameOver            { tok (\(AlexPn _ f c) tk -> TkGameOver tk (f,c)) }
  keepPlaying         { tok (\(AlexPn _ f c) tk -> TkKeepPlaying tk (f,c)) }
  -- I/O
  joystick            { tok (\(AlexPn _ f c) tk -> TkJOYSTICK tk (f,c)) }
  drop                { tok (\(AlexPn _ f c) tk -> TkDROP tk (f,c)) }
  -- Pointers
  DeathZone           { tok (\(AlexPn _ f c) tk -> TkDeathZone tk (f,c)) }
  free                { tok (\(AlexPn _ f c) tk -> TkFREE tk (f,c)) }
  puff                { tok (\(AlexPn _ f c) tk -> TkPUFF tk (f,c)) }

  -- Boolean literals
  Win                 { tok (\(AlexPn _ f c) tk -> TkWIN tk (f,c)) }
  Lose                { tok (\(AlexPn _ f c) tk -> TkLOSE tk (f,c)) }

  -- Ids
  @programs           { tok (\(AlexPn _ f c) tk -> TkProgramName tk (f,c)) }
  @id                 { tok (\(AlexPn _ f c) tk -> TkID tk (f,c)) }
  @id_type            { tok (\(AlexPn _ f c) tk -> TkIDType tk (f,c)) }

  -- Characters
  @char               { createTkCHARACTER }
  @strings            { createTkSTRING }
  
  -- Numeric literals
  $digits+            { createTkINT }
  @float              { createTkFLOAT }

  -- Symbols

  -- End of block
  ".~"                { tok (\(AlexPn _ f c) tk -> TkFIN tk (f,c)) }
  -- Numeric operators
  "+"                 { tok (\(AlexPn _ f c) tk -> TkADD tk (f,c)) }
  "-"                 { tok (\(AlexPn _ f c) tk -> TkMIN tk (f,c)) }
  "*"                 { tok (\(AlexPn _ f c) tk -> TkMULT tk (f,c)) }
  "/"                 { tok (\(AlexPn _ f c) tk -> TkDIV tk (f,c)) }
  "//"                { tok (\(AlexPn _ f c) tk -> TkDivEntera tk (f,c)) }
  "%"                 { tok (\(AlexPn _ f c) tk -> TkMOD tk (f,c)) }
  "++"                { tok (\(AlexPn _ f c) tk -> TkINCREMENT tk (f,c)) }
  "--"                { tok (\(AlexPn _ f c) tk -> TkDECREMENT tk (f,c)) }
  "#"                 { tok (\(AlexPn _ f c) tk -> TkLEN tk (f,c)) }
  -- Comparison operators
  "||"                { tok (\(AlexPn _ f c) tk -> TkOR tk (f,c)) }
  "&&"                { tok (\(AlexPn _ f c) tk -> TkAND tk (f,c)) }
  "<="                { tok (\(AlexPn _ f c) tk -> TkLessEqual tk (f,c)) }
  "<"                 { tok (\(AlexPn _ f c) tk -> TkLessThan tk (f,c)) }
  ">="                { tok (\(AlexPn _ f c) tk -> TkGreaterEqual tk (f,c)) }
  ">"                 { tok (\(AlexPn _ f c) tk -> TkGreaterThan tk (f,c)) }
  "=="                { tok (\(AlexPn _ f c) tk -> TkEQUAL tk (f,c)) }
  "!="                { tok (\(AlexPn _ f c) tk -> TkNotEqual tk (f,c)) }
  "!"                 { tok (\(AlexPn _ f c) tk -> TkNOT tk (f,c)) }
  -- Chars operators
  "^"                 { tok (\(AlexPn _ f c) tk -> TkUPPER tk (f,c)) }
  "."                 { tok (\(AlexPn _ f c) tk -> TkLOWER tk (f,c)) }
  -- Pointers
  "?"                 { tok (\(AlexPn _ f c) tk -> TkREF tk (f,c)) }
  -- Lists
  "<<"                { tok (\(AlexPn _ f c) tk -> TkOpenList tk (f,c)) }
  ">>"                { tok (\(AlexPn _ f c) tk -> TkCloseList tk (f,c)) }
  "|>"                { tok (\(AlexPn _ f c) tk -> TkOpenListIndex tk (f,c)) }
  "<|"                { tok (\(AlexPn _ f c) tk -> TkCloseListIndex tk (f,c)) }
  ":"                 { tok (\(AlexPn _ f c) tk -> TkANEXO tk (f,c)) }
  "::"                { tok (\(AlexPn _ f c) tk -> TkCONCAT tk (f,c)) }
  -- Arrays
  "|}"                { tok (\(AlexPn _ f c) tk -> TkOpenArray tk (f,c)) }
  "{|"                { tok (\(AlexPn _ f c) tk -> TkCloseArray tk (f,c)) }
  "|)"                { tok (\(AlexPn _ f c) tk -> TkOpenArrayIndex tk (f,c)) }
  "(|"                { tok (\(AlexPn _ f c) tk -> TkCloseArrayIndex tk (f,c)) }
  -- Registers inicialization
  "{"                 { tok (\(AlexPn _ f c) tk -> TkOpenBrackets tk (f,c)) }
  "}"                 { tok (\(AlexPn _ f c) tk -> TkCloseBrackets tk (f,c)) }
  -- Determined iterations
  "<-"                { tok (\(AlexPn _ f c) tk -> TkIN tk (f,c)) }
  "->"                { tok (\(AlexPn _ f c) tk -> TkTO tk (f,c)) }
  -- Guards
  "|"                 { tok (\(AlexPn _ f c) tk -> TkGUARD tk (f,c)) }
  -- Assig
  "="                 { tok (\(AlexPn _ f c) tk -> TkASSIG tk (f,c)) }
  -- Exprs
  "("                 { tok (\(AlexPn _ f c) tk -> TkOpenParenthesis tk (f,c)) }
  ")"                 { tok (\(AlexPn _ f c) tk -> TkCloseParenthesis tk (f,c)) }
  ","                 { tok (\(AlexPn _ f c) tk -> TkCOMA tk (f,c)) }
  -- Comments
  "\"'"               { tok (\(AlexPn _ f c) tk -> TkOpenComments tk (f,c)) }
  "'\""               { tok (\(AlexPn _ f c) tk -> TkCloseComments tk (f,c)) }
  '@'                 { tok (\(AlexPn _ f c) tk -> TkCOMMENT tk (f,c)) }
  @comments           ;
  @comment            ;
  -- Invalid characters
  @error              { tok (\(AlexPn _ f c) err -> TkError err (f,c)) }

{
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                             Create tokens
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

tok :: (AlexPosn -> Id -> Token) -> AlexPosn -> Id -> Token
tok f p tk = f p tk

createTkINT (AlexPn _ f c) tk = TkINT tk (f,c) (read tk :: Int)

createTkCHARACTER (AlexPn _ f c) tk = TkCHARACTER tk (f,c) (toChar tk)
    where
      toChar c = (read (map (\x-> if x == '*' then '\'' else x) c)::Char)

createTkFLOAT (AlexPn _ f c) tk = TkFLOAT tk (f,c) (toFloat tk)
    where
      toFloat f = (read (map (\x-> if x == '\'' then '.' else x) f)::Float)

createTkSTRING (AlexPn _ f c) tk = TkSTRINGS tk  (f,c) (tail $ init tk)
  -- where
  --   toString s = tail $ init $ s

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                              Data type Token
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data Token = 
  TkEndLine          { getTk :: Id, getPos :: Pos }                  |
  TkWORLD            { getTk :: Id, getPos :: Pos }                  |
  TkBATTLE           { getTk :: Id, getPos :: Pos }                  |
  TkPOWER            { getTk :: Id, getPos :: Pos }                  |
  TkSKILL            { getTk :: Id, getPos :: Pos }                  |
  TkRUNE             { getTk :: Id, getPos :: Pos }                  |
  TkRUNES            { getTk :: Id, getPos :: Pos }                  |
  TkKitOf            { getTk :: Id, getPos :: Pos }                  |
  TkINVENTORY        { getTk :: Id, getPos :: Pos }                  |
  TkITEMS            { getTk :: Id, getPos :: Pos }                  |
  TkSPAWN            { getTk :: Id, getPos :: Pos }                  |
  TkSUMMON           { getTk :: Id, getPos :: Pos }                  |
  TkBUTTON           { getTk :: Id, getPos :: Pos }                  |
  TkNotPressed       { getTk :: Id, getPos :: Pos }                  |
  TkKILL             { getTk :: Id, getPos :: Pos }                  |
  TkBOSS             { getTk :: Id, getPos :: Pos }                  |
  TkMONSTER          { getTk :: Id, getPos :: Pos }                  |
  TkUNLOCK           { getTk :: Id, getPos :: Pos }                  |
  TkCONTROLLER       { getTk :: Id, getPos :: Pos }                  |
  TkPLAY             { getTk :: Id, getPos :: Pos }                  |
  TkLOCK             { getTk :: Id, getPos :: Pos }                  |
  TkGameOver         { getTk :: Id, getPos :: Pos }                  |
  TkKeepPlaying      { getTk :: Id, getPos :: Pos }                  |
  TkJOYSTICK         { getTk :: Id, getPos :: Pos }                  |
  TkDROP             { getTk :: Id, getPos :: Pos }                  |
  TkDeathZone        { getTk :: Id, getPos :: Pos }                  |
  TkFREE             { getTk :: Id, getPos :: Pos }                  |
  TkPUFF             { getTk :: Id, getPos :: Pos }                  |
  TkWIN              { getTk :: Id, getPos :: Pos }                  |
  TkLOSE             { getTk :: Id, getPos :: Pos }                  |
  TkProgramName      { getTk :: Id, getPos :: Pos }                  |
  TkID               { getTk :: Id, getPos :: Pos }                  |
  TkIDType           { getTk :: Id, getPos :: Pos }                  |
  TkCHARACTER        { getTk :: Id, getPos :: Pos, getTkChar ::Char }   |
  TkSTRINGS          { getTk :: Id, getPos :: Pos, getTkStr  ::String } |
  TkINT              { getTk :: Id, getPos :: Pos, getTkInt  ::Int }    |
  TkFLOAT            { getTk :: Id, getPos :: Pos, getTkFloat::Float }  |
  TkFIN              { getTk :: Id, getPos :: Pos }                  |
  TkADD              { getTk :: Id, getPos :: Pos }                  |
  TkMIN              { getTk :: Id, getPos :: Pos }                  |
  TkMULT             { getTk :: Id, getPos :: Pos }                  |
  TkDIV              { getTk :: Id, getPos :: Pos }                  |
  TkDivEntera        { getTk :: Id, getPos :: Pos }                  |
  TkMOD              { getTk :: Id, getPos :: Pos }                  |
  TkINCREMENT        { getTk :: Id, getPos :: Pos }                  |
  TkDECREMENT        { getTk :: Id, getPos :: Pos }                  |
  TkLEN              { getTk :: Id, getPos :: Pos }                  |
  TkOR               { getTk :: Id, getPos :: Pos }                  |
  TkAND              { getTk :: Id, getPos :: Pos }                  |
  TkLessEqual        { getTk :: Id, getPos :: Pos }                  |
  TkLessThan         { getTk :: Id, getPos :: Pos }                  |
  TkGreaterEqual     { getTk :: Id, getPos :: Pos }                  |
  TkGreaterThan      { getTk :: Id, getPos :: Pos }                  |
  TkEQUAL            { getTk :: Id, getPos :: Pos }                  |
  TkNotEqual         { getTk :: Id, getPos :: Pos }                  |
  TkNOT              { getTk :: Id, getPos :: Pos }                  |
  TkUPPER            { getTk :: Id, getPos :: Pos }                  |
  TkLOWER            { getTk :: Id, getPos :: Pos }                  |
  TkOpenList         { getTk :: Id, getPos :: Pos }                  |
  TkCloseList        { getTk :: Id, getPos :: Pos }                  |
  TkOpenListIndex    { getTk :: Id, getPos :: Pos }                  |
  TkCloseListIndex   { getTk :: Id, getPos :: Pos }                  |
  TkANEXO            { getTk :: Id, getPos :: Pos }                  |
  TkCONCAT           { getTk :: Id, getPos :: Pos }                  |
  TkOpenArray        { getTk :: Id, getPos :: Pos }                  |
  TkCloseArray       { getTk :: Id, getPos :: Pos }                  |
  TkOpenArrayIndex   { getTk :: Id, getPos :: Pos }                  |
  TkCloseArrayIndex  { getTk :: Id, getPos :: Pos }                  |
  TkOpenBrackets     { getTk :: Id, getPos :: Pos }                  |
  TkCloseBrackets    { getTk :: Id, getPos :: Pos }                  |
  TkIN               { getTk :: Id, getPos :: Pos }                  |
  TkTO               { getTk :: Id, getPos :: Pos }                  |
  TkREF              { getTk :: Id, getPos :: Pos }                  |
  TkGUARD            { getTk :: Id, getPos :: Pos }                  |
  TkASSIG            { getTk :: Id, getPos :: Pos }                  |
  TkOpenParenthesis  { getTk :: Id, getPos :: Pos }                  |
  TkCloseParenthesis { getTk :: Id, getPos :: Pos }                  |
  TkCOMA             { getTk :: Id, getPos :: Pos }                  |
  TkOpenComments     { getTk :: Id, getPos :: Pos }                  |
  TkCloseComments    { getTk :: Id, getPos :: Pos }                  |
  TkCOMMENT          { getTk :: Id, getPos :: Pos }                  |
  TkError            { getTk :: Id, getPos :: Pos }
  
  deriving (Eq)

showTk :: Id -> Pos -> String
showTk tk p = "(" ++ tk ++ "), pos " ++ show p

instance Show Token where
  show (TkEndLine tk p)          = "(\\n), pos " ++ show p
  show (TkWORLD tk p)            = showTk tk p
  show (TkBATTLE tk p)            = showTk tk p
  show (TkPOWER tk p)            = showTk tk p
  show (TkSKILL tk p)            = showTk tk p
  show (TkRUNE tk p)             = showTk tk p
  show (TkRUNES tk p)            = showTk tk p
  show (TkKitOf tk p)            = showTk tk p
  show (TkINVENTORY tk p)        = showTk tk p
  show (TkITEMS tk p)            = showTk tk p
  show (TkSPAWN tk p)            = showTk tk p
  show (TkSUMMON tk p)           = showTk tk p
  show (TkBUTTON tk p)           = showTk tk p
  show (TkNotPressed tk p)       = showTk tk p
  show (TkKILL tk p)             = showTk tk p
  show (TkBOSS tk p)             = showTk tk p
  show (TkMONSTER tk p)          = showTk tk p
  show (TkUNLOCK tk p)           = showTk tk p
  show (TkCONTROLLER tk p)       = showTk tk p
  show (TkPLAY tk p)             = showTk tk p
  show (TkLOCK tk p)             = showTk tk p
  show (TkGameOver tk p)         = showTk tk p
  show (TkKeepPlaying tk p)      = showTk tk p
  show (TkJOYSTICK tk p)         = showTk tk p
  show (TkDROP tk p)             = showTk tk p
  show (TkDeathZone tk p)        = showTk tk p
  show (TkFREE tk p)             = showTk tk p
  show (TkPUFF tk p)             = showTk tk p
  show (TkWIN tk p)              = showTk tk p
  show (TkLOSE tk p)             = showTk tk p
  show (TkProgramName tk p)      = showTk tk p
  show (TkID tk p)               = "Identifier \"" ++ tk ++ "\", pos " ++ show p
  show (TkIDType tk p)           = "Type identifier \"" ++ tk ++ "\", pos " ++ show p
  show (TkCHARACTER tk p _)      = "Character '" ++ tk ++ "', pos " ++ show p
  show (TkSTRINGS tk p _)        = "String \"" ++ tk ++ "\", pos " ++ show p
  show (TkINT tk p _)            = "Integer " ++ tk ++ ", pos " ++ show p
  show (TkFLOAT tk p _)          = "Float " ++ tk ++ ", pos " ++ show p
  show (TkFIN tk p)              = showTk tk p
  show (TkADD tk p)              = showTk tk p
  show (TkMIN tk p)              = showTk tk p
  show (TkMULT tk p)             = showTk tk p
  show (TkDIV tk p)              = showTk tk p
  show (TkDivEntera tk p)        = showTk tk p
  show (TkMOD tk p)              = showTk tk p
  show (TkINCREMENT tk p)        = showTk tk p
  show (TkDECREMENT tk p)        = showTk tk p
  show (TkLEN tk p)              = showTk tk p
  show (TkOR tk p)               = showTk tk p
  show (TkAND tk p)              = showTk tk p
  show (TkLessEqual tk p)        = showTk tk p
  show (TkLessThan tk p)         = showTk tk p
  show (TkGreaterEqual tk p)     = showTk tk p
  show (TkGreaterThan tk p)      = showTk tk p
  show (TkEQUAL tk p)            = showTk tk p
  show (TkNotEqual tk p)         = showTk tk p
  show (TkNOT tk p)              = showTk tk p
  show (TkUPPER tk p)            = showTk tk p
  show (TkLOWER tk p)            = showTk tk p
  show (TkOpenList tk p)         = showTk tk p
  show (TkCloseList tk p)        = showTk tk p
  show (TkOpenListIndex tk p)    = showTk tk p
  show (TkCloseListIndex tk p)   = showTk tk p
  show (TkANEXO tk p)            = showTk tk p
  show (TkCONCAT tk p)           = showTk tk p
  show (TkOpenArray tk p)        = showTk tk p
  show (TkCloseArray tk p)       = showTk tk p
  show (TkOpenArrayIndex tk p)   = showTk tk p
  show (TkCloseArrayIndex tk p)  = showTk tk p
  show (TkOpenBrackets tk p)     = showTk tk p
  show (TkCloseBrackets tk p)    = showTk tk p
  show (TkIN tk p)               = showTk tk p
  show (TkTO tk p)               = showTk tk p
  show (TkREF tk p)              = showTk tk p
  show (TkGUARD tk p)            = showTk tk p
  show (TkASSIG tk p)            = showTk tk p
  show (TkOpenParenthesis tk p)  = showTk tk p
  show (TkCloseParenthesis tk p) = showTk tk p
  show (TkCOMA tk p)             = showTk tk p
  show (TkOpenComments tk p)     = showTk tk p
  show (TkCloseComments tk p)    = showTk tk p
  show (TkCOMMENT tk p)          = showTk tk p
  show (TkError tk p)            = tk
}
