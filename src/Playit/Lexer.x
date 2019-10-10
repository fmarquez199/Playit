{
{-
 *  Analizador léxico del Lenguaje  Playit
 *
 * Copyright : (c) 
 *  Manuel Gonzalez     11-10390
 *  Francisco Javier    12-11163
 *  Natascha Gamboa     12-11250
-}

module Playit.Lexer (
    Token(..),
    AlexPosn(..), 
    hasError,
    alexScanTokens,
    tkErrorToString,
    isError,
    posicion
) where

import Data.List(intercalate)

}

%wrapper "posn"

-- Conjuntos de caracteres

$digitos      = [0-9]
$abecedario   = [a-zA-Z]
$simbolos     = [\! \" \# \$ \% \& \' \( \) \* \+ \, \- \. \/ \: \; \< \= \> \? \@
              \[ \\ \] \^ \_ \` \{ \| \} \~ '\0' '\t' '\n' '\\' '\'' '\"' '\~' '\*']
$validos      = [$digitos $abecedario $simbolos  $white]
$char_texto   = [$validos # [\* \~ \\]]
$char_id      = [$digitos $abecedario \_ \']

-- Expresiones regulares

@scape        = "\\" | "\0" | "\n" | "\t" | "\~" | "\*"
@caracteres   = $char_texto | @scape
@texto        = @caracteres*
@caracter     = "*" @caracteres "*"
@strings      = \~ @texto \~
@id           = $abecedario $char_id*
@programas    = \% $char_id+ \%
@endLine      = ($white* \n)+ 
@float        = $digitos+ \' $digitos+
@comments     = \"\' ( . # [\'\"] | \n)* \'\"
@comment      = \@ [. # \n]* \n 
@error        = .

tokens :-

  ([$white # \n])+     ;
  
  -- Palabras reservadas

  Battle               { tok (\p s -> TkBATLE p s) }
  DeathZone            { tok (\p s -> TkDeathZone p s) }
  Inventory            { tok (\p s -> TkINVENTORY p s) }
  Items                { tok (\p s -> TkITEMS p s) }
  Kit                  { tok (\p s -> TkKIT p s) }
  Power                { tok (\p s -> TkPOWER p s) }
  Rune                 { tok (\p s -> TkRUNE p s) }
  Runes                { tok (\p s -> TkRUNES p s) }
  Skill                { tok (\p s -> TkSKILL p s) }
  Button               { tok (\p s -> TkBUTTON p s) }
  boss                 { tok (\p s -> TkBOSS p s) }
  controller           { tok (\p s -> TkCONTROLLER p s) }
  drop                 { tok (\p s -> TkDROP p s) }
  notPressed           { tok (\p s -> TkNotPressed p s) }
  free                 { tok (\p s -> TkFREE p s) }
  gameOver             { tok (\p s -> TkGameOver p s) }
  joystick             { tok (\p s -> TkJOYSTICK p s) }
  keepPlaying          { tok (\p s -> TkKeepPlaying p s) }
  kill                 { tok (\p s -> TkKILL p s) }
  lock                 { tok (\p s -> TkLOCK p s) }
  monster              { tok (\p s -> TkMONSTER p s) }
  play                 { tok (\p s -> TkPLAY p s) }
  puff                 { tok (\p s -> TkPUFF p s) }
  spawn                { tok (\p s -> TkSPAWN p s) }
  summon               { tok (\p s -> TkSUMMON p s) }
  unlock               { tok (\p s -> TkUNLOCK p s) }
  world                { tok (\p s -> TkWORLD p s) }
  of                   { tok (\p s -> TkOF p s) }
  @endLine             { tok (\p s -> TkEndLine p s) }

  -- Literales booleanos
  
  Win                  { tok (\p s -> TkWIN p s) }
  Lose                 { tok (\p s -> TkLOSE p s) }

  -- Identificadores

  @programas           { tok (\p s -> TkProgramName p s) }
  @id                  { tok (\p s -> TkID p s) }

  -- Caracteres

  @caracter            { tok (\p s -> TkCARACTER p s) }
  @strings             { tok (\p s -> TkSTRINGS p s) }
  
  -- Literares numericos
  
  $digitos+            { tok (\p s -> TkINT p s) }
  @float               { tok (\p s -> TkFLOAT p s) }

  -- Simbolos

  ".~"                 { tok (\p s -> TkFIN p s) }
  "//"                 { tok (\p s -> TkDivEntera p s) }
  "||"                 { tok (\p s -> TkOR p s) }
  "&&"                 { tok (\p s -> TkAND p s) }
  "<="                 { tok (\p s -> TkLessEqual p s) }
  "=="                 { tok (\p s -> TkEQUAL p s) }
  "!="                 { tok (\p s -> TkNotEqual p s) }
  ">="                 { tok (\p s -> TkGreaterEqual p s) }
  "<<"                 { tok (\p s -> TkOpenList p s) }
  ">>"                 { tok (\p s -> TkCloseList p s) }
  "|>"                 { tok (\p s -> TkOpenListIndex p s) }
  "<|"                 { tok (\p s -> TkCloseListIndex p s) }
  "++"                 { tok (\p s -> TkINCREMENT p s) }
  "--"                 { tok (\p s -> TkDECREMENT p s) }
  "<-"                 { tok (\p s -> TkIN  p s) }
  "->"                 { tok (\p s -> TkTO  p s) }
  "|}"                 { tok (\p s -> TkOpenArray p s) }
  "{|"                 { tok (\p s -> TkCloseArray p s) }
  "|)"                 { tok (\p s -> TkOpenArrayIndex p s) }
  "(|"                 { tok (\p s -> TkCloseArrayIndex p s) }
  "+"                  { tok (\p s -> TkSUM p s) }
  "-"                  { tok (\p s -> TkMIN p s) }
  "*"                  { tok (\p s -> TkMULT p s) }
  "/"                  { tok (\p s -> TkDIV p s) }
  "%"                  { tok (\p s -> TkMOD p s) }
  "#"                  { tok (\p s -> TkLEN p s) }
  "?"                  { tok (\p s -> TkREF p s) }
  "!"                  { tok (\p s -> TkNOT p s) }
  "<"                  { tok (\p s -> TkLessThan p s) }
  ">"                  { tok (\p s -> TkGreaterThan p s) }
  "("                  { tok (\p s -> TkOpenParenthesis p s) }
  ")"                  { tok (\p s -> TkCloseParenthesis p s) }
  "{"                  { tok (\p s -> TkOpenBrackets p s) }
  "}"                  { tok (\p s -> TkCloseBrackets p s) }
  ","                  { tok (\p s -> TkCOMA p s) }
  ":"                  { tok (\p s -> TkANEXO p s) }
  "::"                 { tok (\p s -> TkCONCAT p s) }
  "|"                  { tok (\p s -> TkGUARD p s) }
  "="                  { tok (\p s -> TkASING p s) }
  "^"                  { tok (\p s -> TkUPPER p s) }
  "."                  { tok (\p s -> TkLOWER p s) }
  
  -- Comentarios

  @comments            ;
  @comment             ;

  -- Caracteres invalidos

  @error               { createTkError}

{
tok :: (AlexPosn -> String -> Token) -> AlexPosn -> String -> Token
tok f p s = f p s

pos :: AlexPosn -> String
pos (AlexPn _ f c) = " en la fila: " ++ (show f) ++ ", columna: " ++ (show c)

data Token = TkWORLD AlexPosn String
           | TkRUNE AlexPosn String
           | TkLOSE AlexPosn String
           | TkOF AlexPosn String
           | TkBUTTON AlexPosn String
           | TkWIN AlexPosn String
           | TkBATLE AlexPosn String
           | TkPOWER AlexPosn String
           | TkSKILL AlexPosn String
           | TkRUNES AlexPosn String
           | TkKIT AlexPosn String
           | TkINVENTORY AlexPosn String
           | TkITEMS AlexPosn String
           | TkSUMMON AlexPosn String
           | TkFREE AlexPosn String
           | TkDeathZone AlexPosn String
           | TkJOYSTICK AlexPosn String
           | TkDROP AlexPosn String
           | TkNotPressed AlexPosn String
           | TkCONTROLLER AlexPosn String
           | TkIN  AlexPosn String
           | TkTO  AlexPosn String
           | TkPLAY AlexPosn String
           | TkLOCK AlexPosn String
           | TkUNLOCK AlexPosn String
           | TkSPAWN AlexPosn String
           | TkGameOver AlexPosn String
           | TkKeepPlaying AlexPosn String
           | TkKILL AlexPosn String
           | TkMONSTER AlexPosn String
           | TkBOSS AlexPosn String
           | TkProgramName AlexPosn String
           | TkID AlexPosn String
           | TkCARACTER AlexPosn String
           | TkSTRINGS AlexPosn String
           | TkINT AlexPosn String
           | TkFLOAT AlexPosn String
           | TkDivEntera AlexPosn String
           | TkOR AlexPosn String
           | TkAND AlexPosn String
           | TkLessEqual AlexPosn String
           | TkEQUAL AlexPosn String
           | TkNotEqual AlexPosn String
           | TkGreaterEqual AlexPosn String
           | TkOpenList AlexPosn String
           | TkCloseList AlexPosn String
           | TkOpenListIndex AlexPosn String
           | TkCloseListIndex AlexPosn String
           | TkINCREMENT AlexPosn String
           | TkDECREMENT AlexPosn String
           | TkSUM AlexPosn String
           | TkMIN AlexPosn String
           | TkMULT AlexPosn String
           | TkDIV AlexPosn String
           | TkMOD AlexPosn String
           | TkLEN AlexPosn String
           | TkREF AlexPosn String
           | TkNOT AlexPosn String
           | TkLessThan AlexPosn String
           | TkGreaterThan AlexPosn String
           | TkPUFF AlexPosn String
           | TkOpenParenthesis AlexPosn String
           | TkCloseParenthesis AlexPosn String
           | TkOpenBrackets AlexPosn String
           | TkCloseBrackets AlexPosn String
           | TkCOMA AlexPosn String
           | TkANEXO AlexPosn String
           | TkGUARD AlexPosn String
           | TkASING AlexPosn String
           | TkUPPER AlexPosn String
           | TkLOWER AlexPosn String
           | TkCMV AlexPosn String
           | TkCM1 AlexPosn String
           | TkFIN AlexPosn String
           | TkOpenArray AlexPosn String
           | TkCloseArray AlexPosn String
           | TkOpenArrayIndex AlexPosn String
           | TkCloseArrayIndex AlexPosn String
           | TkError {mensaje :: String}
           | TkCONCAT AlexPosn String
           | TkEndLine AlexPosn String
           deriving (Eq)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                       Manejo de los tokens erroneos
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


createTkError alex_pos err           = TkError $ tokerr err alex_pos


-- 'hasError' : Determina si en una lista de tokens existe al menos un error.
hasError :: [Token] -> Bool
hasError [] = False
hasError ((TkError _):tks) = True
hasError (_:tks) = hasError tks

--  'isError' : Determina si un token es un error.
isError :: Token -> Bool
isError (TkError _) = True
isError _ = False

-- 'tokerr' : Traduce los errores al formato especificado en el proyecto.
tokerr s (AlexPn _ l c) = 
    "Error: Caracter inesperado " ++ s ++ 
    " en la linea " ++ (show l) ++ ", columna " ++ (show c) ++ "."

-- 'tkErrorToString': Inserta nuevas lineas entre los errores para ser impresos.
tkErrorToString :: [Token] -> String
tkErrorToString tk = intercalate "\n" $ map mensaje tk

instance Show Token where
    show (TkWORLD p s) = "Token " ++ s ++ (pos p) -- world
    show (TkOF p s) = "Token " ++ s ++ (pos p) -- of
    show (TkEndLine p s) = "Token End Instruction \\n" ++  (pos p) -- New line
    show (TkBUTTON p s) = "Token " ++ s ++ (pos p) -- Button
    show (TkRUNE p s) = "Token " ++ s ++ (pos p) -- rune
    show (TkLOSE p s) = "Token " ++ s ++ (pos p) -- Lose
    show (TkWIN p s) = "Token " ++ s ++ (pos p) -- Win
    show (TkBATLE p s) = "Token " ++ s ++ (pos p) -- Battle
    show (TkPOWER p s) = "Token " ++ s ++ (pos p) -- Power
    show (TkSKILL p s) = "Token " ++ s ++ (pos p) -- Skill
    show (TkRUNES p s) = "Token " ++ s ++ (pos p) -- Runes
    show (TkCONCAT p s) = "Token " ++ s ++ (pos p) -- Concatenación de lista
    show (TkKIT p s) = "Token " ++ s ++ (pos p) -- Kit
    show (TkINVENTORY p s) = "Token " ++ s ++ (pos p) -- Inventory
    show (TkITEMS p s) = "Token " ++ s ++ (pos p) -- Items
    show (TkSUMMON p s) = "Token " ++ s ++ (pos p) -- summon
    show (TkFREE p s) = "Token " ++ s ++ (pos p) -- free
    show (TkDeathZone p s) = "Token " ++ s ++ (pos p) -- DeathZone
    show (TkJOYSTICK p s) = "Token " ++ s ++ (pos p) -- joystick
    show (TkDROP p s) = "Token " ++ s ++ (pos p) -- drop
    show (TkNotPressed p s) = "Token " ++ s ++ (pos p) -- notPressed
    show (TkCONTROLLER p s) = "Token " ++ s ++ (pos p) -- controller
    show (TkIN p s)  = "Token " ++ s ++ (pos p) -- <-
    show (TkTO p s)  = "Token " ++ s ++ (pos p) -- ->
    show (TkPLAY p s) = "Token " ++ s ++ (pos p) -- play
    show (TkLOCK p s) = "Token " ++ s ++ (pos p) -- lock
    show (TkUNLOCK p s) = "Token " ++ s ++ (pos p) -- unlock
    show (TkSPAWN p s) = "Token " ++ s ++ (pos p) -- spawn
    show (TkGameOver p s) = "Token " ++ s ++ (pos p) -- gameOver
    show (TkKeepPlaying p s) = "Token " ++ s ++ (pos p) -- keepPlaying
    show (TkKILL p s) = "Token " ++ s ++ (pos p) -- kill
    show (TkMONSTER p s) = "Token " ++ s ++ (pos p) -- monster
    show (TkBOSS p s) = "Token " ++ s ++ (pos p) -- boss
    show (TkProgramName p s) = "Token nombre programa " ++ s ++ (pos p) -- Nombre programa
    show (TkID p s) = "Token identificador \"" ++ s ++ "\"" ++ (pos p) -- Id
    show (TkCARACTER p s) = "Token caracter " ++ s ++ (pos p) -- carActer
    show (TkSTRINGS p s) = "Token string " ++ s ++ (pos p) -- String
    show (TkINT p s) = "Token entero " ++ s ++ (pos p) -- Entero
    show (TkFLOAT p s) = "Token flotante " ++ s ++ (pos p) -- Flotante
    show (TkDivEntera p s) = "Token " ++ s ++ (pos p) -- //
    show (TkOR p s) = "Token " ++ s ++ (pos p) -- ||
    show (TkAND p s) = "Token " ++ s ++ (pos p) -- &&
    show (TkLessEqual p s) = "Token " ++ s ++ (pos p) -- <=
    show (TkEQUAL p s) = "Token " ++ s ++ (pos p) -- ==
    show (TkNotEqual p s) = "Token " ++ s ++ (pos p) -- !=
    show (TkGreaterEqual p s) = "Token " ++ s ++ (pos p) -- >=
    show (TkOpenList p s) = "Token " ++ s ++ (pos p) -- <<
    show (TkCloseList p s) = "Token " ++ s ++ (pos p) -- >>
    show (TkOpenListIndex p s) = "Token " ++ s ++ (pos p) -- |>
    show (TkCloseListIndex p s) = "Token " ++ s ++ (pos p) -- <°
    show (TkINCREMENT p s) = "Token " ++ s ++ (pos p) -- ++
    show (TkDECREMENT p s) = "Token " ++ s ++ (pos p) -- --
    show (TkSUM p s) = "Token " ++ s ++ (pos p) -- +
    show (TkMIN p s) = "Token " ++ s ++ (pos p) -- -
    show (TkMULT p s) = "Token " ++ s ++ (pos p) -- *
    show (TkDIV p s) = "Token " ++ s ++ (pos p) -- /
    show (TkMOD p s) = "Token " ++ s ++ (pos p) -- %
    show (TkLEN p s) = "Token " ++ s ++ (pos p) -- #
    show (TkREF p s) = "Token " ++ s ++ (pos p) -- ?
    show (TkNOT p s) = "Token " ++ s ++ (pos p) -- !
    show (TkLessThan p s) = "Token " ++ s ++ (pos p) -- <
    show (TkGreaterThan p s) = "Token " ++ s ++ (pos p) -- >
    show (TkPUFF p s) = "Token " ++ s ++ (pos p) -- puff
    show (TkOpenParenthesis p s) = "Token " ++ s ++ (pos p) -- (
    show (TkCloseParenthesis p s) = "Token " ++ s ++ (pos p) -- )
    show (TkOpenBrackets p s) = "Token " ++ s ++ (pos p) -- {
    show (TkCloseBrackets p s) = "Token " ++ s ++ (pos p) -- }
    show (TkCOMA p s) = "Token " ++ s ++ (pos p) -- ,
    show (TkANEXO p s) = "Token " ++ s ++ (pos p) -- :
    show (TkGUARD p s) = "Token " ++ s ++ (pos p) -- |
    show (TkASING p s) = "Token " ++ s ++ (pos p) -- =
    show (TkUPPER p s) = "Token " ++ s ++ (pos p) -- ^
    show (TkLOWER p s) = "Token " ++ s ++ (pos p) -- .
    show (TkFIN p s) = "Token " ++ s ++ (pos p) -- .~
    show (TkOpenArray p s) = "Token " ++ s ++ (pos p) -- "|}"
    show (TkCloseArray p s) = "Token " ++ s ++ (pos p) -- "{|"
    show (TkOpenArrayIndex p s) = "Token " ++ s ++ (pos p) -- "|)"
    show (TkCloseArrayIndex p s) = "Token " ++ s ++ (pos p) -- "(|"


posicion :: Token -> (Int, Int)
posicion (TkWORLD (AlexPn _ f c) _) = (f, c)
posicion (TkRUNE (AlexPn _ f c) _) = (f, c)
posicion (TkLOSE (AlexPn _ f c) _) = (f, c)
posicion (TkOF (AlexPn _ f c) _) = (f, c)
posicion (TkBUTTON (AlexPn _ f c) _) = (f, c)
posicion (TkWIN (AlexPn _ f c) _) = (f, c)
posicion (TkBATLE (AlexPn _ f c) _) = (f, c)
posicion (TkPOWER (AlexPn _ f c) _) = (f, c)
posicion (TkSKILL (AlexPn _ f c) _) = (f, c)
posicion (TkRUNES (AlexPn _ f c) _) = (f, c)
posicion (TkKIT (AlexPn _ f c) _) = (f, c)
posicion (TkINVENTORY (AlexPn _ f c) _) = (f, c)
posicion (TkITEMS (AlexPn _ f c) _) = (f, c)
posicion (TkSUMMON (AlexPn _ f c) _) = (f, c)
posicion (TkFREE (AlexPn _ f c) _) = (f, c)
posicion (TkDeathZone (AlexPn _ f c) _) = (f, c)
posicion (TkJOYSTICK (AlexPn _ f c) _) = (f, c)
posicion (TkDROP (AlexPn _ f c) _) = (f, c)
posicion (TkNotPressed (AlexPn _ f c) _) = (f, c)
posicion (TkCONTROLLER (AlexPn _ f c) _) = (f, c)
posicion (TkIN (AlexPn _ f c) _) = (f, c)
posicion (TkTO (AlexPn _ f c) _) = (f, c)
posicion (TkPLAY (AlexPn _ f c) _) = (f, c)
posicion (TkLOCK (AlexPn _ f c) _) = (f, c)
posicion (TkUNLOCK (AlexPn _ f c) _) = (f, c)
posicion (TkSPAWN (AlexPn _ f c) _) = (f, c)
posicion (TkGameOver (AlexPn _ f c) _) = (f, c)
posicion (TkKeepPlaying (AlexPn _ f c) _) = (f, c)
posicion (TkKILL (AlexPn _ f c) _) = (f, c)
posicion (TkMONSTER (AlexPn _ f c) _) = (f, c)
posicion (TkBOSS (AlexPn _ f c) _) = (f, c)
posicion (TkProgramName (AlexPn _ f c) _) = (f, c)
posicion (TkID (AlexPn _ f c) _) = (f, c)
posicion (TkCARACTER (AlexPn _ f c) _) = (f, c)
posicion (TkSTRINGS (AlexPn _ f c) _) = (f, c)
posicion (TkINT (AlexPn _ f c) _) = (f, c)
posicion (TkFLOAT (AlexPn _ f c) _) = (f, c)
posicion (TkDivEntera (AlexPn _ f c) _) = (f, c)
posicion (TkOR (AlexPn _ f c) _) = (f, c)
posicion (TkAND (AlexPn _ f c) _) = (f, c)
posicion (TkLessEqual (AlexPn _ f c) _) = (f, c)
posicion (TkEQUAL (AlexPn _ f c) _) = (f, c)
posicion (TkNotEqual (AlexPn _ f c) _) = (f, c)
posicion (TkGreaterEqual (AlexPn _ f c) _) = (f, c)
posicion (TkOpenList (AlexPn _ f c) _) = (f, c)
posicion (TkCloseList (AlexPn _ f c) _) = (f, c)
posicion (TkOpenListIndex (AlexPn _ f c) _) = (f, c)
posicion (TkCloseListIndex (AlexPn _ f c) _) = (f, c)
posicion (TkINCREMENT (AlexPn _ f c) _) = (f, c)
posicion (TkDECREMENT (AlexPn _ f c) _) = (f, c)
posicion (TkSUM (AlexPn _ f c) _) = (f, c)
posicion (TkMIN (AlexPn _ f c) _) = (f, c)
posicion (TkMULT (AlexPn _ f c) _) = (f, c)
posicion (TkDIV (AlexPn _ f c) _) = (f, c)
posicion (TkMOD (AlexPn _ f c) _) = (f, c)
posicion (TkLEN (AlexPn _ f c) _) = (f, c)
posicion (TkREF (AlexPn _ f c) _) = (f, c)
posicion (TkNOT (AlexPn _ f c) _) = (f, c)
posicion (TkLessThan (AlexPn _ f c) _) = (f, c)
posicion (TkGreaterThan (AlexPn _ f c) _) = (f, c)
posicion (TkPUFF (AlexPn _ f c) _) = (f, c)
posicion (TkOpenParenthesis (AlexPn _ f c) _) = (f, c)
posicion (TkCloseParenthesis (AlexPn _ f c) _) = (f, c)
posicion (TkOpenBrackets (AlexPn _ f c) _) = (f, c)
posicion (TkCloseBrackets (AlexPn _ f c) _) = (f, c)
posicion (TkCOMA (AlexPn _ f c) _) = (f, c)
posicion (TkANEXO (AlexPn _ f c) _) = (f, c)
posicion (TkGUARD (AlexPn _ f c) _) = (f, c)
posicion (TkASING (AlexPn _ f c) _) = (f, c)
posicion (TkUPPER (AlexPn _ f c) _) = (f, c)
posicion (TkLOWER (AlexPn _ f c) _) = (f, c)
posicion (TkCMV (AlexPn _ f c) _) = (f, c)
posicion (TkCM1 (AlexPn _ f c) _) = (f, c)
posicion (TkFIN (AlexPn _ f c) _) = (f, c)
posicion (TkOpenArray (AlexPn _ f c) _) = (f, c)
posicion (TkCloseArray (AlexPn _ f c) _) = (f, c)
posicion (TkOpenArrayIndex (AlexPn _ f c) _) = (f, c)
posicion (TkCloseArrayIndex (AlexPn _ f c) _) = (f, c)
posicion (TkCONCAT (AlexPn _ f c) _) = (f, c)
posicion (TkEndLine (AlexPn _ f c) _) = (f, c)

}
