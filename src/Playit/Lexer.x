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
    tkErrorToString,
    alexScanTokens,
    getTokenPosicion,
    hasError,
    isError,
) where

import Data.List(intercalate)
-- import Data.List.Split (splitOn)

}

%wrapper "posn"

-- Conjuntos de caracteres

$digitos      = [0-9]
$abece_minus  = [a-z]
$abece_mayus  = [A-Z]
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
@id_tipo      = $abece_mayus $char_id*
@id           = $abece_minus $char_id*
@programas    = \% $char_id+ \%
@endLine      = ($white* \n)+ 
@float        = $digitos+ \' $digitos+
@comments     = \"\' ( . # [\'\"] | \n)* \'\"
@comment      = \@ [. # \n]*
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
  @id              { tok (\p s -> TkID  p s) }
  @id_tipo             { tok (\p s -> TkIDTipo p s) }

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

getPosnToStr :: AlexPosn -> String
getPosnToStr (AlexPn _ f c) = " en la fila: " ++ (show f) ++ ", columna: " ++ (show c)

getTokenPosicion :: Token -> (Int,Int)
getTokenPosicion token = (f,c)
    where (AlexPn _ f c) = getAlexPosn token


data Token = TkWORLD {getAlexPosn :: AlexPosn,getToken :: String}
           | TkRUNE {getAlexPosn :: AlexPosn,getToken :: String}
           | TkLOSE {getAlexPosn :: AlexPosn,getToken :: String}
           | TkOF {getAlexPosn :: AlexPosn,getToken :: String}
           | TkBUTTON {getAlexPosn :: AlexPosn,getToken :: String}
           | TkWIN {getAlexPosn :: AlexPosn,getToken :: String}
           | TkBATLE {getAlexPosn :: AlexPosn,getToken :: String}
           | TkPOWER {getAlexPosn :: AlexPosn,getToken :: String}
           | TkSKILL {getAlexPosn :: AlexPosn,getToken :: String}
           | TkRUNES {getAlexPosn :: AlexPosn,getToken :: String}
           | TkKIT {getAlexPosn :: AlexPosn,getToken :: String}
           | TkINVENTORY {getAlexPosn :: AlexPosn,getToken :: String}
           | TkITEMS {getAlexPosn :: AlexPosn,getToken :: String}
           | TkSUMMON {getAlexPosn :: AlexPosn,getToken :: String}
           | TkFREE {getAlexPosn :: AlexPosn,getToken :: String}
           | TkDeathZone {getAlexPosn :: AlexPosn,getToken :: String}
           | TkJOYSTICK {getAlexPosn :: AlexPosn,getToken :: String}
           | TkDROP {getAlexPosn :: AlexPosn,getToken :: String}
           | TkNotPressed {getAlexPosn :: AlexPosn,getToken :: String}
           | TkCONTROLLER {getAlexPosn :: AlexPosn,getToken :: String}
           | TkIN  {getAlexPosn :: AlexPosn,getToken :: String}
           | TkTO  {getAlexPosn :: AlexPosn,getToken :: String}
           | TkPLAY {getAlexPosn :: AlexPosn,getToken :: String}
           | TkLOCK {getAlexPosn :: AlexPosn,getToken :: String}
           | TkUNLOCK {getAlexPosn :: AlexPosn,getToken :: String}
           | TkSPAWN {getAlexPosn :: AlexPosn,getToken :: String}
           | TkGameOver {getAlexPosn :: AlexPosn,getToken :: String}
           | TkKeepPlaying {getAlexPosn :: AlexPosn,getToken :: String}
           | TkKILL {getAlexPosn :: AlexPosn,getToken :: String}
           | TkMONSTER {getAlexPosn :: AlexPosn,getToken :: String}
           | TkBOSS {getAlexPosn :: AlexPosn,getToken :: String}
           | TkProgramName {getAlexPosn :: AlexPosn,getToken :: String}
           | TkID {getAlexPosn :: AlexPosn,getToken :: String}
           | TkIDTipo {getAlexPosn :: AlexPosn,getToken :: String}
           | TkCARACTER {getAlexPosn :: AlexPosn,getToken :: String}
           | TkSTRINGS {getAlexPosn :: AlexPosn,getToken :: String}
           | TkINT {getAlexPosn :: AlexPosn,getToken :: String}
           | TkFLOAT {getAlexPosn :: AlexPosn,getToken :: String}
           | TkDivEntera {getAlexPosn :: AlexPosn,getToken :: String}
           | TkOR {getAlexPosn :: AlexPosn,getToken :: String}
           | TkAND {getAlexPosn :: AlexPosn,getToken :: String}
           | TkLessEqual {getAlexPosn :: AlexPosn,getToken :: String}
           | TkEQUAL {getAlexPosn :: AlexPosn,getToken :: String}
           | TkNotEqual {getAlexPosn :: AlexPosn,getToken :: String}
           | TkGreaterEqual {getAlexPosn :: AlexPosn,getToken :: String}
           | TkOpenList {getAlexPosn :: AlexPosn,getToken :: String}
           | TkCloseList {getAlexPosn :: AlexPosn,getToken :: String}
           | TkOpenListIndex {getAlexPosn :: AlexPosn,getToken :: String}
           | TkCloseListIndex {getAlexPosn :: AlexPosn,getToken :: String}
           | TkINCREMENT {getAlexPosn :: AlexPosn,getToken :: String}
           | TkDECREMENT {getAlexPosn :: AlexPosn,getToken :: String}
           | TkSUM {getAlexPosn :: AlexPosn,getToken :: String}
           | TkMIN {getAlexPosn :: AlexPosn,getToken :: String}
           | TkMULT {getAlexPosn :: AlexPosn,getToken :: String}
           | TkDIV {getAlexPosn :: AlexPosn,getToken :: String}
           | TkMOD {getAlexPosn :: AlexPosn,getToken :: String}
           | TkLEN {getAlexPosn :: AlexPosn,getToken :: String}
           | TkREF {getAlexPosn :: AlexPosn,getToken :: String}
           | TkNOT {getAlexPosn :: AlexPosn,getToken :: String}
           | TkLessThan {getAlexPosn :: AlexPosn,getToken :: String}
           | TkGreaterThan {getAlexPosn :: AlexPosn,getToken :: String}
           | TkPUFF {getAlexPosn :: AlexPosn,getToken :: String}
           | TkOpenParenthesis {getAlexPosn :: AlexPosn,getToken :: String}
           | TkCloseParenthesis {getAlexPosn :: AlexPosn,getToken :: String}
           | TkOpenBrackets {getAlexPosn :: AlexPosn,getToken :: String}
           | TkCloseBrackets {getAlexPosn :: AlexPosn,getToken :: String}
           | TkCOMA {getAlexPosn :: AlexPosn,getToken :: String}
           | TkANEXO {getAlexPosn :: AlexPosn,getToken :: String}
           | TkGUARD {getAlexPosn :: AlexPosn,getToken :: String}
           | TkASING {getAlexPosn :: AlexPosn,getToken :: String}
           | TkUPPER {getAlexPosn :: AlexPosn,getToken :: String}
           | TkLOWER {getAlexPosn :: AlexPosn,getToken :: String}
           | TkCMV {getAlexPosn :: AlexPosn,getToken :: String}
           | TkCM1 {getAlexPosn :: AlexPosn,getToken :: String}
           | TkFIN {getAlexPosn :: AlexPosn,getToken :: String}
           | TkOpenArray {getAlexPosn :: AlexPosn,getToken :: String}
           | TkCloseArray {getAlexPosn :: AlexPosn,getToken :: String}
           | TkOpenArrayIndex {getAlexPosn :: AlexPosn,getToken :: String}
           | TkCloseArrayIndex {getAlexPosn :: AlexPosn,getToken :: String}
           | TkError {mensaje :: String}
           | TkCONCAT {getAlexPosn :: AlexPosn,getToken :: String}
           | TkEndLine {getAlexPosn :: AlexPosn,getToken :: String}
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

-- 'tokerr' : Traduce los errores a un formato.
tokerr s (AlexPn _ l c) = 
    "Error: Caracter inesperado " ++ s ++ 
    " en la linea " ++ (show l) ++ ", columna " ++ (show c) ++ "."
    -- "\x1b[1m\x1b[31mGAME OVER!!\x1b[0m Lexical error at line \x1b[1m\x1b[31m%d\x1b[0m, column \x1b[1m\x1b[31m%d\x1b[0m:\n%s\n"
    -- ++ (show l) ++ (show c) ++ fs
    -- where
    --     allLines = splitOn "\n" s
    --     maxSize = foldl max (-1) $ map length allLines
    --     buildRuler = flip replicate '~'
    --     rule = buildRuler maxSize ++ "\n"
    --     relevantLines = drop (l-1) allLines
    --     firstLine = head relevantLines ++ "\n"
    --     restLines = take 4 $ tail relevantLines
    --     errorRuler = "\x1b[1m\x1b[31m" ++ (buildRuler (c-1)) ++ "^" ++ buildRuler (maxSize - c) ++ "\x1b[0m\n"
    --     fs = firstLine ++ errorRuler ++ (intercalate "\n" restLines)




-- 'tkErrorToString': Inserta nuevas lineas entre los errores para ser impresos.
tkErrorToString :: [Token] -> String
tkErrorToString tk = intercalate "\n" $ map mensaje tk

instance Show Token where
    show (TkWORLD p s) = s ++ (getPosnToStr p) -- world
    show (TkOF p s) = s ++ (getPosnToStr p) -- of
    show (TkEndLine p s) = "End Instruction \\n" ++  (getPosnToStr p) -- New line
    show (TkBUTTON p s) = s ++ (getPosnToStr p) -- Button
    show (TkRUNE p s) = s ++ (getPosnToStr p) -- rune
    show (TkLOSE p s) = s ++ (getPosnToStr p) -- Lose
    show (TkWIN p s) = s ++ (getPosnToStr p) -- Win
    show (TkBATLE p s) = s ++ (getPosnToStr p) -- Battle
    show (TkPOWER p s) = s ++ (getPosnToStr p) -- Power
    show (TkSKILL p s) = s ++ (getPosnToStr p) -- Skill
    show (TkRUNES p s) = s ++ (getPosnToStr p) -- Runes
    show (TkCONCAT p s) = s ++ (getPosnToStr p) -- Concatenación de lista
    show (TkKIT p s) = s ++ (getPosnToStr p) -- Kit
    show (TkINVENTORY p s) = s ++ (getPosnToStr p) -- Inventory
    show (TkITEMS p s) = s ++ (getPosnToStr p) -- Items
    show (TkSUMMON p s) = s ++ (getPosnToStr p) -- summon
    show (TkFREE p s) = s ++ (getPosnToStr p) -- free
    show (TkDeathZone p s) = s ++ (getPosnToStr p) -- DeathZone
    show (TkJOYSTICK p s) = s ++ (getPosnToStr p) -- joystick
    show (TkDROP p s) = s ++ (getPosnToStr p) -- drop
    show (TkNotPressed p s) = s ++ (getPosnToStr p) -- notPressed
    show (TkCONTROLLER p s) = s ++ (getPosnToStr p) -- controller
    show (TkIN p s)  = s ++ (getPosnToStr p) -- <-
    show (TkTO p s)  = s ++ (getPosnToStr p) -- ->
    show (TkPLAY p s) = s ++ (getPosnToStr p) -- play
    show (TkLOCK p s) = s ++ (getPosnToStr p) -- lock
    show (TkUNLOCK p s) = s ++ (getPosnToStr p) -- unlock
    show (TkSPAWN p s) = s ++ (getPosnToStr p) -- spawn
    show (TkGameOver p s) = s ++ (getPosnToStr p) -- gameOver
    show (TkKeepPlaying p s) = s ++ (getPosnToStr p) -- keepPlaying
    show (TkKILL p s) = s ++ (getPosnToStr p) -- kill
    show (TkMONSTER p s) =s ++ (getPosnToStr p) -- monster
    show (TkBOSS p s) = s ++ (getPosnToStr p) -- boss
    show (TkProgramName p s) = "nombre del programa '" ++ s ++"'" ++ (getPosnToStr p) -- Nombre programa
    show (TkID p s) = "identificador '" ++ s ++ "'" ++ (getPosnToStr p) -- Id
    show (TkIDTipo p s) = "tipo '" ++ s ++ "'" ++ (getPosnToStr p) -- Id
    show (TkCARACTER p s) = "caracter '" ++ s ++ "'" ++ (getPosnToStr p) -- carActer
    show (TkSTRINGS p s) = "string '" ++ s ++"'" ++ (getPosnToStr p) -- String
    show (TkINT p s) = "entero '" ++ s ++"'" ++ (getPosnToStr p) -- Entero
    show (TkFLOAT p s) = "flotante '" ++ s ++"'" ++ (getPosnToStr p) -- Flotante
    show (TkDivEntera p s) = s ++ (getPosnToStr p) -- //
    show (TkOR p s) = s ++ (getPosnToStr p) -- ||
    show (TkAND p s) = s ++ (getPosnToStr p) -- &&
    show (TkLessEqual p s) = s ++ (getPosnToStr p) -- <=
    show (TkEQUAL p s) = s ++ (getPosnToStr p) -- ==
    show (TkNotEqual p s) = s ++ (getPosnToStr p) -- !=
    show (TkGreaterEqual p s) = s ++ (getPosnToStr p) -- >=
    show (TkOpenList p s) = s ++ (getPosnToStr p) -- <<
    show (TkCloseList p s) = s ++ (getPosnToStr p) -- >>
    show (TkOpenListIndex p s) = s ++ (getPosnToStr p) -- |>
    show (TkCloseListIndex p s) = s ++ (getPosnToStr p) -- <°
    show (TkINCREMENT p s) = s ++ (getPosnToStr p) -- ++
    show (TkDECREMENT p s) = s ++ (getPosnToStr p) -- --
    show (TkSUM p s) = s ++ (getPosnToStr p) -- +
    show (TkMIN p s) = s ++ (getPosnToStr p) -- -
    show (TkMULT p s) = s ++ (getPosnToStr p) -- *
    show (TkDIV p s) = s ++ (getPosnToStr p) -- /
    show (TkMOD p s) = s ++ (getPosnToStr p) -- %
    show (TkLEN p s) = s ++ (getPosnToStr p) -- #
    show (TkREF p s) = s ++ (getPosnToStr p) -- ?
    show (TkNOT p s) = s ++ (getPosnToStr p) -- !
    show (TkLessThan p s) = s ++ (getPosnToStr p) -- <
    show (TkGreaterThan p s) = s ++ (getPosnToStr p) -- >
    show (TkPUFF p s) = s ++ (getPosnToStr p) -- puff
    show (TkOpenParenthesis p s) = s ++ (getPosnToStr p) -- (
    show (TkCloseParenthesis p s) = s ++ (getPosnToStr p) -- )
    show (TkOpenBrackets p s) = s ++ (getPosnToStr p) -- {
    show (TkCloseBrackets p s) = s ++ (getPosnToStr p) -- }
    show (TkCOMA p s) = s ++ (getPosnToStr p) -- ,
    show (TkANEXO p s) = s ++ (getPosnToStr p) -- :
    show (TkGUARD p s) = s ++ (getPosnToStr p) -- |
    show (TkASING p s) = s ++ (getPosnToStr p) -- =
    show (TkUPPER p s) = s ++ (getPosnToStr p) -- ^
    show (TkLOWER p s) = s ++ (getPosnToStr p) -- .
    show (TkFIN p s) = s ++ (getPosnToStr p) -- .~
    show (TkOpenArray p s) = s ++ (getPosnToStr p) -- "|}"
    show (TkCloseArray p s) = s ++ (getPosnToStr p) -- "{|"
    show (TkOpenArrayIndex p s) = s ++ (getPosnToStr p) -- "|)"
    show (TkCloseArrayIndex p s) = s ++ (getPosnToStr p) -- "(|"

}
