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

  Battle               { createTkBATLE }
  DeathZone            { createTkDeathZone }
  Inventory            { createTkINVENTORY }
  Items                { createTkITEMS }
  Kit                  { createTkKIT }
  Power                { createTkPOWER }
  Rune                 { createTkRUNE }
  Runes                { createTkRUNES }
  Skill                { createTkSKILL }
  Button               { createTkBUTTON }
  boss                 { createTkBOSS }
  controller           { createTkCONTROLLER }
  drop                 { createTkDROP }
  notPressed           { createTkNotPressed }
  free                 { createTkFREE }
  gameOver             { createTkGameOver }
  joystick             { createTkJOYSTICK }
  keepPlaying          { createTkKeepPlaying }
  kill                 { createTkKILL }
  lock                 { createTkLOCK }
  monster              { createTkMONSTER }
  play                 { createTkPLAY }
  puff                 { createTkPUFF }
  spawn                { createTkSPAWN }
  summon               { createTkSUMMON }
  unlock               { createTkUNLOCK }
  world                { createTkWORLD }
  of                   { createTkOF }
  @endLine             { createTkEndLine }

  -- Literales booleanos
  
  Win                  { createTkWIN }
  Lose                 { createTkLOSE }

  -- Identificadores

  @programas           { createTkProgramName }
  @id                  { createTkID }
  @id_tipo             { createTkIDTipo }

  -- Caracteres

  @caracter            { createTkCARACTER }
  @strings             { createTkSTRINGS }
  
  -- Literares numericos
  
  $digitos+            { createTkINT }
  @float               { createTkFLOAT }

  -- Simbolos

  ".~"                 { createTkFIN }
  "//"                 { createTkDivEntera }
  "||"                 { createTkOR }
  "&&"                 { createTkAND }
  "<="                 { createTkLessEqual }
  "=="                 { createTkEQUAL }
  "!="                 { createTkNotEqual }
  ">="                 { createTkGreaterEqual }
  "<<"                 { createTkOpenList }
  ">>"                 { createTkCloseList }
  "|>"                 { createTkOpenListIndex }
  "<|"                 { createTkCloseListIndex }
  "++"                 { createTkINCREMENT }
  "--"                 { createTkDECREMENT }
  "<-"                 { createTkIN }
  "->"                 { createTkTO }
  "|}"                 { createTkOpenArray }
  "{|"                 { createTkCloseArray }
  "|)"                 { createTkOpenArrayIndex }
  "(|"                 { createTkCloseArrayIndex }
  "+"                  { createTkSUM }
  "-"                  { createTkMIN }
  "*"                  { createTkMULT }
  "/"                  { createTkDIV }
  "%"                  { createTkMOD }
  "#"                  { createTkLEN }
  "?"                  { createTkREF }
  "!"                  { createTkNOT }
  "<"                  { createTkLessThan }
  ">"                  { createTkGreaterThan }
  "("                  { createTkOpenParenthesis }
  ")"                  { createTkCloseParenthesis }
  "{"                  { createTkOpenBrackets }
  "}"                  { createTkCloseBrackets }
  ","                  { createTkCOMA }
  ":"                  { createTkANEXO }
  "::"                 { createTkCONCAT }
  "|"                  { createTkGUARD }
  "="                  { createTkASING }
  "^"                  { createTkUPPER }
  "."                  { createTkLOWER }
  
  -- Comentarios

  @comments            ;
  @comment             ;

  -- Caracteres invalidos

  @error               { createTkError }

{
tok :: (AlexPosn -> String -> Token) -> AlexPosn -> String -> Token
tok f p s = f p s

getPosnToStr :: AlexPosn -> String
getPosnToStr (AlexPn _ f c) = " en la fila: " ++ (show f) ++ ", columna: " ++ (show c)

getTokenPosicion :: Token -> (Int,Int)
getTokenPosicion token = (f,c)
    where (AlexPn _ f c) = getAlexPosn token


data Token = TkWORLD {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkRUNE {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkLOSE {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkOF {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkBUTTON {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkWIN {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkBATLE {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkPOWER {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkSKILL {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkRUNES {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkKIT {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkINVENTORY {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkITEMS {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkSUMMON {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkFREE {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkDeathZone {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkJOYSTICK {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkDROP {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkNotPressed {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkCONTROLLER {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkIN  {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkTO  {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkPLAY {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkLOCK {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkUNLOCK {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkSPAWN {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkGameOver {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkKeepPlaying {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkKILL {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkMONSTER {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkBOSS {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkProgramName {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkID {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkIDTipo {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkCARACTER {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkSTRINGS {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkINT {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkFLOAT {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkDivEntera {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkOR {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkAND {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkLessEqual {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkEQUAL {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkNotEqual {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkGreaterEqual {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkOpenList {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkCloseList {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkOpenListIndex {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkCloseListIndex {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkINCREMENT {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkDECREMENT {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkSUM {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkMIN {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkMULT {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkDIV {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkMOD {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkLEN {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkREF {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkNOT {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkLessThan {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkGreaterThan {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkPUFF {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkOpenParenthesis {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkCloseParenthesis {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkOpenBrackets {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkCloseBrackets {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkCOMA {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkANEXO {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkGUARD {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkASING {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkUPPER {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkLOWER {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkCMV {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkCM1 {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkFIN {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkOpenArray {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkCloseArray {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkOpenArrayIndex {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkCloseArrayIndex {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkError {mensaje :: String}
           | TkCONCAT {getAlexPosn :: AlexPosn,getTknStr :: String}
           | TkEndLine {getAlexPosn :: AlexPosn,getTknStr :: String}
           deriving (Eq)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--                       Manejo de los tokens erroneos
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

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
    show (TkWORLD tk p) = "(" ++ tk ++ "), posicion " ++ show p -- world
    show (TkOF tk p) = "(" ++ tk ++ "), posicion " ++ show p -- of
    show (TkEndLine tk p) = "(" ++ tk ++  "), posicion " ++ show p -- New line
    show (TkBUTTON tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Button
    show (TkRUNE tk p) = "(" ++ tk ++ "), posicion " ++ show p -- rune
    show (TkLOSE tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Lose
    show (TkWIN tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Win
    show (TkBATLE tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Battle
    show (TkPOWER tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Power
    show (TkSKILL tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Skill
    show (TkRUNES tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Runes
    show (TkCONCAT tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Concatenación de lista
    show (TkKIT tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Kit
    show (TkINVENTORY tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Inventory
    show (TkITEMS tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Items
    show (TkSUMMON tk p) = "(" ++ tk ++ "), posicion " ++ show p -- summon
    show (TkFREE tk p) = "(" ++ tk ++ "), posicion " ++ show p -- free
    show (TkDeathZone tk p) = "(" ++ tk ++ "), posicion " ++ show p -- DeathZone
    show (TkJOYSTICK tk p) = "(" ++ tk ++ "), posicion " ++ show p -- joystick
    show (TkDROP tk p) = "(" ++ tk ++ "), posicion " ++ show p -- drop
    show (TkNotPressed tk p) = "(" ++ tk ++ "), posicion " ++ show p -- notPressed
    show (TkCONTROLLER tk p) = "(" ++ tk ++ "), posicion " ++ show p -- controller
    show (TkIN tk p)  = "(" ++ tk ++ "), posicion " ++ show p -- <-
    show (TkTO tk p)  = "(" ++ tk ++ "), posicion " ++ show p -- ->
    show (TkPLAY tk p) = "(" ++ tk ++ "), posicion " ++ show p -- play
    show (TkLOCK tk p) = "(" ++ tk ++ "), posicion " ++ show p -- lock
    show (TkUNLOCK tk p) = "(" ++ tk ++ "), posicion " ++ show p -- unlock
    show (TkSPAWN tk p) = "(" ++ tk ++ "), posicion " ++ show p -- spawn
    show (TkGameOver tk p) = "(" ++ tk ++ "), posicion " ++ show p -- gameOver
    show (TkKeepPlaying tk p) = "(" ++ tk ++ "), posicion " ++ show p -- keepPlaying
    show (TkKILL tk p) = "(" ++ tk ++ "), posicion " ++ show p -- kill
    show (TkMONSTER tk p) = "(" ++ tk ++ "), posicion " ++ show p -- monster
    show (TkBOSS tk p) = "(" ++ tk ++ "), posicion " ++ show p -- boss
    show (TkProgramName tk p) = "(" ++ tk ++ "), posicion " ++ show p -- Nombre programa
    show (TkID tk p) = "Identificador \"" ++ tk ++ "\", posicion " ++ show p -- Id
    show (TkIDTipo tk p) = "Identificador de tipo \"" ++ tk ++ "\", posicion " ++ show p -- Id
    show (TkCARACTER tk p _) = "Caracter '" ++ tk ++ "', posicion " ++ show p -- carActer
    show (TkSTRINGS tk p) = "String \"" ++ tk ++ "\", posicion " ++ show p -- String
    show (TkINT tk p _) = "Entero " ++ tk ++ ", posicion " ++ show p -- Entero
    show (TkFLOAT tk p _) = "Flotante " ++ tk ++ ", posicion " ++ show p -- Flotante
    show (TkDivEntera tk p) = "(" ++ tk ++ "), posicion " ++ show p -- //
    show (TkOR tk p) = "(" ++ tk ++ "), posicion " ++ show p -- ||
    show (TkAND tk p) = "(" ++ tk ++ "), posicion " ++ show p -- &&
    show (TkLessEqual tk p) = "(" ++ tk ++ "), posicion " ++ show p -- <=
    show (TkEQUAL tk p) = "(" ++ tk ++ "), posicion " ++ show p -- ==
    show (TkNotEqual tk p) = "(" ++ tk ++ "), posicion " ++ show p -- !=
    show (TkGreaterEqual tk p) = "(" ++ tk ++ "), posicion " ++ show p -- >=
    show (TkOpenList tk p) = "(" ++ tk ++ "), posicion " ++ show p -- <<
    show (TkCloseList tk p) = "(" ++ tk ++ "), posicion " ++ show p -- >>
    show (TkOpenListIndex tk p) = "(" ++ tk ++ "), posicion " ++ show p -- |>
    show (TkCloseListIndex tk p) = "(" ++ tk ++ "), posicion " ++ show p -- <°
    show (TkINCREMENT tk p) = "(" ++ tk ++ "), posicion " ++ show p -- ++
    show (TkDECREMENT tk p) = "(" ++ tk ++ "), posicion " ++ show p -- --
    show (TkSUM tk p) = "(" ++ tk ++ "), posicion " ++ show p -- +
    show (TkMIN tk p) = "(" ++ tk ++ "), posicion " ++ show p -- -
    show (TkMULT tk p) = "(" ++ tk ++ "), posicion " ++ show p -- *
    show (TkDIV tk p) = "(" ++ tk ++ "), posicion " ++ show p -- /
    show (TkMOD tk p) = "(" ++ tk ++ "), posicion " ++ show p -- %
    show (TkLEN tk p) = "(" ++ tk ++ "), posicion " ++ show p -- #
    show (TkREF tk p) = "(" ++ tk ++ "), posicion " ++ show p -- ?
    show (TkNOT tk p) = "(" ++ tk ++ "), posicion " ++ show p -- !
    show (TkLessThan tk p) = "(" ++ tk ++ "), posicion " ++ show p -- <
    show (TkGreaterThan tk p) = "(" ++ tk ++ "), posicion " ++ show p -- >
    show (TkPUFF tk p) = "(" ++ tk ++ "), posicion " ++ show p -- puff
    show (TkOpenParenthesis tk p) = "(" ++ tk ++ "), posicion " ++ show p -- (
    show (TkCloseParenthesis tk p) = "(" ++ tk ++ "), posicion " ++ show p -- )
    show (TkOpenBrackets tk p) = "(" ++ tk ++ "), posicion " ++ show p -- {
    show (TkCloseBrackets tk p) = "(" ++ tk ++ "), posicion " ++ show p -- }
    show (TkCOMA tk p) = "(" ++ tk ++ "), posicion " ++ show p -- ,
    show (TkANEXO tk p) = "(" ++ tk ++ "), posicion " ++ show p -- :
    show (TkGUARD tk p) = "(" ++ tk ++ "), posicion " ++ show p -- |
    show (TkASING tk p) = "(" ++ tk ++ "), posicion " ++ show p -- =
    show (TkUPPER tk p) = "(" ++ tk ++ "), posicion " ++ show p -- ^
    show (TkLOWER tk p) = "(" ++ tk ++ "), posicion " ++ show p -- .
    show (TkFIN tk p) = "(" ++ tk ++ "), posicion " ++ show p -- .~
    show (TkOpenArray tk p) = "(" ++ tk ++ "), posicion " ++ show p -- "|}"
    show (TkCloseArray tk p) = "(" ++ tk ++ "), posicion " ++ show p -- "{|"
    show (TkOpenArrayIndex tk p) = "(" ++ tk ++ "), posicion " ++ show p -- "|)"
    show (TkCloseArrayIndex tk p) = "(" ++ tk ++ "), posicion " ++ show p -- "(|"
    show (TkError msj) = msj
}
