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
    alexScanTokens,
    hasError,
    showAllErrors,
    tokerr
) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
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

  Battle               { tok (\(AlexPn _ f c) tk -> TkBATLE tk (f,c)) }
  DeathZone            { tok (\(AlexPn _ f c) tk -> TkDeathZone tk (f,c)) }
  Inventory            { tok (\(AlexPn _ f c) tk -> TkINVENTORY tk (f,c)) }
  Items                { tok (\(AlexPn _ f c) tk -> TkITEMS tk (f,c)) }
  Kit                  { tok (\(AlexPn _ f c) tk -> TkKIT tk (f,c)) }
  Power                { tok (\(AlexPn _ f c) tk -> TkPOWER tk (f,c)) }
  Rune                 { tok (\(AlexPn _ f c) tk -> TkRUNE tk (f,c)) }
  Runes                { tok (\(AlexPn _ f c) tk -> TkRUNES tk (f,c)) }
  Skill                { tok (\(AlexPn _ f c) tk -> TkSKILL tk (f,c)) }
  Button               { tok (\(AlexPn _ f c) tk -> TkBUTTON tk (f,c)) }
  boss                 { tok (\(AlexPn _ f c) tk -> TkBOSS tk (f,c)) }
  controller           { tok (\(AlexPn _ f c) tk -> TkCONTROLLER tk (f,c)) }
  drop                 { tok (\(AlexPn _ f c) tk -> TkDROP tk (f,c)) }
  notPressed           { tok (\(AlexPn _ f c) tk -> TkNotPressed tk (f,c)) }
  free                 { tok (\(AlexPn _ f c) tk -> TkFREE tk (f,c)) }
  gameOver             { tok (\(AlexPn _ f c) tk -> TkGameOver tk (f,c)) }
  joystick             { tok (\(AlexPn _ f c) tk -> TkJOYSTICK tk (f,c)) }
  keepPlaying          { tok (\(AlexPn _ f c) tk -> TkKeepPlaying tk (f,c)) }
  kill                 { tok (\(AlexPn _ f c) tk -> TkKILL tk (f,c)) }
  lock                 { tok (\(AlexPn _ f c) tk -> TkLOCK tk (f,c)) }
  monster              { tok (\(AlexPn _ f c) tk -> TkMONSTER tk (f,c)) }
  play                 { tok (\(AlexPn _ f c) tk -> TkPLAY tk (f,c)) }
  puff                 { tok (\(AlexPn _ f c) tk -> TkPUFF tk (f,c)) }
  spawn                { tok (\(AlexPn _ f c) tk -> TkSPAWN tk (f,c)) }
  summon               { tok (\(AlexPn _ f c) tk -> TkSUMMON tk (f,c)) }
  unlock               { tok (\(AlexPn _ f c) tk -> TkUNLOCK tk (f,c)) }
  world                { tok (\(AlexPn _ f c) tk -> TkWORLD tk (f,c)) }
  of                   { tok (\(AlexPn _ f c) tk -> TkOF tk (f,c)) }
  @endLine             { tok (\(AlexPn _ f c) tk -> TkEndLine tk (f,c)) }

  -- Literales booleanos
  
  Win                  { tok (\(AlexPn _ f c) tk -> TkWIN tk (f,c)) }
  Lose                 { tok (\(AlexPn _ f c) tk -> TkLOSE tk (f,c)) }

  -- Identificadores

  @programas           { tok (\(AlexPn _ f c) tk -> TkProgramName tk (f,c)) }
  @id                  { tok (\(AlexPn _ f c) tk -> TkID tk (f,c)) }
  @id_tipo             { tok (\(AlexPn _ f c) tk -> TkIDTipo tk (f,c)) }

  -- Caracteres

  @caracter            { createTkCARACTER }
  @strings             { tok (\(AlexPn _ f c) tk -> TkSTRINGS tk (f,c)) }
  
  -- Literares numericos
  
  $digitos+            { createTkINT }
  @float               { createTkFLOAT }

  -- Simbolos

  ".~"                 { tok (\(AlexPn _ f c) tk -> TkFIN tk (f,c)) }
  "//"                 { tok (\(AlexPn _ f c) tk -> TkDivEntera tk (f,c)) }
  "||"                 { tok (\(AlexPn _ f c) tk -> TkOR tk (f,c)) }
  "&&"                 { tok (\(AlexPn _ f c) tk -> TkAND tk (f,c)) }
  "<="                 { tok (\(AlexPn _ f c) tk -> TkLessEqual tk (f,c)) }
  "=="                 { tok (\(AlexPn _ f c) tk -> TkEQUAL tk (f,c)) }
  "!="                 { tok (\(AlexPn _ f c) tk -> TkNotEqual tk (f,c)) }
  ">="                 { tok (\(AlexPn _ f c) tk -> TkGreaterEqual tk (f,c)) }
  "<<"                 { tok (\(AlexPn _ f c) tk -> TkOpenList tk (f,c)) }
  ">>"                 { tok (\(AlexPn _ f c) tk -> TkCloseList tk (f,c)) }
  "|>"                 { tok (\(AlexPn _ f c) tk -> TkOpenListIndex tk (f,c)) }
  "<|"                 { tok (\(AlexPn _ f c) tk -> TkCloseListIndex tk (f,c)) }
  "++"                 { tok (\(AlexPn _ f c) tk -> TkINCREMENT tk (f,c)) }
  "--"                 { tok (\(AlexPn _ f c) tk -> TkDECREMENT tk (f,c)) }
  "<-"                 { tok (\(AlexPn _ f c) tk -> TkIN tk (f,c)) }
  "->"                 { tok (\(AlexPn _ f c) tk -> TkTO tk (f,c)) }
  "|}"                 { tok (\(AlexPn _ f c) tk -> TkOpenArray tk (f,c)) }
  "{|"                 { tok (\(AlexPn _ f c) tk -> TkCloseArray tk (f,c)) }
  "|)"                 { tok (\(AlexPn _ f c) tk -> TkOpenArrayIndex tk (f,c)) }
  "(|"                 { tok (\(AlexPn _ f c) tk -> TkCloseArrayIndex tk (f,c)) }
  "+"                  { tok (\(AlexPn _ f c) tk -> TkSUM tk (f,c)) }
  "-"                  { tok (\(AlexPn _ f c) tk -> TkMIN tk (f,c)) }
  "*"                  { tok (\(AlexPn _ f c) tk -> TkMULT tk (f,c)) }
  "/"                  { tok (\(AlexPn _ f c) tk -> TkDIV tk (f,c)) }
  "%"                  { tok (\(AlexPn _ f c) tk -> TkMOD tk (f,c)) }
  "#"                  { tok (\(AlexPn _ f c) tk -> TkLEN tk (f,c)) }
  "?"                  { tok (\(AlexPn _ f c) tk -> TkREF tk (f,c)) }
  "!"                  { tok (\(AlexPn _ f c) tk -> TkNOT tk (f,c)) }
  "<"                  { tok (\(AlexPn _ f c) tk -> TkLessThan tk (f,c)) }
  ">"                  { tok (\(AlexPn _ f c) tk -> TkGreaterThan tk (f,c)) }
  "("                  { tok (\(AlexPn _ f c) tk -> TkOpenParenthesis tk (f,c)) }
  ")"                  { tok (\(AlexPn _ f c) tk -> TkCloseParenthesis tk (f,c)) }
  "{"                  { tok (\(AlexPn _ f c) tk -> TkOpenBrackets tk (f,c)) }
  "}"                  { tok (\(AlexPn _ f c) tk -> TkCloseBrackets tk (f,c)) }
  ","                  { tok (\(AlexPn _ f c) tk -> TkCOMA tk (f,c)) }
  ":"                  { tok (\(AlexPn _ f c) tk -> TkANEXO tk (f,c)) }
  "::"                 { tok (\(AlexPn _ f c) tk -> TkCONCAT tk (f,c)) }
  "|"                  { tok (\(AlexPn _ f c) tk -> TkGUARD tk (f,c)) }
  "="                  { tok (\(AlexPn _ f c) tk -> TkASING tk (f,c)) }
  "^"                  { tok (\(AlexPn _ f c) tk -> TkUPPER tk (f,c)) }
  "."                  { tok (\(AlexPn _ f c) tk -> TkLOWER tk (f,c)) }
  
  -- Comentarios

  @comments            ;
  @comment             ;

  -- Caracteres invalidos

  @error               { tok (\(AlexPn _ f c) err -> TkError err (f,c)) }

{
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                         Creacion de los tokens
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

tok :: (AlexPosn -> String -> Token) -> AlexPosn -> String -> Token
tok f p tk = f p tk

createTkCARACTER (AlexPn _ f c) char =
    TkCARACTER char (f, c) (read (map (\c -> if c == '*' then '\'' else c) char) :: Char)
createTkINT (AlexPn _ f c) int = TkINT int (f, c) (read int :: Int)
createTkFLOAT (AlexPn _ f c) float =
    TkFLOAT float (f, c) (read (map (\c -> if c == '\'' then '.' else c) float) :: Float)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                       Manejo de los tokens erroneos
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- 'hasError' : Obtiene todos los tokens erroneos.
hasError :: [Token] -> (Bool, [(Int,Int)])
hasError [] = (False, [(-1::Int, -1::Int)])
hasError ((TkError _ p):tks) = (True, p : map isError tks)
hasError (_:tks) = hasError tks

--  'isError' : Obtiene la posicion de un token erroneo.
isError :: Token -> (Int,Int)
isError (TkError _ p) = p
isError _ = (-1::Int, -1::Int)

-- 'tokerr' : Traduce los errores a un formato.
tokerr :: String -> (Int,Int) -> String
tokerr code (l,c) = 
  "\n\t\x1b[1;52;94m¡¡¡PLAYIT FATALITY!!!\n" ++ fs
  
  where
    allLines = splitOn "\n" code
    maxSize = foldl max (-1) $ map length allLines
    buildRuler = flip replicate '.'
    rule = buildRuler maxSize ++ "\n"
    relevantLines = drop (l-1) allLines
    firstLine = head relevantLines ++ "\n"
    errorRuler = "\t\x1b[1;93m" ++ buildRuler (c-1) ++ "\x1b[5;31m^\x1b[25;93m"
      ++ {-buildRuler (maxSize - c) ++-} "\n"
    fs = "\x1b[93m| " ++ show l ++ "\t\x1b[0;96m" ++ firstLine ++ errorRuler


-- 'showAllErrors': Muestra todos los errores lexicos encontrados.
showAllErrors :: String -> [(Int,Int)] -> String
showAllErrors code [] = ""
showAllErrors code ((-1, -1):pos) = showAllErrors code pos
showAllErrors code (p:pos) = concat $ tokerr code p : [showAllErrors code pos]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                      Tipos de datos para los los tokens
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

data Token = TkWORLD {getTk :: String, getPos :: (Int, Int)}
           | TkRUNE {getTk :: String, getPos :: (Int, Int)}
           | TkLOSE {getTk :: String, getPos :: (Int, Int)}
           | TkOF {getTk :: String, getPos :: (Int, Int)}
           | TkBUTTON {getTk :: String, getPos :: (Int, Int)}
           | TkWIN {getTk :: String, getPos :: (Int, Int)}
           | TkBATLE {getTk :: String, getPos :: (Int, Int)}
           | TkPOWER {getTk :: String, getPos :: (Int, Int)}
           | TkSKILL {getTk :: String, getPos :: (Int, Int)}
           | TkRUNES {getTk :: String, getPos :: (Int, Int)}
           | TkKIT {getTk :: String, getPos :: (Int, Int)}
           | TkINVENTORY {getTk :: String, getPos :: (Int, Int)}
           | TkITEMS {getTk :: String, getPos :: (Int, Int)}
           | TkSUMMON {getTk :: String, getPos :: (Int, Int)}
           | TkFREE {getTk :: String, getPos :: (Int, Int)}
           | TkDeathZone {getTk :: String, getPos :: (Int, Int)}
           | TkJOYSTICK {getTk :: String, getPos :: (Int, Int)}
           | TkDROP {getTk :: String, getPos :: (Int, Int)}
           | TkNotPressed {getTk :: String, getPos :: (Int, Int)}
           | TkCONTROLLER {getTk :: String, getPos :: (Int, Int)}
           | TkIN  {getTk :: String, getPos :: (Int, Int)}
           | TkTO  {getTk :: String, getPos :: (Int, Int)}
           | TkPLAY {getTk :: String, getPos :: (Int, Int)}
           | TkLOCK {getTk :: String, getPos :: (Int, Int)}
           | TkUNLOCK {getTk :: String, getPos :: (Int, Int)}
           | TkSPAWN {getTk :: String, getPos :: (Int, Int)}
           | TkGameOver {getTk :: String, getPos :: (Int, Int)}
           | TkKeepPlaying {getTk :: String, getPos :: (Int, Int)}
           | TkKILL {getTk :: String, getPos :: (Int, Int)}
           | TkMONSTER {getTk :: String, getPos :: (Int, Int)}
           | TkBOSS {getTk :: String, getPos :: (Int, Int)}
           | TkProgramName {getTk :: String, getPos :: (Int, Int)}
           | TkID {getTk :: String, getPos :: (Int, Int)}
           | TkIDTipo {getTk :: String, getPos :: (Int, Int)}
           | TkCARACTER {getTk :: String, getPos :: (Int, Int), getChar :: Char}
           | TkSTRINGS {getTk :: String, getPos :: (Int, Int)}
           | TkINT {getTk :: String, getPos :: (Int, Int), getInt :: Int}
           | TkFLOAT {getTk :: String, getPos :: (Int, Int), getFloat :: Float}
           | TkDivEntera {getTk :: String, getPos :: (Int, Int)}
           | TkOR {getTk :: String, getPos :: (Int, Int)}
           | TkAND {getTk :: String, getPos :: (Int, Int)}
           | TkLessEqual {getTk :: String, getPos :: (Int, Int)}
           | TkEQUAL {getTk :: String, getPos :: (Int, Int)}
           | TkNotEqual {getTk :: String, getPos :: (Int, Int)}
           | TkGreaterEqual {getTk :: String, getPos :: (Int, Int)}
           | TkOpenList {getTk :: String, getPos :: (Int, Int)}
           | TkCloseList {getTk :: String, getPos :: (Int, Int)}
           | TkOpenListIndex {getTk :: String, getPos :: (Int, Int)}
           | TkCloseListIndex {getTk :: String, getPos :: (Int, Int)}
           | TkINCREMENT {getTk :: String, getPos :: (Int, Int)}
           | TkDECREMENT {getTk :: String, getPos :: (Int, Int)}
           | TkSUM {getTk :: String, getPos :: (Int, Int)}
           | TkMIN {getTk :: String, getPos :: (Int, Int)}
           | TkMULT {getTk :: String, getPos :: (Int, Int)}
           | TkDIV {getTk :: String, getPos :: (Int, Int)}
           | TkMOD {getTk :: String, getPos :: (Int, Int)}
           | TkLEN {getTk :: String, getPos :: (Int, Int)}
           | TkREF {getTk :: String, getPos :: (Int, Int)}
           | TkNOT {getTk :: String, getPos :: (Int, Int)}
           | TkLessThan {getTk :: String, getPos :: (Int, Int)}
           | TkGreaterThan {getTk :: String, getPos :: (Int, Int)}
           | TkPUFF {getTk :: String, getPos :: (Int, Int)}
           | TkOpenParenthesis {getTk :: String, getPos :: (Int, Int)}
           | TkCloseParenthesis {getTk :: String, getPos :: (Int, Int)}
           | TkOpenBrackets {getTk :: String, getPos :: (Int, Int)}
           | TkCloseBrackets {getTk :: String, getPos :: (Int, Int)}
           | TkCOMA {getTk :: String, getPos :: (Int, Int)}
           | TkANEXO {getTk :: String, getPos :: (Int, Int)}
           | TkGUARD {getTk :: String, getPos :: (Int, Int)}
           | TkASING {getTk :: String, getPos :: (Int, Int)}
           | TkUPPER {getTk :: String, getPos :: (Int, Int)}
           | TkLOWER {getTk :: String, getPos :: (Int, Int)}
           | TkCMV {getTk :: String, getPos :: (Int, Int)}
           | TkCM1 {getTk :: String, getPos :: (Int, Int)}
           | TkFIN {getTk :: String, getPos :: (Int, Int)}
           | TkOpenArray {getTk :: String, getPos :: (Int, Int)}
           | TkCloseArray {getTk :: String, getPos :: (Int, Int)}
           | TkOpenArrayIndex {getTk :: String, getPos :: (Int, Int)}
           | TkCloseArrayIndex {getTk :: String, getPos :: (Int, Int)}
           | TkError {getTk :: String, getPos :: (Int, Int)}
           | TkCONCAT {getTk :: String, getPos :: (Int, Int)}
           | TkEndLine {getTk :: String, getPos :: (Int, Int)}
           deriving (Eq)

instance Show Token where
  show (TkWORLD tk p) = "(" ++ tk ++ "), pos " ++ show p -- world
  show (TkOF tk p) = "(" ++ tk ++ "), pos " ++ show p -- of
  show (TkEndLine tk p) = "(\\n), pos " ++ show p -- New line
  show (TkBUTTON tk p) = "(" ++ tk ++ "), pos " ++ show p -- Button
  show (TkRUNE tk p) = "(" ++ tk ++ "), pos " ++ show p -- rune
  show (TkLOSE tk p) = "(" ++ tk ++ "), pos " ++ show p -- Lose
  show (TkWIN tk p) = "(" ++ tk ++ "), pos " ++ show p -- Win
  show (TkBATLE tk p) = "(" ++ tk ++ "), pos " ++ show p -- Battle
  show (TkPOWER tk p) = "(" ++ tk ++ "), pos " ++ show p -- Power
  show (TkSKILL tk p) = "(" ++ tk ++ "), pos " ++ show p -- Skill
  show (TkRUNES tk p) = "(" ++ tk ++ "), pos " ++ show p -- Runes
  show (TkCONCAT tk p) = "(" ++ tk ++ "), pos " ++ show p -- Concatenación de lista
  show (TkKIT tk p) = "(" ++ tk ++ "), pos " ++ show p -- Kit
  show (TkINVENTORY tk p) = "(" ++ tk ++ "), pos " ++ show p -- Inventory
  show (TkITEMS tk p) = "(" ++ tk ++ "), pos " ++ show p -- Items
  show (TkSUMMON tk p) = "(" ++ tk ++ "), pos " ++ show p -- summon
  show (TkFREE tk p) = "(" ++ tk ++ "), pos " ++ show p -- free
  show (TkDeathZone tk p) = "(" ++ tk ++ "), pos " ++ show p -- DeathZone
  show (TkJOYSTICK tk p) = "(" ++ tk ++ "), pos " ++ show p -- joystick
  show (TkDROP tk p) = "(" ++ tk ++ "), pos " ++ show p -- drop
  show (TkNotPressed tk p) = "(" ++ tk ++ "), pos " ++ show p -- notPressed
  show (TkCONTROLLER tk p) = "(" ++ tk ++ "), pos " ++ show p -- controller
  show (TkIN tk p)  = "(" ++ tk ++ "), pos " ++ show p -- <-
  show (TkTO tk p)  = "(" ++ tk ++ "), pos " ++ show p -- ->
  show (TkPLAY tk p) = "(" ++ tk ++ "), pos " ++ show p -- play
  show (TkLOCK tk p) = "(" ++ tk ++ "), pos " ++ show p -- lock
  show (TkUNLOCK tk p) = "(" ++ tk ++ "), pos " ++ show p -- unlock
  show (TkSPAWN tk p) = "(" ++ tk ++ "), pos " ++ show p -- spawn
  show (TkGameOver tk p) = "(" ++ tk ++ "), pos " ++ show p -- gameOver
  show (TkKeepPlaying tk p) = "(" ++ tk ++ "), pos " ++ show p -- keepPlaying
  show (TkKILL tk p) = "(" ++ tk ++ "), pos " ++ show p -- kill
  show (TkMONSTER tk p) = "(" ++ tk ++ "), pos " ++ show p -- monster
  show (TkBOSS tk p) = "(" ++ tk ++ "), pos " ++ show p -- boss
  show (TkProgramName tk p) = "(" ++ tk ++ "), pos " ++ show p -- Nombre programa
  show (TkID tk p) = "Identificador \"" ++ tk ++ "\", pos " ++ show p -- Id
  show (TkIDTipo tk p) = "Identificador de tipo \"" ++ tk ++ "\", pos " ++ show p -- Id
  show (TkCARACTER tk p _) = "Caracter '" ++ tk ++ "', pos " ++ show p -- carActer
  show (TkSTRINGS tk p) = "String \"" ++ tk ++ "\", pos " ++ show p -- String
  show (TkINT tk p _) = "Entero " ++ tk ++ ", pos " ++ show p -- Entero
  show (TkFLOAT tk p _) = "Flotante " ++ tk ++ ", pos " ++ show p -- Flotante
  show (TkDivEntera tk p) = "(" ++ tk ++ "), pos " ++ show p -- //
  show (TkOR tk p) = "(" ++ tk ++ "), pos " ++ show p -- ||
  show (TkAND tk p) = "(" ++ tk ++ "), pos " ++ show p -- &&
  show (TkLessEqual tk p) = "(" ++ tk ++ "), pos " ++ show p -- <=
  show (TkEQUAL tk p) = "(" ++ tk ++ "), pos " ++ show p -- ==
  show (TkNotEqual tk p) = "(" ++ tk ++ "), pos " ++ show p -- !=
  show (TkGreaterEqual tk p) = "(" ++ tk ++ "), pos " ++ show p -- >=
  show (TkOpenList tk p) = "(" ++ tk ++ "), pos " ++ show p -- <<
  show (TkCloseList tk p) = "(" ++ tk ++ "), pos " ++ show p -- >>
  show (TkOpenListIndex tk p) = "(" ++ tk ++ "), pos " ++ show p -- |>
  show (TkCloseListIndex tk p) = "(" ++ tk ++ "), pos " ++ show p -- <°
  show (TkINCREMENT tk p) = "(" ++ tk ++ "), pos " ++ show p -- ++
  show (TkDECREMENT tk p) = "(" ++ tk ++ "), pos " ++ show p -- --
  show (TkSUM tk p) = "(" ++ tk ++ "), pos " ++ show p -- +
  show (TkMIN tk p) = "(" ++ tk ++ "), pos " ++ show p -- -
  show (TkMULT tk p) = "(" ++ tk ++ "), pos " ++ show p -- *
  show (TkDIV tk p) = "(" ++ tk ++ "), pos " ++ show p -- /
  show (TkMOD tk p) = "(" ++ tk ++ "), pos " ++ show p -- %
  show (TkLEN tk p) = "(" ++ tk ++ "), pos " ++ show p -- #
  show (TkREF tk p) = "(" ++ tk ++ "), pos " ++ show p -- ?
  show (TkNOT tk p) = "(" ++ tk ++ "), pos " ++ show p -- !
  show (TkLessThan tk p) = "(" ++ tk ++ "), pos " ++ show p -- <
  show (TkGreaterThan tk p) = "(" ++ tk ++ "), pos " ++ show p -- >
  show (TkPUFF tk p) = "(" ++ tk ++ "), pos " ++ show p -- puff
  show (TkOpenParenthesis tk p) = "(" ++ tk ++ "), pos " ++ show p -- (
  show (TkCloseParenthesis tk p) = "(" ++ tk ++ "), pos " ++ show p -- )
  show (TkOpenBrackets tk p) = "(" ++ tk ++ "), pos " ++ show p -- {
  show (TkCloseBrackets tk p) = "(" ++ tk ++ "), pos " ++ show p -- }
  show (TkCOMA tk p) = "(" ++ tk ++ "), pos " ++ show p -- ,
  show (TkANEXO tk p) = "(" ++ tk ++ "), pos " ++ show p -- :
  show (TkGUARD tk p) = "(" ++ tk ++ "), pos " ++ show p -- |
  show (TkASING tk p) = "(" ++ tk ++ "), pos " ++ show p -- =
  show (TkUPPER tk p) = "(" ++ tk ++ "), pos " ++ show p -- ^
  show (TkLOWER tk p) = "(" ++ tk ++ "), pos " ++ show p -- .
  show (TkFIN tk p) = "(" ++ tk ++ "), pos " ++ show p -- .~
  show (TkOpenArray tk p) = "(" ++ tk ++ "), pos " ++ show p -- "|}"
  show (TkCloseArray tk p) = "(" ++ tk ++ "), pos " ++ show p -- "{|"
  show (TkOpenArrayIndex tk p) = "(" ++ tk ++ "), pos " ++ show p -- "|)"
  show (TkCloseArrayIndex tk p) = "(" ++ tk ++ "), pos " ++ show p -- "(|"
  show (TkError tk p) = tk
}