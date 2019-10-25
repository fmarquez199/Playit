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
    hasError,
    isError
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
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--                         Creacion de los tokens
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

createTkWORLD (AlexPn _ f c) tk = TkWORLD tk (f, c)
createTkRUNE (AlexPn _ f c) tk = TkRUNE tk (f, c)
createTkLOSE (AlexPn _ f c) tk = TkLOSE tk (f, c)
createTkOF (AlexPn _ f c) tk = TkOF tk (f, c)
createTkBUTTON (AlexPn _ f c) tk = TkBUTTON tk (f, c)
createTkWIN (AlexPn _ f c) tk = TkWIN tk (f, c)
createTkBATLE (AlexPn _ f c) tk = TkBATLE tk (f, c)
createTkPOWER (AlexPn _ f c) tk = TkPOWER tk (f, c)
createTkSKILL (AlexPn _ f c) tk = TkSKILL tk (f, c)
createTkRUNES (AlexPn _ f c) tk = TkRUNES tk (f, c)
createTkKIT (AlexPn _ f c) tk = TkKIT tk (f, c)
createTkINVENTORY (AlexPn _ f c) tk = TkINVENTORY tk (f, c)
createTkITEMS (AlexPn _ f c) tk = TkITEMS tk (f, c)
createTkSUMMON (AlexPn _ f c) tk = TkSUMMON tk (f, c)
createTkFREE (AlexPn _ f c) tk = TkFREE tk (f, c)
createTkDeathZone (AlexPn _ f c) tk = TkDeathZone tk (f, c)
createTkJOYSTICK (AlexPn _ f c) tk = TkJOYSTICK tk (f, c)
createTkDROP (AlexPn _ f c) tk = TkDROP tk (f, c)
createTkNotPressed (AlexPn _ f c) tk = TkNotPressed tk (f, c)
createTkCONTROLLER (AlexPn _ f c) tk = TkCONTROLLER tk (f, c)
createTkIN (AlexPn _ f c) tk = TkIN tk (f, c)
createTkTO (AlexPn _ f c) tk = TkTO tk (f, c)
createTkPLAY (AlexPn _ f c) tk = TkPLAY tk (f, c)
createTkLOCK (AlexPn _ f c) tk = TkLOCK tk (f, c)
createTkUNLOCK (AlexPn _ f c) tk = TkUNLOCK tk (f, c)
createTkSPAWN (AlexPn _ f c) tk = TkSPAWN tk (f, c)
createTkGameOver (AlexPn _ f c) tk = TkGameOver tk (f, c)
createTkKeepPlaying (AlexPn _ f c) tk = TkKeepPlaying tk (f, c)
createTkKILL (AlexPn _ f c) tk = TkKILL tk (f, c)
createTkMONSTER (AlexPn _ f c) tk = TkMONSTER tk (f, c)
createTkBOSS (AlexPn _ f c) tk = TkBOSS tk (f, c)
createTkProgramName (AlexPn _ f c) tk = TkProgramName tk (f, c)
createTkID (AlexPn _ f c) id = TkID id (f, c)
createTkIDTipo (AlexPn _ f c) idTipo = TkIDTipo idTipo (f, c)
createTkCARACTER (AlexPn _ f c) char =
    TkCARACTER char (f, c) (read (map (\c -> if c == '*' then '\'' else c) char) :: Char)
createTkSTRINGS (AlexPn _ f c) str = TkSTRINGS str (f, c)
createTkINT (AlexPn _ f c) int = TkINT int (f, c) (read int :: Int)
createTkFLOAT (AlexPn _ f c) float =
    TkFLOAT float (f, c) (read (map (\c -> if c == '\'' then '.' else c) float) :: Float)
createTkDivEntera (AlexPn _ f c) tk = TkDivEntera tk (f, c)
createTkOR (AlexPn _ f c) tk = TkOR tk (f, c)
createTkAND (AlexPn _ f c) tk = TkAND tk (f, c)
createTkLessEqual (AlexPn _ f c) tk = TkLessEqual tk (f, c)
createTkEQUAL (AlexPn _ f c) tk = TkEQUAL tk (f, c)
createTkNotEqual (AlexPn _ f c) tk = TkNotEqual tk (f, c)
createTkGreaterEqual (AlexPn _ f c) tk = TkGreaterEqual tk (f, c)
createTkOpenList (AlexPn _ f c) tk = TkOpenList tk (f, c)
createTkCloseList (AlexPn _ f c) tk = TkCloseList tk (f, c)
createTkOpenListIndex (AlexPn _ f c) tk = TkOpenListIndex tk (f, c)
createTkCloseListIndex (AlexPn _ f c) tk = TkCloseListIndex tk (f, c)
createTkINCREMENT (AlexPn _ f c) tk = TkINCREMENT tk (f, c)
createTkDECREMENT (AlexPn _ f c) tk = TkDECREMENT tk (f, c)
createTkSUM (AlexPn _ f c) tk = TkSUM tk (f, c)
createTkMIN (AlexPn _ f c) tk = TkMIN tk (f, c)
createTkMULT (AlexPn _ f c) tk = TkMULT tk (f, c)
createTkDIV (AlexPn _ f c) tk = TkDIV tk (f, c)
createTkMOD (AlexPn _ f c) tk = TkMOD tk (f, c)
createTkLEN (AlexPn _ f c) tk = TkLEN tk (f, c)
createTkREF (AlexPn _ f c) tk = TkREF tk (f, c)
createTkNOT (AlexPn _ f c) tk = TkNOT tk (f, c)
createTkLessThan (AlexPn _ f c) tk = TkLessThan tk (f, c)
createTkGreaterThan (AlexPn _ f c) tk = TkGreaterThan tk (f, c)
createTkPUFF (AlexPn _ f c) tk = TkPUFF tk (f, c)
createTkOpenParenthesis (AlexPn _ f c) tk = TkOpenParenthesis tk (f, c)
createTkCloseParenthesis (AlexPn _ f c) tk = TkCloseParenthesis tk (f, c)
createTkOpenBrackets (AlexPn _ f c) tk = TkOpenBrackets tk (f, c)
createTkCloseBrackets (AlexPn _ f c) tk = TkCloseBrackets tk (f, c)
createTkCOMA (AlexPn _ f c) tk = TkCOMA tk (f, c)
createTkANEXO (AlexPn _ f c) tk = TkANEXO tk (f, c)
createTkGUARD (AlexPn _ f c) tk = TkGUARD tk (f, c)
createTkASING (AlexPn _ f c) tk = TkASING tk (f, c)
createTkUPPER (AlexPn _ f c) tk = TkUPPER tk (f, c)
createTkLOWER (AlexPn _ f c) tk = TkLOWER tk (f, c)
createTkCMV (AlexPn _ f c) tk = TkCMV tk (f, c)
createTkCM1 (AlexPn _ f c) tk = TkCM1 tk (f, c)
createTkFIN (AlexPn _ f c) tk = TkFIN tk (f, c)
createTkOpenArray (AlexPn _ f c) tk = TkOpenArray tk (f, c)
createTkCloseArray (AlexPn _ f c) tk = TkCloseArray tk (f, c)
createTkOpenArrayIndex (AlexPn _ f c) tk = TkOpenArrayIndex tk (f, c)
createTkCloseArrayIndex (AlexPn _ f c) tk = TkCloseArrayIndex tk (f, c)
createTkCONCAT (AlexPn _ f c) tk = TkCONCAT tk (f, c)
createTkEndLine (AlexPn _ f c) tk = TkEndLine tk (f, c)
createTkError alex_pos err = TkError $ tokerr err alex_pos

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
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
           | TkError {mensaje :: String}
           | TkCONCAT {getTk :: String, getPos :: (Int, Int)}
           | TkEndLine {getTk :: String, getPos :: (Int, Int)}
           deriving (Eq)

instance Show Token where
    show (TkWORLD tk p) = "(" ++ tk ++ "), posicion " ++ show p -- world
    show (TkOF tk p) = "(" ++ tk ++ "), posicion " ++ show p -- of
    show (TkEndLine tk p) = "(" ++ tk ++  )", posicion " ++ show p -- New line
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