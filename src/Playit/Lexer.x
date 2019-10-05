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
    alexScanTokens
) where
}

%wrapper "posn"

-- Conjuntos de caracteres

$digitos        = [0-9]
$abecedario     = [a-zA-Z]
$simbolos       = [\! \" \# \$ \% \& \' \( \) \* \+ \, \- \. \/ \: \; \< \= \> \? \@]
$especial       = [\[ \\ \] \^ \_ \` \{ \| \} \~ '\0' '\t' '\n' '\\' '\'' '\"' '\~' '\*']
$validos        = [$digitos $abecedario $simbolos $especial $white]
$comentarios    = [$validos ~$validos]
$char_texto     = [$validos # [\* \~ \\]]   -- arreglar
$char_id        = [$digitos $abecedario \_ \']

-- Expresiones regulares

-- @texto PUEDE SER CUALQUIER caracter MENOS el caracter ~ y *
--  O PUEDE SER el caracter \*

@scape          = "\\" | "\0" | "\n" | "\t" | "\~" | "\*"
@mcaracter      = $char_texto | @scape
@caracter       = "*" @mcaracter "*"
@texto          = @mcaracter*
@identificador  = $abecedario $char_id*
@programas      = \% $char_id+ \%
@strings        = \~ @texto \~
@end_instruction= (\n)+
@float          = $digitos+ \' $digitos+
@comments       = \"\' [$comentarios $white]* \'\"
@comment        = \@ [$comentarios # \n]* \n 
@error          = .

tokens :-

  ([$white # \n])+               ;
  
  -- Palabras reservadas

  Battle               { tok (\p s -> TkBTL p s) } -- Booleano
  DeathZone            { tok (\p s -> TkDTZ p s) } -- Null pointer
  Inventory            { tok (\p s -> TkINV p s) } -- Registro
  Items                { tok (\p s -> TkITM p s) } -- Union
  Kit                  { tok (\p s -> TkKIT p s) } -- Lista
  Power                { tok (\p s -> TkPWR p s) } -- Entero
  Rune                 { tok (\p s -> TkRNE p s) } -- Char
  Runes                { tok (\p s -> TkRNS p s) } -- String
  Skill                { tok (\p s -> TkSKL p s) } -- Flotante
  Button               { tok (\p s -> TkBTN p s) } -- Condicional if
  boss                 { tok (\p s -> TkBSS p s) } -- procedimiento
  controller           { tok (\p s -> TkCTR p s) } -- Bucle for
  drop                 { tok (\p s -> TkDRP p s) } -- print
  notPressed           { tok (\p s -> TkNPR p s) } -- else
  free                 { tok (\p s -> TkFRE p s) } -- delete de C++
  gameOver             { tok (\p s -> TkGMO p s) } -- break de C
  joystick             { tok (\p s -> TkJST p s) } -- read 
  keepPlaying          { tok (\p s -> TkKPP p s) } -- continue de C
  kill                 { tok (\p s -> TkKLL p s) } -- llama a una funcion
  lock                 { tok (\p s -> TkLCK p s) } -- condicional del bucle do(play) while(lock){}
  monster              { tok (\p s -> TkMST p s) } -- Funcion
  play                 { tok (\p s -> TkPLY p s) } -- do de un bucle do(play) while(lock){}
  puff                 { tok (\p s -> TkAPT p s) } -- * de C para punteros
  spawn                { tok (\p s -> TkSPW p s) } -- . para registros de C
  summon               { tok (\p s -> TkSMN p s) } -- new de Java para punteros
  unlock               { tok (\p s -> TkNLK p s) } -- return para funciones
  world                { tok (\p s -> TkWRL p s) } -- void main() 
  of                   { tok (\p s -> TkOFK p s) } -- Declara el tipo de una lista
  @end_instruction     { tok (\p s -> TokenEndInstruction p s) }

  -- Literales booleanos
  
  Win                  { tok (\p s -> TkWIN p s) }
  Lose                 { tok (\p s -> TkLOS p s) }

  -- Identificadores

  @programas           { tok (\p s -> TkNMB p s) }
  @identificador       { tok (\p s -> TkIDF p s) }

  -- Caracteres

  @caracter            { tok (\p s -> TkCHA p s) }
  @strings             { tok (\p s -> TkSTG p s) }
  
  -- Literares numericos
  
  $digitos+            { tok (\p s -> TkINT p s) }
  @float               { tok (\p s -> TkFLT p s) }

  -- Simbolos

  ".~"                 { tok (\p s -> TkFIN p s) }
  "//"                 { tok (\p s -> TkIDV p s) }
  "||"                 { tok (\p s -> TkLOR p s) }
  "&&"                 { tok (\p s -> TkAND p s) }
  "<="                 { tok (\p s -> TkLET p s) }
  "=="                 { tok (\p s -> TkEQL p s) }
  "!="                 { tok (\p s -> TkNEQ p s) }
  ">="                 { tok (\p s -> TkGET p s) }
  "<<"                 { tok (\p s -> TkLSA p s) }
  ">>"                 { tok (\p s -> TkLSC p s) }
  "++"                 { tok (\p s -> TkINC p s) }
  "--"                 { tok (\p s -> TkDEC p s) }
  "<-"                 { tok (\p s -> TkIN  p s) }
  "->"                 { tok (\p s -> TkTO  p s) }
  "|}"                 { tok (\p s -> TkARA p s) }
  "{|"                 { tok (\p s -> TkARC p s) }
  "+"                  { tok (\p s -> TkSUM p s) }
  "-"                  { tok (\p s -> TkMIN p s) }
  "*"                  { tok (\p s -> TkTMS p s) }
  "/"                  { tok (\p s -> TkDVD p s) }
  "%"                  { tok (\p s -> TkMOD p s) }
  "#"                  { tok (\p s -> TkLEN p s) }
  "?"                  { tok (\p s -> TkREF p s) }
  "!"                  { tok (\p s -> TkEXC p s) }
  "<"                  { tok (\p s -> TkLTH p s) }
  ">"                  { tok (\p s -> TkGTH p s) }
  "("                  { tok (\p s -> TkPRA p s) }
  ")"                  { tok (\p s -> TkPRC p s) }
  "{"                  { tok (\p s -> TkLLA p s) }
  "}"                  { tok (\p s -> TkLLC p s) }
  ","                  { tok (\p s -> TkCOM p s) }
  ":"                  { tok (\p s -> TkDSP p s) }
  "::"                 { tok (\p s -> TkCONCAT p s) } -- Concatenación de dos listas
  "|"                  { tok (\p s -> TkCON p s) }
  "="                  { tok (\p s -> TkASG p s) }
  "^"                  { tok (\p s -> TkUPP p s) }
  "."                  { tok (\p s -> TkLOW p s) }
  
  -- Comentarios

  @comments           ; -- Se ignoran los comentarios
  @comment             ;-- Se ignora el comentario

  -- Caracteres invalidos

  @error               { tok (\p s -> TkERR p s) }

{
tok :: (AlexPosn -> String -> Token) -> AlexPosn -> String -> Token
tok f p s = f p s

pos :: AlexPosn -> String
pos (AlexPn _ f c) = " en la fila: " ++ (show f) ++ ", columna: " ++ (show c)

data Token = TkWRL AlexPosn String
           | TkRNE AlexPosn String
           | TkLOS AlexPosn String
           | TkOFK AlexPosn String
           | TkBTN AlexPosn String
           | TkWIN AlexPosn String
           | TkBTL AlexPosn String
           | TkPWR AlexPosn String
           | TkCONCAT AlexPosn String
           | TkSKL AlexPosn String
           | TkRNS AlexPosn String
           | TkKIT AlexPosn String
           | TkINV AlexPosn String
           | TkITM AlexPosn String
           | TkSMN AlexPosn String
           | TkFRE AlexPosn String
           | TkDTZ AlexPosn String
           | TkJST AlexPosn String
           | TkDRP AlexPosn String
           | TkNPR AlexPosn String
           | TkCTR AlexPosn String
           | TkIN  AlexPosn String
           | TkTO  AlexPosn String
           | TkPLY AlexPosn String
           | TkLCK AlexPosn String
           | TkNLK AlexPosn String
           | TkSPW AlexPosn String
           | TkGMO AlexPosn String
           | TkKPP AlexPosn String
           | TkKLL AlexPosn String
           | TkMST AlexPosn String
           | TkBSS AlexPosn String
           | TkNMB AlexPosn String
           | TkIDF AlexPosn String
           | TkCHA AlexPosn String
           | TkSTG AlexPosn String
           | TkINT AlexPosn String
           | TkFLT AlexPosn String
           | TkIDV AlexPosn String
           | TkLOR AlexPosn String
           | TkAND AlexPosn String
           | TkLET AlexPosn String
           | TkEQL AlexPosn String
           | TkNEQ AlexPosn String
           | TkGET AlexPosn String
           | TkLSA AlexPosn String
           | TkLSC AlexPosn String
           | TkINC AlexPosn String
           | TkDEC AlexPosn String
           | TkSUM AlexPosn String
           | TokenEndInstruction AlexPosn String
           | TkMIN AlexPosn String
           | TkTMS AlexPosn String
           | TkDVD AlexPosn String
           | TkMOD AlexPosn String
           | TkLEN AlexPosn String
           | TkREF AlexPosn String
           | TkEXC AlexPosn String
           | TkLTH AlexPosn String
           | TkGTH AlexPosn String
           | TkAPT AlexPosn String
           | TkPRA AlexPosn String
           | TkPRC AlexPosn String
           | TkLLA AlexPosn String
           | TkLLC AlexPosn String
           | TkCOM AlexPosn String
           | TkDSP AlexPosn String
           | TkCON AlexPosn String
           | TkASG AlexPosn String
           | TkUPP AlexPosn String
           | TkLOW AlexPosn String
           | TkFIN AlexPosn String
           | TkARA AlexPosn String
           | TkARC AlexPosn String
           | TkERR AlexPosn String
           deriving (Eq)

instance Show Token where
    show (TkWRL p s) = "Token " ++ s ++ (pos p) -- world
    show (TkOFK p s) = "Token " ++ s ++ (pos p) -- of
    show (TokenEndInstruction p s) = "Token End Instruction \\n" ++  (pos p) -- New line
    show (TkBTN p s) = "Token " ++ s ++ (pos p) -- Button
    show (TkRNE p s) = "Token " ++ s ++ (pos p) -- rune
    show (TkLOS p s) = "Token " ++ s ++ (pos p) -- Lose
    show (TkWIN p s) = "Token " ++ s ++ (pos p) -- Win
    show (TkBTL p s) = "Token " ++ s ++ (pos p) -- Battle
    show (TkPWR p s) = "Token " ++ s ++ (pos p) -- Power
    show (TkSKL p s) = "Token " ++ s ++ (pos p) -- Skill
    show (TkRNS p s) = "Token " ++ s ++ (pos p) -- Runes
    show (TkCONCAT p s) = "Token " ++ s ++ (pos p) -- Concatenación de lista
    show (TkKIT p s) = "Token " ++ s ++ (pos p) -- Kit
    show (TkINV p s) = "Token " ++ s ++ (pos p) -- Inventory
    show (TkITM p s) = "Token " ++ s ++ (pos p) -- Items
    show (TkSMN p s) = "Token " ++ s ++ (pos p) -- summon
    show (TkFRE p s) = "Token " ++ s ++ (pos p) -- free
    show (TkDTZ p s) = "Token " ++ s ++ (pos p) -- DeathZone
    show (TkJST p s) = "Token " ++ s ++ (pos p) -- joystick
    show (TkDRP p s) = "Token " ++ s ++ (pos p) -- drop
    show (TkNPR p s) = "Token " ++ s ++ (pos p) -- notPressed
    show (TkCTR p s) = "Token " ++ s ++ (pos p) -- controller
    show (TkIN p s)  = "Token " ++ s ++ (pos p) -- <-
    show (TkTO p s)  = "Token " ++ s ++ (pos p) -- ->
    show (TkPLY p s) = "Token " ++ s ++ (pos p) -- play
    show (TkLCK p s) = "Token " ++ s ++ (pos p) -- lock
    show (TkNLK p s) = "Token " ++ s ++ (pos p) -- unlock
    show (TkSPW p s) = "Token " ++ s ++ (pos p) -- spawn
    show (TkGMO p s) = "Token " ++ s ++ (pos p) -- gameOver
    show (TkKPP p s) = "Token " ++ s ++ (pos p) -- keepPlaying
    show (TkKLL p s) = "Token " ++ s ++ (pos p) -- kill
    show (TkMST p s) = "Token " ++ s ++ (pos p) -- monster
    show (TkBSS p s) = "Token " ++ s ++ (pos p) -- boss
    show (TkNMB p s) = "Token nombre programa " ++ s ++ (pos p) -- Nombre programa
    show (TkIDF p s) = "Token identificador \"" ++ s ++ "\"" ++ (pos p) -- Id
    show (TkCHA p s) = "Token caracter " ++ s ++ (pos p) -- carácter
    show (TkSTG p s) = "Token string " ++ s ++ (pos p) -- String
    show (TkINT p s) = "Token entero " ++ s ++ (pos p) -- Entero
    show (TkFLT p s) = "Token flotante " ++ s ++ (pos p) -- Flotante
    show (TkIDV p s) = "Token " ++ s ++ (pos p) -- //
    show (TkLOR p s) = "Token " ++ s ++ (pos p) -- ||
    show (TkAND p s) = "Token " ++ s ++ (pos p) -- &&
    show (TkLET p s) = "Token " ++ s ++ (pos p) -- <=
    show (TkEQL p s) = "Token " ++ s ++ (pos p) -- ==
    show (TkNEQ p s) = "Token " ++ s ++ (pos p) -- !=
    show (TkGET p s) = "Token " ++ s ++ (pos p) -- >=
    show (TkLSA p s) = "Token " ++ s ++ (pos p) -- <<
    show (TkLSC p s) = "Token " ++ s ++ (pos p) -- >>
    show (TkINC p s) = "Token " ++ s ++ (pos p) -- ++
    show (TkDEC p s) = "Token " ++ s ++ (pos p) -- --
    show (TkSUM p s) = "Token " ++ s ++ (pos p) -- +
    show (TkMIN p s) = "Token " ++ s ++ (pos p) -- -
    show (TkTMS p s) = "Token " ++ s ++ (pos p) -- *
    show (TkDVD p s) = "Token " ++ s ++ (pos p) -- /
    show (TkMOD p s) = "Token " ++ s ++ (pos p) -- %
    show (TkLEN p s) = "Token " ++ s ++ (pos p) -- #
    show (TkREF p s) = "Token " ++ s ++ (pos p) -- ?
    show (TkEXC p s) = "Token " ++ s ++ (pos p) -- !
    show (TkLTH p s) = "Token " ++ s ++ (pos p) -- <
    show (TkGTH p s) = "Token " ++ s ++ (pos p) -- >
    show (TkAPT p s) = "Token " ++ s ++ (pos p) -- puff
    show (TkPRA p s) = "Token " ++ s ++ (pos p) -- (
    show (TkPRC p s) = "Token " ++ s ++ (pos p) -- )
    show (TkLLA p s) = "Token " ++ s ++ (pos p) -- {
    show (TkLLC p s) = "Token " ++ s ++ (pos p) -- }
    show (TkCOM p s) = "Token " ++ s ++ (pos p) -- ,
    show (TkDSP p s) = "Token " ++ s ++ (pos p) -- :
    show (TkCON p s) = "Token " ++ s ++ (pos p) -- |
    show (TkASG p s) = "Token " ++ s ++ (pos p) -- =
    show (TkUPP p s) = "Token " ++ s ++ (pos p) -- ^
    show (TkLOW p s) = "Token " ++ s ++ (pos p) -- .
    show (TkFIN p s) = "Token " ++ s ++ (pos p) -- .~
    show (TkARA p s) = "Token " ++ s ++ (pos p) -- "|}"
    show (TkARC p s) = "Token " ++ s ++ (pos p) -- "{|"
    show (TkERR p s) = "Error, caracter inesperado " ++ s ++ (pos p) -- Error
}
