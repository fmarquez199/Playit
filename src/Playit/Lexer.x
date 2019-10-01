
{
{-
Analizador lexico

Copyright : (c) Manuel Gonzalez 11-10390
                Francisco Javier 12-11163
                Natascha Gamboa 12-11250
-}

module Playit.Lexer (Token(..), AlexPosn(..), alexScanTokens) where
}

%wrapper "posn"

-- Conjunto de caracteres

$digitos      = [0-9]
$abecedario   = [a-zA-Z]
$simbolos     = [\+ \- \* \/ \% \# \! \< \> \( \) \' \{ \} \, \: \| \=]
$especiales   = [0 t n \* \~ \\]
$validos      = [$digitos $abecedario $simbolos $especiales]
$char_id      = [$digitos $abecedario \_ \']
$char_texto   = [ $validos # [\* \~] ]

-- Expresiones regulares

-- @texto PUEDE SER CUALQUIER caracter MENOS el caracter ~ y *
--  O PUEDE SER el caracter \*

@scape        = '\\'  $especiales
@texto        = $char_texto* | @scape
@variables    = $abecedario $char_id*
@programas    = \% $char_id+ \%
@caracter     = \*.{1}\* | \* @scape \*
@strings      = \~ @texto \~
@comentarios  = "~*" ([^[\*]] | [\r\n] | (\** ([^[\*\~]] | [\r\n]) ))* \*+\~
@comentario   = "@" .*
@float        = $digitos+ \' $digitos+
@error        = .

-- Tokens admitidos por el lenguaje

tokens :-

  $white+                           ;
  
  -- Palabras reservadas

  Battle               { tok (\p s -> TkBTL p s) }
  DeathZone            { tok (\p s -> TkDTZ p s) }
  Inventory            { tok (\p s -> TkINV p s) }
  Items                { tok (\p s -> TkITM p s) }
  Kit                  { tok (\p s -> TkKIT p s) }
  Power                { tok (\p s -> TkPWR p s) }
  Rune                 { tok (\p s -> TkRNE p s) }
  Runes                { tok (\p s -> TkRNS p s) }
  Skill                { tok (\p s -> TkSKL p s) }
  Button               { tok (\p s -> TkBTN p s) }
  boss                 { tok (\p s -> TkBSS p s) }
  controller           { tok (\p s -> TkCTR p s) }
  drop                 { tok (\p s -> TkDRP p s) }
  notPressed           { tok (\p s -> TkNPR p s) }
  free                 { tok (\p s -> TkFRE p s) }
  gameOver             { tok (\p s -> TkGMO p s) }
  joystick             { tok (\p s -> TkJST p s) }
  keepPlaying          { tok (\p s -> TkKPP p s) }
  kill                 { tok (\p s -> TkKLL p s) }
  lock                 { tok (\p s -> TkLCK p s) }
  monster              { tok (\p s -> TkMST p s) }
  play                 { tok (\p s -> TkPLY p s) }
  puff                 { tok (\p s -> TkAPT p s) }
  spawn                { tok (\p s -> TkSPW p s) }
  summon               { tok (\p s -> TkSMN p s) }
  unlock               { tok (\p s -> TkNLK p s) }
  world                { tok (\p s -> TkWRL p s) }
  of                   { tok (\p s -> TkOFK p s) }

  -- Literales booleanos
  
  Win                  { tok (\p s -> TkWIN p s) }
  Lose                 { tok (\p s -> TkLOS p s) }

  -- Identificadores

  @programas          { tok (\p s -> TkNMB p s) }
  @variables          { tok (\p s -> TkIDF p s) }

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
  "["                  { tok (\p s -> TkCRA p s) }
  "]"                  { tok (\p s -> TkCRC p s) }
  "{"                  { tok (\p s -> TkLLA p s) }
  "}"                  { tok (\p s -> TkLLC p s) }
  ","                  { tok (\p s -> TkCOM p s) }
  "'"                  { tok (\p s -> TkCMS p s) }
  ":"                  { tok (\p s -> TkDSP p s) }
  "|"                  { tok (\p s -> TkCON p s) }
  "="                  { tok (\p s -> TkASG p s) }
  
  -- Comentarios

  @comentarios         { tok (\p s -> TkCMV p (init s)) }
  @comentario          { tok (\p s -> TkCM1 p (init s)) }
  
  -- Caracteres erroneos

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
           | TkCRA AlexPosn String
           | TkCRC AlexPosn String
           | TkLLA AlexPosn String
           | TkLLC AlexPosn String
           | TkCOM AlexPosn String
           | TkCMS AlexPosn String
           | TkDSP AlexPosn String
           | TkCON AlexPosn String
           | TkASG AlexPosn String
           | TkCMV AlexPosn String
           | TkCM1 AlexPosn String
           | TkFIN AlexPosn String
           | TkARA AlexPosn String
           | TkARC AlexPosn String
           | TkERR AlexPosn String
           deriving (Eq)

instance Show Token where
    show (TkWRL p s) = "Token " ++ s ++ (pos p) ++ "\n" -- world
    show (TkOFK p s) = "Token " ++ s ++ (pos p) ++ "\n" -- of
    show (TkBTN p s) = "Token " ++ s ++ (pos p) ++ "\n" -- Button
    show (TkRNE p s) = "Token " ++ s ++ (pos p) ++ "\n" -- rune
    show (TkLOS p s) = "Token " ++ s ++ (pos p) ++ "\n" -- Lose
    show (TkWIN p s) = "Token " ++ s ++ (pos p) ++ "\n" -- Win
    show (TkBTL p s) = "Token " ++ s ++ (pos p) ++ "\n" -- Battle
    show (TkPWR p s) = "Token " ++ s ++ (pos p) ++ "\n" -- Power
    show (TkSKL p s) = "Token " ++ s ++ (pos p) ++ "\n" -- Skill
    show (TkRNS p s) = "Token " ++ s ++ (pos p) ++ "\n" -- Runes
    show (TkKIT p s) = "Token " ++ s ++ (pos p) ++ "\n" -- Kit
    show (TkINV p s) = "Token " ++ s ++ (pos p) ++ "\n" -- Inventory
    show (TkITM p s) = "Token " ++ s ++ (pos p) ++ "\n" -- Items
    show (TkSMN p s) = "Token " ++ s ++ (pos p) ++ "\n" -- summon
    show (TkFRE p s) = "Token " ++ s ++ (pos p) ++ "\n" -- free
    show (TkDTZ p s) = "Token " ++ s ++ (pos p) ++ "\n" -- DeathZone
    show (TkJST p s) = "Token " ++ s ++ (pos p) ++ "\n" -- joystick
    show (TkDRP p s) = "Token " ++ s ++ (pos p) ++ "\n" -- drop
    show (TkNPR p s) = "Token " ++ s ++ (pos p) ++ "\n" -- notPressed
    show (TkCTR p s) = "Token " ++ s ++ (pos p) ++ "\n" -- controller
    show (TkIN p s)  = "Token " ++ s ++ (pos p) ++ "\n" -- <-
    show (TkTO p s)  = "Token " ++ s ++ (pos p) ++ "\n" -- ->
    show (TkPLY p s) = "Token " ++ s ++ (pos p) ++ "\n" -- play
    show (TkLCK p s) = "Token " ++ s ++ (pos p) ++ "\n" -- lock
    show (TkNLK p s) = "Token " ++ s ++ (pos p) ++ "\n" -- unlock
    show (TkSPW p s) = "Token " ++ s ++ (pos p) ++ "\n" -- spawn
    show (TkGMO p s) = "Token " ++ s ++ (pos p) ++ "\n" -- gameOver
    show (TkKPP p s) = "Token " ++ s ++ (pos p) ++ "\n" -- keepPlaying
    show (TkKLL p s) = "Token " ++ s ++ (pos p) ++ "\n" -- kill
    show (TkMST p s) = "Token " ++ s ++ (pos p) ++ "\n" -- monster
    show (TkBSS p s) = "Token " ++ s ++ (pos p) ++ "\n" -- boss
    show (TkNMB p s) = "Token nombre programa " ++ s ++ (pos p) ++ "\n" -- Nombre programa
    show (TkIDF p s) = "Token identificador \"" ++ s ++ "\"" ++ (pos p) ++ "\n" -- Id
    show (TkCHA p s) = "Token caracter " ++ s ++ (pos p) ++ "\n" -- carácter
    show (TkSTG p s) = "Token string " ++ s ++ (pos p) ++ "\n" -- String
    show (TkINT p s) = "Token entero " ++ s ++ (pos p) ++ "\n" -- Entero
    show (TkFLT p s) = "Token flotante " ++ s ++ (pos p) ++ "\n" -- Flotante
    show (TkIDV p s) = "Token " ++ s ++ (pos p) ++ "\n" -- //
    show (TkLOR p s) = "Token " ++ s ++ (pos p) ++ "\n" -- ||
    show (TkAND p s) = "Token " ++ s ++ (pos p) ++ "\n" -- &&
    show (TkLET p s) = "Token " ++ s ++ (pos p) ++ "\n" -- <=
    show (TkEQL p s) = "Token " ++ s ++ (pos p) ++ "\n" -- ==
    show (TkNEQ p s) = "Token " ++ s ++ (pos p) ++ "\n" -- !=
    show (TkGET p s) = "Token " ++ s ++ (pos p) ++ "\n" -- >=
    show (TkLSA p s) = "Token " ++ s ++ (pos p) ++ "\n" -- <<
    show (TkLSC p s) = "Token " ++ s ++ (pos p) ++ "\n" -- >>
    show (TkINC p s) = "Token " ++ s ++ (pos p) ++ "\n" -- ++
    show (TkDEC p s) = "Token " ++ s ++ (pos p) ++ "\n" -- --
    show (TkSUM p s) = "Token " ++ s ++ (pos p) ++ "\n" -- +
    show (TkMIN p s) = "Token " ++ s ++ (pos p) ++ "\n" -- -
    show (TkTMS p s) = "Token " ++ s ++ (pos p) ++ "\n" -- *
    show (TkDVD p s) = "Token " ++ s ++ (pos p) ++ "\n" -- /
    show (TkMOD p s) = "Token " ++ s ++ (pos p) ++ "\n" -- %
    show (TkLEN p s) = "Token " ++ s ++ (pos p) ++ "\n" -- #
    show (TkREF p s) = "Token " ++ s ++ (pos p) ++ "\n" -- ?
    show (TkEXC p s) = "Token " ++ s ++ (pos p) ++ "\n" -- !
    show (TkLTH p s) = "Token " ++ s ++ (pos p) ++ "\n" -- <
    show (TkGTH p s) = "Token " ++ s ++ (pos p) ++ "\n" -- >
    show (TkAPT p s) = "Token " ++ s ++ (pos p) ++ "\n" -- puff
    show (TkPRA p s) = "Token " ++ s ++ (pos p) ++ "\n" -- (
    show (TkPRC p s) = "Token " ++ s ++ (pos p) ++ "\n" -- )
    show (TkCRA p s) = "Token " ++ s ++ (pos p) ++ "\n" -- [
    show (TkCRC p s) = "Token " ++ s ++ (pos p) ++ "\n" -- ]
    show (TkLLA p s) = "Token " ++ s ++ (pos p) ++ "\n" -- {
    show (TkLLC p s) = "Token " ++ s ++ (pos p) ++ "\n" -- }
    show (TkCOM p s) = "Token " ++ s ++ (pos p) ++ "\n" -- ,
    show (TkCMS p s) = "Token " ++ s ++ (pos p) ++ "\n" -- " ' "
    show (TkDSP p s) = "Token " ++ s ++ (pos p) ++ "\n" -- :
    show (TkCON p s) = "Token " ++ s ++ (pos p) ++ "\n" -- |
    show (TkASG p s) = "Token " ++ s ++ (pos p) ++ "\n" -- =
    show (TkCMV p s) = "Comentario varias lineas" ++ (pos p) ++ "\n" -- ~* whatever *~
    show (TkCM1 p s) = "Comentario una linea" ++ (pos p) ++ "\n" -- @ whatever
    show (TkFIN p s) = "Token " ++ s ++ (pos p) ++ "\n" -- .~
    show (TkARA p s) = "Token " ++ s ++ (pos p) ++ "\n" -- "|}"
    show (TkARC p s) = "Token " ++ s ++ (pos p) ++ "\n" -- "{|"
    show (TkERR p s) = "Error, caracter inesperado " ++ s ++ (pos p) ++ "\n" -- Error
}
