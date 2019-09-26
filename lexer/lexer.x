{
module Lexer (Token(..), AlexPosn(..), alexScanTokens) where
}

%wrapper "posn"

$digitos = [0-9]
$abecedario = [a-zA-Z]
$simbolos = [\+ \- \* \/ \% \# \! \< \> \$ \( \) \[ \] \{ \} \, \. \: \| \=]
$especiales = [\0 \t \n \\ \' \"]
$validos = [$digitos $abecedario $simbolos $especiales $white]

tokens :-

  $white+           ;
  Cr                { tok (\p s -> TkCHR p s) }
  F                 { tok (\p s -> TkFLS p s) }
  T                 { tok (\p s -> TkTRU p s) }
  Bl                { tok (\p s -> TkBLN p s) }
  Fl                { tok (\p s -> TkFLT p s) }
  Str               { tok (\p s -> TkSTR p s) }
  Lst               { tok (\p s -> TkLST p s) }
  Reg               { tok (\p s -> TkREG p s) }
  U                 { tok (\p s -> TkUNN p s) }
  new               { tok (\p s -> TkNEW p s) }
  free              { tok (\p s -> TkFRE p s) }
  Nlp               { tok (\p s -> TkNLP p s) }
  input             { tok (\p s -> TkINP p s) }
  out               { tok (\p s -> TkOUT p s) }
  break             { tok (\p s -> TkBRK p s) }
  continue          { tok (\p s -> TkCNT p s) }
  else              { tok (\p s -> TkELS p s) }
  for               { tok (\p s -> TkFOR p s) }
  in                { tok (\p s -> TkIN  p s) }
  while             { tok (\p s -> TkWHL p s) }
  return            { tok (\p s -> TkRTN p s) }
  [a-zA-Z0-9_']+    { tok (\p s -> TkIDF p s) }
  \' [$validos] \'  { tok (\p s -> TkCHA p s) }
  \" [$validos]* \" { tok (\p s -> TkSTG p s) }
  [\-]?[0-9]+       { tok (\p s -> TkINT p s) }
  \/\/              { tok (\p s -> TkIDV p s) }
  \|\|              { tok (\p s -> TkLOR p s) }
  \&\&              { tok (\p s -> TkAND p s) }
  \<\=              { tok (\p s -> TkLET p s) }
  \=\=              { tok (\p s -> TkEQL p s) }
  \!\=              { tok (\p s -> TkNEQ p s) }
  \>\=              { tok (\p s -> TkGET p s) }
  \<\<              { tok (\p s -> TkLSA p s) }
  \>\>              { tok (\p s -> TkLSC p s) }
  \+\+              { tok (\p s -> TkINC p s) }
  \-\-              { tok (\p s -> TkDEC p s) }
  \+                { tok (\p s -> TkSUM p s) }
  \-                { tok (\p s -> TkMIN p s) }
  \*                { tok (\p s -> TkTMS p s) }
  \/                { tok (\p s -> TkDVD p s) }
  \%                { tok (\p s -> TkMOD p s) }
  \#                { tok (\p s -> TkLEN p s) }
  \!                { tok (\p s -> TkNOT p s) }
  \<                { tok (\p s -> TkLTH p s) }
  \>                { tok (\p s -> TkGTH p s) }
  \$                { tok (\p s -> TkAPT p s) }
  \(                { tok (\p s -> TkPRA p s) }
  \)                { tok (\p s -> TkPRC p s) }
  \[                { tok (\p s -> TkCRA p s) }
  \]                { tok (\p s -> TkCRC p s) }
  \{                { tok (\p s -> TkLLA p s) }
  \}                { tok (\p s -> TkLLC p s) }
  \,                { tok (\p s -> TkCOM p s) }
  \.                { tok (\p s -> TkPNT p s) }
  \:                { tok (\p s -> TkDSP p s) }
  \|                { tok (\p s -> TkCON p s) }
  \=                { tok (\p s -> TkASG p s) }
  ~$validos         { tok (\p s -> TkERR p s) }

{
tok :: (AlexPosn -> String -> Token) -> AlexPosn -> String -> Token
tok f p s = f p s

pos :: AlexPosn -> String
pos (AlexPn _ f c) = " en la fila: " ++ (show f) ++ ", columna: " ++ (show c)

data Token = TkCHR AlexPosn String
           | TkFLS AlexPosn String
           | TkTRU AlexPosn String
           | TkBLN AlexPosn String
           | TkFLT AlexPosn String
           | TkSTR AlexPosn String
           | TkLST AlexPosn String
           | TkREG AlexPosn String
           | TkUNN AlexPosn String
           | TkNEW AlexPosn String
           | TkFRE AlexPosn String
           | TkNLP AlexPosn String
           | TkINP AlexPosn String
           | TkOUT AlexPosn String
           | TkBRK AlexPosn String
           | TkCNT AlexPosn String
           | TkELS AlexPosn String
           | TkFOR AlexPosn String
           | TkIN  AlexPosn String
           | TkWHL AlexPosn String
           | TkRTN AlexPosn String
           | TkIDF AlexPosn String
           | TkCHA AlexPosn String
           | TkSTG AlexPosn String
           | TkINT AlexPosn String
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
           | TkNOT AlexPosn String
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
           | TkPNT AlexPosn String
           | TkDSP AlexPosn String
           | TkCON AlexPosn String
           | TkASG AlexPosn String
           | TkERR AlexPosn String
           deriving (Eq)

instance Show Token where
    show (TkCHR p s) = "Token " ++ s ++ (pos p)
    show (TkFLS p s) = "Token " ++ s ++ (pos p)
    show (TkTRU p s) = "Token " ++ s ++ (pos p)
    show (TkBLN p s) = "Token " ++ s ++ (pos p)
    show (TkFLT p s) = "Token " ++ s ++ (pos p)
    show (TkSTR p s) = "Token " ++ s ++ (pos p)
    show (TkLST p s) = "Token " ++ s ++ (pos p)
    show (TkREG p s) = "Token " ++ s ++ (pos p)
    show (TkUNN p s) = "Token " ++ s ++ (pos p)
    show (TkNEW p s) = "Token " ++ s ++ (pos p)
    show (TkFRE p s) = "Token " ++ s ++ (pos p)
    show (TkNLP p s) = "Token " ++ s ++ (pos p)
    show (TkINP p s) = "Token " ++ s ++ (pos p)
    show (TkOUT p s) = "Token " ++ s ++ (pos p)
    show (TkBRK p s) = "Token " ++ s ++ (pos p)
    show (TkCNT p s) = "Token " ++ s ++ (pos p)
    show (TkELS p s) = "Token " ++ s ++ (pos p)
    show (TkFOR p s) = "Token " ++ s ++ (pos p)
    show (TkIN p s)  = "Token " ++ s ++ (pos p)
    show (TkWHL p s) = "Token " ++ s ++ (pos p)
    show (TkRTN p s) = "Token " ++ s ++ (pos p)
    show (TkIDF p s) = "Token " ++ s ++ (pos p)
    show (TkCHA p s) = "Token " ++ s ++ (pos p)
    show (TkSTG p s) = "Token " ++ s ++ (pos p)
    show (TkINT p s) = "Token " ++ s ++ (pos p)
    show (TkIDV p s) = "Token " ++ s ++ (pos p)
    show (TkLOR p s) = "Token " ++ s ++ (pos p)
    show (TkAND p s) = "Token " ++ s ++ (pos p)
    show (TkLET p s) = "Token " ++ s ++ (pos p)
    show (TkEQL p s) = "Token " ++ s ++ (pos p)
    show (TkNEQ p s) = "Token " ++ s ++ (pos p)
    show (TkGET p s) = "Token " ++ s ++ (pos p)
    show (TkLSA p s) = "Token " ++ s ++ (pos p)
    show (TkLSC p s) = "Token " ++ s ++ (pos p)
    show (TkINC p s) = "Token " ++ s ++ (pos p)
    show (TkDEC p s) = "Token " ++ s ++ (pos p)
    show (TkSUM p s) = "Token " ++ s ++ (pos p)
    show (TkMIN p s) = "Token " ++ s ++ (pos p)
    show (TkTMS p s) = "Token " ++ s ++ (pos p)
    show (TkDVD p s) = "Token " ++ s ++ (pos p)
    show (TkMOD p s) = "Token " ++ s ++ (pos p)
    show (TkLEN p s) = "Token " ++ s ++ (pos p)
    show (TkNOT p s) = "Token " ++ s ++ (pos p)
    show (TkLTH p s) = "Token " ++ s ++ (pos p)
    show (TkGTH p s) = "Token " ++ s ++ (pos p)
    show (TkAPT p s) = "Token " ++ s ++ (pos p)
    show (TkPRA p s) = "Token " ++ s ++ (pos p)
    show (TkPRC p s) = "Token " ++ s ++ (pos p)
    show (TkCRA p s) = "Token " ++ s ++ (pos p)
    show (TkCRC p s) = "Token " ++ s ++ (pos p)
    show (TkLLA p s) = "Token " ++ s ++ (pos p)
    show (TkLLC p s) = "Token " ++ s ++ (pos p)
    show (TkCOM p s) = "Token " ++ s ++ (pos p)
    show (TkPNT p s) = "Token " ++ s ++ (pos p)
    show (TkDSP p s) = "Token " ++ s ++ (pos p)
    show (TkCON p s) = "Token " ++ s ++ (pos p)
    show (TkASG p s) = "Token " ++ s ++ (pos p)
    show (TkERR p s) = "Error, carácter inesperado " ++ s ++ (pos p)
}
