# Analizador léxico de CHask*

Este documento es el informe sobre el analizador léxico del lenguaje CHask*.
Las expresiones regulares siguen la sintaxis de Alex 3.2.4

## Tokens.

Son las unidades léxicas que se admiten dentro del lenguaje. Se ordenan en
palabras clave, identificadores, enteros, flotantes, booleanos, caracteres,
strings, separadores, y operadores.

### Palabras clave.

Son las que el lenguaje utiliza para identificar alguna funcionalidad
intrínseca. Corresponden al conjunto:

`[Cr F T Bl Fl Str Lst Reg U new free Nlp input out break continue else for in
while return]`

### Identificadores.

Son aquellos que cumplen con la expresión regular:

`[a-zA-z0-9_']+`

### Enteros.

Son aquellos que cumplen con la expresión regular:

`[-]?[0-9]+`


### Flotantes.

Son aquellos que cumplen con la expresión regular:

`[-]?[0-9]+.[0-9]+`

### Booleanos.

Son aquellos que cumplen con la expresión regular:

`[TF]`

### Carácteres.

Son aquellos que cumplen con la expresión regular:

```
$simbolos = [\+ \- \* \/ \% \# \! \< \> \$ \( \) \[ \] \{ \} \, \. \: \| \=]
$especiales = [\0 \t \n \\ \' \"]
$blancos = [\ \t \n \f \v \r]
\'[a-zA-Z0-9 $simbolos $especiales $blancos]\'
```

### Strings.

Son aquellos que cumplen con la expresión regular:

```
$simbolos = [\+ \- \* \/ \% \# \! \< \> \$ \( \) \[ \] \{ \} \, \. \: \| \=]
$especiales = [\0 \t \n \\ \' \"]
$blancos = [\ \t \n \f \v \r]
\"[a-zA-Z0-9 $simbolos $especiales $blancos]\"
```

### Separadores.

Son aquellos que restringen los alcances o a ciertos datos compuestos:

`[\< \> \( \) \[ \] \{ \} \, \. \: \| \<\< \>\>]`

### Operadores.

Son aquellos que cumplen con la función de indicar una operación entre datos:

`\"[\+ \- \* \/ \% \# \! \< \> \$ \= \/\/ \|\| \&\& \<\= \=\= \!\= \>\= \+\+
\-\-]\"`

## El tipo Token.

Esta sección describe el tipo Token que será utilizado.

```Haskell
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
```
