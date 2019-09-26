# Analizador léxico de CHask*

Este documento es el informe sobre el analizador léxico del lenguaje CHask*.
La

## Tokens.

Son las unidades léxicas que se admiten dentro del lenguaje. Se ordenan en
palabras clave, identificadores, enteros, flotantes, booleanos, caracteres,
arreglos, strings, listas, registros, uniones, apuntadores, operadores,

### Palabras clave.

Son las que el lenguaje utiliza para identificar alguna funcionalidad
intrínseca.

### Identificadores.

Son aquellos que cumplen con la expresión regular:

`[a-zA-z0-9_']+ == [a-zA-z0-9_'][a-zA-z0-9_']*`

### Enteros.

Son aquellos que cumplen con la expresión regular:

`-[0-9]+ | [0-9]+`


### Flotantes.

Son aquellos que cumplen con la expresión regular:

`-[0-9]+.[0-9]+ | [0-9]+.[0-9]+`

### Booleanos.

Son aquellos que cumplen con la expresión regular:

`[TF]`

### Carácteres.

Son aquellos que cumplen con la expresión regular:

`\'ASCII\'`

### Arreglos.

Son aquellos que cumplen con la expresión regular:

`\[\]`

### Strings.

Son aquellos que cumplen con la expresión regular:

`\"ASCII\"`

### Listas.

Son aquellos que cumplen con la expresión regular:

`\<\<\>\>`

### Registros.

Son aquellos que cumplen con la expresión regular:

``

### Uniones.

Son aquellos que cumplen con la expresión regular:

``

### Apuntadores.

Son aquellos que cumplen con la expresión regular:

``

### Operadores.

Son aquellos que cumplen con la expresión regular:

``

## El tipo Token.

Esta sección describe el tipo Token que será utilizado.

```Haskell
data Token = TkPalabraClave
           | TkInt Int
           | TkFlt Fraccional
           | TkChr Char
           | TkStr String
           | TkArr
           | TkReg
           | TkLst
           | TkSum
           | TkMin
           | TkTms
           | TkDvd
           | TkIDv
           | TkMod

```
