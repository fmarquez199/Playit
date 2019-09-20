# CH*.

CH* (C & C ++ & Haskell) es un lenguaje imperativo de propósito general diseñado e implementado por estudiantes de Ingeniería de Computación de la Universidad Simón Bolívar en el transcurso de la cadena de Lenguajes de Programación.

## Estructura de un programa.

Un programa en CH* tiene la siguiente estructura:

```
<Lista de Instrucciones>
```

## Estructuras de control de flujo.

### Selección.

La selección en CH* es de la forma siguiente.

```
| <Expresion Booleana>:
  <Lista de Instrucciones>
| <Expresion Booleana>:
  <Lista de Instrucciones>
...
| else:
  <Lista de Instrucciones>

O también:
<Expresion Booleana> ? <Lista de Instrucciones> : <Lista de Instrucciones>
```

### Repetición.

#### Determinada.

La repetición determinada es un ciclo `for` como se sigue:

```
<
for <variable> in <arreglo>:
  <Lista de Instrucciones>
>
```

En cada iteración, `<variable>` tendrá a un elemento de `<arreglo>` y las iteraciones culminarán cuando ya no haya más elementos en `<arreglo>` sobre los cuales iterar.

#### Indeterminada.

La repetición indeterminada es un ciclo `while` como se sigue:

```
<
while <Expresion Booleana>:
  <Lista de Instrucciones>
>
```

### Subrutinas.

#### Funciones y procedimientos.

Las funciones se definen son estructuras de control de flujo que toman una cantidad arbitraria de argumentos definidos por el programador, pudiendo incluso no tomar argumento alguno.

Las funciones toman cualquier tipo de dato y retornan un tipo de dato escalar (ver **Tipos de datos**).

Los procedimientos son funciones que siempre retornan el valor unit, el cual no tiene más uso que este.

La sintaxis de una función es:

```
<
nombre(<tipo> <parámetro formal>, <tipo> <parámetro formal>, ..., <tipo> <parámetro formal>) <tipo>:
  <Lista de instrucciones>
>
```

La sintaxis de un procedimiento es:

```
<
nombre(<tipo> <parámetro formal>, <tipo> <parámetro formal>, ..., <tipo> <parámetro formal>):
  <Lista de instrucciones>
>
``` 

##### Pasaje de parámetros.

Se admite el paso de parámetros por valor y por referencia. Se diferencia el pase por referencia por el signo de interrogación (`?`) prefijo a uno de los parámetros. El pase por valor no requiere ninguna sintaxis adicional.

Ejemplo:

```
<
funcion(<tipo> <parámetro por valor>, <tipo> ?<parámetro por referencia>)<tipo de retorno>:
  <Lista de instrucciones>
>
```

#### Recursión.

CH* admite la invocación recursiva de funciones en cualquier momento.

Ejemplo:

```
<
procedimiento(<tipo> <parámetro>):
  <Lista de instrucciones>
  <Llamada recursiva>
  <Llamada a>funcion>
}

<
funcion(<tipo> <parámetro>)<tipo>:
  <Lista de instrucciones>
  <Llamada a prodecimiento>
>

<Llamada a funcion>
```

Un código de esta forma es posible en CH*.

## Tipos de datos

### Escalares.

#### Carácteres.

Son datos de 8 bit (1 B) en memoria y se caracterizan por ser un carácter ASCII Extendido entre comillas simples.

Se distinguen las secuencias de escape por ser aquellos precedidos por un backslash (`\`):
* `\n` (salto de espacio).
* `\t` (tabulación de 4 espacios).
* `\\` (backslash).
* `\"` (comillas dobles).
* `\'` (comilla simple).

La palabra reservada para las variables de tipo carácter es `Cr`.

#### Booleanos.

Son datos de 8 bit (1 B) en memoria y se caracterizan por ser `T` o `F` sin estar encerrados entre comillas. `F` corresponde en memoria a los ocho bit en 0 y `T` a cualquier otra combinación.

La palabra reservada para las variables de tipo booleano es `Bl`.

#### Enteros.

Son datos de 32 bit (4 B) en memoria que pueden ser cualquier cadena no vacía de números de la base decimal. Su representación es **complemento a 2**, por lo tanto los enteros están acotados en el rango: `[-2 147 483 648..2 147 483 647]`.

La palabra reservada para las variables de tipo entero es `In`.

#### Números de punto flotante.

Son datos de 64 bit (8 B) en memoria representados según el estándar **IEEE 754 de precisión doble**. Se construyen como:

`<Entero>.<Entero>`

La palabra reservada para las variables de tipo punto flotante es `Fl`.

### Compuestos.

#### Arreglos.

Son estructuras de datos homogéneas de cualquier tipo, es decir, se admite arreglos multidimensionales de algún tipo escalar. Su representación en un programa es `[elemento0, elemento1, ..., elementoN]`, siendo todos los elementos del mismo tipo. Son estructuras estáticas y su tamaño debe ser definido en su declaración.

Para variables de tipo arreglo la construcción reservada que lo identifica es `<tipo>[<entero>]`, donde `<tipo>` puede ser otro arreglo.

Ejemplo: ``` Cr[2] arreglo ```

#### Strings.

Son arreglos de carácteres. Corresponden a un grupo particular de arreglos por su representación, estos admiten la representación de arreglo y de cadena de carácteres como en los lenguajes tradicionales, encerrados entre comillas dobles (`"`), es decir,

```
['s', 't', 'r', 'i', 'n', 'g', 's'] == "strings"
```

Para variables de tipo Str, puede utilizarse `Cr[<entero>]` o `Str`.

#### Registros.

Son estructuras de la forma 

```
Reg <nombre> {
  <tipo> <nombre>
  <tipo> <nombre>
  ...
  <tipo> <nombre>
}
```

Su tamaño en memoria corresponde a la suma de los tamaños individuales de cada campo que posea. Los tipos que acepta un registro son todos aquellos tipos que están definidos en CH*. `Reg` es la palabra reservada para identificar a una variable de tipo registro.

Ejemplo:

#### Registros variantes.

Son estructuras de la forma:

```
U <nombre> {
  <tipo> <nombre>
  <tipo> <nombre>
  ...
  <tipo> <nombre>
}
```

Su tamaño en memoria corresponde al tamaño del campo que tenga mayor tamaño. Los tipos que acepta un registro variante son todos aquellos tipos que están definidos en CH*. `U` es la palabra reservada para identificar a una variable de tipo registro.

#### Apuntadores.

Son un tipo de dato especial que guarda la dirección de memoria dinámica donde comienza el objeto apuntado. En memoria son una palabra del procesador. Su sintaxis es: `<tipo> $<nombre>`. Para obtener la dirección del objeto apuntado se usa `?`.

## Instrucciones y Expresiones.

### Asignación.

La asignación se realiza mediante el operador `=`. Puede realizarse al momento de declarar una variable o posterior a su declaración, como se sigue:

```
<tipo> <variable>
<tipo> <variable1> = <expresión de tipo>

<variable> = <variable1>
```

Esto declara dos variables `<variable>` y `<variable1>`, a la segunda se le asigna el resultado de `<expresión de tipo>` y luego a la primera, que no fue inicializada por el programador, se le asigna el contenido de la segunda.

#### Asignaciones especiales: Incremento (`++`) y Decremento (`--`).

Son asignaciones que funcionan exclusivamente sobre variables de tipo entero y funcionan como operadores sufijos/postfijos:

* Incremento (`++`): Incrementa el valor de la variable en 1.
* Decremento (`--`): Disminuye el valor de la variable en 1.

Estas asignaciones son equivalentes cada una a realizar `<variable> = <variable> ± 1` según corresponda.

#### Identificadores de variables.

Un identificador válido para una variable en un programa de CH* cumple con las siguientes condiciones:

1. No empieza con números.
2. Posee solo caracteres alfanuméricos y los caracteres especiales `'_'` y `'''`.
3. No corresponde a ninguna palabra reservada de CH*.

##### Declaración e inicialización de variables.

Se permite la declaración de variables sin previa inicialización.

### Condicional.

Ver sección **Estructuras de Datos. Selección**.

### Repetición.

Ver sección **Estructuras de Datos. Repetición**.

### Entrada/Salida.

Las operaciones de entrada/salida serán realizadas con las instrucciones `in` y `out` como se sigue:

```
in <variable>

out <variable>
```

Esto toma el contenido de la entrada estándar y lo almacena en `<variable>` y posteriormente el contenido de `<variable>` es mostrado por la salida estándar, respectivamente.

La instrucción `in` toma una variable en la cual almacenará lo que lea de la entrada estándar. Su ejecución consiste en una interrupción para leer de la entrada estándar y almacenar en el destino indicado por el programador, si no se indica el destino la ejecución continua su curso y no se almacena la información. Se levanta una advertencia al programador al compilar.

La instrucción `out` toma una variable o una constante de tipo `Cr` o `Str` y muestra en la salida estándar el contenido de la variable o el valor de la constante.

### Expresiones Aritméticas.

Corresponden a las expresiones que devuelven valores numéricos después de evaluarse. Los valores numéricos pueden ser indiferentemente enteros o punto flotante. Las expresiones aritméticas son aquellas que involucren los operadores de:

* Adición (`+`): definida tradicionalmente.
* Sustracción (`-`): definida tradicionalmente.
* Multiplicación (`*`): definida tradicionalmente.
* División (`/`): toma dos números y devuelve un punto flotante.
* División entera (`//`): toma dos enteros y regresa un entero que corresponde al cociente de la división.
* Residuo (`%`): toma dos enteros y regresa un entero que corresponde al resto de la división.
* Menos unario (`-`): toma un entero y lo multiplica por `-1`.
* Longitud (`#`): toma un arreglo y devuelve la cantidad de elementos que contiene.

### Expresiones Booleanas.

Corresponden a las expresiones que devuelven valores booleanos después de evaluarse.

Las expresiones booleanas que reciben booleanos son aquellas que involucran a los operadores de:

* Disyunción (`||`): definida tradicionalmente.
* Conjunción (`&&`): definida tradicionalmente.
* Negación (`!`): definida tradicionalmente.

Las expresiones booleanas que reciben cualquier tipo escalar son aquellas que involucran comparaciones:

* Igualdad (`==`): definida tradicionalmente.
* Desigualdad (`!=`): definida tradicionalmente.
* Mayor que (`>`): defnida tradicionalmente para enteros y punto flotante.
  `T > F == T` para booleanos. Para caracteres se sigue el orden léxico normal.
* Mayor o igual que (`>=`): definida tradicionalmente para enteros y punto flotante.
  `T > F == T` para booleanos. Para caracteres se sigue el orden léxico normal.
* Menor que (`<`): defnida tradicionalmente para enteros y punto flotante.
  `F < T == T` para booleanos. Para caracteres se sigue el orden léxico normal.
* Menor o igual que (`<=`): definida tradicionalmente para enteros y punto flotante.
  `F < T == T` para booleanos. Para caracteres se sigue el orden léxico normal.
  
### Expresiones de Carácteres.

Corresponden a las expresiones que devuelven un caracter después de su evaluación. Son aquellas en las cuales están involucrados los operadores unarios prefijos de:

* Mayúscula (`^`): convierte el carácter alfabético dado a su representación en mayúscula.
* Minúscula (`~`): convierte el carácter alfabético dado a su representación en minúscula.

### Expresiones para Arreglos.

Corresponden a las expresiones que devuelven un arreglo después de su evaluación. Son aquellas en las cuales están invoucrados los operadores de:

* Indexación (`[]`): toma un arreglo y un entero `i` en el rango `[0..#<arreglo> - 1]` y devuelve el elemento contenido en la posición `i`.
* Concatenación (`::`): toma dos arreglos y concatena el segundo a la cola del primero.
* Anexo (`:`): toma un elemento cualquiera y lo agrega a la cabeza del arreglo.

## Bloques y Alcance de bloques.

Un bloque dentro del programa está delimitado por llaves `<` y `>` y se obtiene al estar dentro de instrucciones condicionales, de repetición y funciones. El alcance es estático (o léxico).

## Evaluación.

### Precedencia de los operadores.

En orden descendente, la precedencia de los operadores es:

|     Tipo   |      Primer Nivel      |      Segundo nivel     |       Tercer nivel       |     Cuarto nivel     |
| ---------- | ---------------------- | ---------------------- | ------------------------ | -------------------- |
| Aritmético | Menos Unario, Longitud | División Entera, Resto | Multiplicación, División | Adición, sustracción |
|  Booleano  |        Negación        | Conjunción, Disyunción |                          |     Comparadores     |
|  Carácter  |        Mayúscula       |        Minúscula       |                          |                      |
|  Arreglos  |    Indexación, Anexo   |      Concatenación     |                          |                      |

### Asociatividad de los operadores.

|     Tipo   |                     Izquierda                    |                     Derecha                  |   No asocia  |
| ---------- | ------------------------------------------------ | -------------------------------------------- | ------------ |
| Aritmético | División, División Entera, Resto, Multiplicación | Adición, sustracción, Menos Unario, Longitud |              |
|  Booleano  |              Conjunción, Disyunción              |                   Negación                   | Comparadores |
|  Carácter  |                                                  |            Mayúsculas, Minúsculas            |              |
|  Arreglos  |        Indexación, Anexo, Concatenación          |                                              |              |

### Orden de evaluación de las expresiones.

Las expresiones se evaluan de izquierda a derecha.

## Comentarios y espacios en blanco.

En CH* se pueden escribir comentarios de una línea o de varias lineas. Al escribir `@` se ignorarán todos los caracteres hasta el siguiente salto de línea. El texto escrito entre `{-` y `-}` será ignorado. Los espacios en blanco también son ignorados.

## Ejemplo de un programa en CH*.
```
@ Esto es un comentario de una línea.

{-
Este es un comentario
de varias líneas.
-}

In cardinal = 3
Cr[3] conjunto
In partes = 1
In contador = 0

<
while contador < cardinal:
  out "Ingrese un carácter: "
  in conjunto[contador]
 partes = partes * 2
  contador++
>

<
for elemento in conjunto:
  out "Los elementos del conjunto son: "
  out elemento : ", "
>
```
