# Nombre del Lenguaje.

Hay que decidir un nombre para el lenguaje. Recomiendo dejar para el final.

## Estructura de un programa.

Un programa en el lenguaje tiene la siguiente estructura:

```
<Lista de Instrucciones>
```

## Estructuras de control de flujo.

### Selección.

La selección en el lenguaje es `if/elif/else` de la forma siguiente.

```
if <Expresion Booleana> {
  <Lista de Instrucciones>
} elif <Expresion Booleana> {
  <Lista de Instrucciones>
} elif <Expresion Booleana> {
  <Lista de Instrucciones>
}... {
  <Lista de Instrucciones>
} else {
  <Lista de Instrucciones>
}
```

### Repetición.

#### Determinada.

La repetición determinada es un ciclo `for` como se sigue:

```
for <variable> in <arreglo> {
  <Lista de Instrucciones>
}
```

En cada iteración, `<variable>` tendrá a un elemento de `<arreglo>`.

#### Indeterminada.

La repetición indeterminada es un ciclo `while` como se sigue:

```
while <Expresion Booleana> {
  <Lista de Instrucciones>
}
```

### Subrutinas.

#### Funciones y procedimientos.

Las funciones se definen son estructuras de control de flujo que toman una cantidad arbitraria de argumentos definidos por el programador, pudiendo incluso no tomar argumento alguno.

Las funciones toman cualquier tipo de dato y retornan un tipo de dato escalar (ver **Tipos de datos**).

Los procedimientos son funciones que siempre retornan el valor unit, el cual no tiene más uso que este.

La sintaxis de una función es:

`<nombre>(<tipo> <parámetro formal>, <tipo> <parámetro formal>, ..., <tipo> <parámetro formal>) <tipo>`

La sintaxis de un procedimiento es:

`<nombre>(<tipo> <parámetro formal>, <tipo> <parámetro formal>, ..., <tipo> <parámetro formal>)` 

##### Pasaje de parámetros.

Se admite el paso de parámetros por valor y por referencia. Se diferencia el pase por referencia por el ampersand prefijo a uno de los parámetros. El pase por valor no requiere ninguna sintaxis adicional.

Ejemplo:

#### Recursión.

El lenguaje admite la invocación recursiva de funciones en cualquier momento.

Ejemplo:

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

La palabra reservada para las variables de tipo carácter es `char`.

#### Booleanos.

Son datos de 8 bit (1 B) en memoria y se caracterizan por ser `True` o `False` sin estar encerrados entre comillas. `False` corresponde en memoria a los ocho bit en 0 y `True` a cualquier otra combinación.

La palabra reservada para las variables de tipo booleano es `bool`.

#### Enteros.

Son datos de 32 bit (4 B) en memoria que pueden ser cualquier cadena no vacía de números de la base decimal. Su representación es **complemento a 2**, por lo tanto los enteros están acotados en el rango: `[-2 147 483 648..2 147 483 647]`.

La palabra reservada para las variables de tipo entero es `int`.

#### Números de punto flotante.

Son datos de 64 bit (8 B) en memoria representados según el estándar **IEEE 754 de precisión doble**. Se construyen como:

`Entero . Entero`

La palabra reservada para las variables de tipo punto flotante es `float`.

### Compuestos.

#### Arreglos.

Son estructuras de datos homogéneas de cualquier tipo, es decir, se admite arreglos multidimensionales de algún tipo escalar. Su representación en un programa es `[elemento0, elemento1, ..., elementoN]`, siendo todos los elementos del mismo tipo. Son estructuras estáticas y su tamaño debe ser definido en su declaración.

Para variables de tipo arreglo la construcción reservada que lo identifica es `<tipo>[<entero>]`, donde `<tipo>` puede ser otro arreglo.

Ejemplo:

#### Strings.

Son arreglos de carácteres. Corresponden a un grupo particular de arreglos por su representación, estos admiten la representación de arreglo y de cadena de carácteres como en los lenguajes tradicionales, encerrados entre comillas dobles (`"`), es decir,

```
['s', 't', 'r', 'i', 'n', 'g', 's'] == "strings"
```

Para variables de tipo string, puede utilizarse `char[<entero>]` o `string`.

#### Registros.

Son estructuras de la forma 

```
register <nombre> {
  <tipo> <nombre>
  <tipo> <nombre>
  ...
  <tipo> <nombre>
}
```

Su tamaño en memoria corresponde a la suma de los tamaños individuales de cada campo que posea. Los tipos que acepta un registro son todos aquellos tipos que están presentes en el lenguaje. `register` es la palabra reservada para identificar a una variable de tipo registro.

Ejemplo:

#### Registros variantes.

Son estructuras de la forma:

```
union <nombre> {
  <tipo> <nombre>
  <tipo> <nombre>
  ...
  <tipo> <nombre>
}
```

Su tamaño en memoria corresponde al tamaño del campo que tenga mayor tamaño. Los tipos que acepta un registro variante son todos aquellos tipos que están presentes en el lenguaje. `union` es la palabra reservada para identificar a una variable de tipo registro.

#### Apuntadores.

Son un tipo de dato especial que guarda la dirección de memoria dinámica donde comienza el objeto apuntado. En memoria son una palabra del procesador. Su sintaxis es: `<tipo> * <nombre>`

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

Un identificador válido para una variable en un programa del lenguaje cumple con las siguientes condiciones:

1. No empieza con números.
2. Posee solo caracteres alfanuméricos y los caracteres especiales `'_'` y `'''`.
3. No corresponde a ninguna palabra reservada del lenguaje.

##### Declaración e inicialización de variables.

Se permite la declaración de variables sin previa inicialización.

### Condicional.

Ver sección **Estructuras de Datos. Selección**.

### Repetición.

Ver sección **Estructuras de Datos. Repetición**.

### Entrada/Salida.

Las operaciones de entrada/salida serán realizadas con las instrucciones `read` y `write` como se sigue:

```
read <variable>

write <variable>
```

Esto toma el contenido de la entrada estándar y lo almacena en `<variable>` y posteriormente el contenido de `<variable>` es mostrado por la salida estándar, respectivamente.

### Expresiones Aritméticas.

Corresponden a las expresiones que devuelven valores numéricos después de evaluarse. Los valores numéricos pueden ser indiferentemente enteros o punto flotante. Las expresiones aritméticas son aquellas que involucren los operadores de:

* Adición (`+`): definida tradicionalmente.
* Sustracción (`-`): definida tradicionalmente.
* Multiplicación (`*`): definida tradicionalmente.
* División (`/`): toma dos números y devuelve un punto flotante.
* División entera (`//`): toma dos enteros y regresa un entero que corresponde al cociente de la división.
* Residuo (`%`): toma dos enteros y regresa un entero que corresponde al resto de la división.
* Menos unario (`-`): toma un entero y lo multiplica por `-1`.

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
  `True > Flase == True` para booleanos. Para caracteres se sigue el orden léxico normal.
* Mayor o igual que (`>=`): definida tradicionalmente para enteros y punto flotante.
  `True > Flase == True` para booleanos. Para caracteres se sigue el orden léxico normal.
* Menor que (`<`): defnida tradicionalmente para enteros y punto flotante.
  `False < True == True` para booleanos. Para caracteres se sigue el orden léxico normal.
* Menor o igual que (`<=`): definida tradicionalmente para enteros y punto flotante.
  `False < True == True` para booleanos. Para caracteres se sigue el orden léxico normal.
  
### Expresiones de Carácteres.

Corresponden a las expresiones que devuelven un caracter después de su evaluación. Son aquellas en las cuales están involucrados los operadores unarios prefijos de:

* Mayúscula (`^`): convierte el carácter alfabético dado a su representación en mayúscula.
* Minúscula (`~`): convierte el carácter alfabético dado a su representación en minúscula.

### Expresiones para Arreglos.

Corresponden a las expresiones que devuelven un arreglo después de su evaluación. Son aquellas en las cuales están invoucrados los operadores de:

* Concatenación (`::`): toma dos arreglos y concatena el segundo a la cola del primero.
* Anexo (`:`): toma un elemento cualquiera y lo agrega a la cabeza del arreglo.

## Bloques y Alcance de bloques.

Un bloque dentro del programa está delimitado por llaves `{` y `}` y se obtiene al estar dentro de instrucciones condicionales, de repetición y funciones. El alcance es estático (o léxico).

## Evaluación.

### Precedencia de los operadores.

En orden descendente, la precedencia de los operadores es:

|     Tipo   | Primer Nivel |      Segundo nivel     |       Tercer nivel       |     Cuarto nivel     |
| ---------- | -------------| ---------------------- | ------------------------ | -------------------- |
| Aritmético | Menos Unario | División Entera, Resto | Multiplicación, División | Adición, sustracción |
|  Booleano  |   Negación   | Conjunción, Disyunción |                          |     Comparadores     |
|  Carácter  |   Mayúscula  |        Minúscula       |                          |                      |
|  Arreglos  |     Anexo    |      Concatenación     |                          |                      |

### Asociatividad de los operadores.

|     Tipo   |                     Izquierda                    |               Derecha              |   No asocia  |
| ---------- | ------------------------------------------------ | ---------------------------------- | ------------ |
| Aritmético | División, División Entera, Resto, Multiplicación | Adición, sustracción, Menos Unario |              |
|  Booleano  |              Conjunción, Disyunción              |              Negación              | Comparadores |
|  Carácter  |                                                  |       Mayúsculas, Minúsculas       |              |
|  Arreglos  |               Anexo, Concatenación               |                                    |              |

### Orden de evaluación de las expresiones.

Las expresiones se evaluan de izquierda a derecha.

## Comentarios y espacios en blanco.

En el lenguaje se pueden escribir comentarios de una línea o de varias lineas. Al escribir `@` se ignorarán todos los caracteres hasta el siguiente salto de línea. El texto escrito entre `{-` y `-}` será ignorado. Los espacios en blanco también son ignorados.

## Ejemplo de un programa en el lenguaje.
