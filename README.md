[![N|Solid](http://www.usb.ve/conocer/corporativa/archivos/logos/logotipo/logotip.png)](http://www.usb.ve)

# EL Lenguaje de Programación CHask*.

## Diseñado por:

* Manuel Gonzalez 11-10390
* Francisco Javier 12-11163
* Natascha Gamboa 12-11250

CHask* (C & C ++ & Haskell) es un **lenguaje imperativo** de propósito general,
no orientado a objetos, compilado y fuertemente tipado, diseñado e
implementado por estudiantes de Ingeniería de Computación de la **Universidad
Simón Bolívar** en el transcurso de la cadena de **Lenguajes de Programación
(CI-4721, CI-4722)**.

CHask* cuenta con:

* Tipos de datos primitivos (Integer, Character, Flotantes, Booleanos).
* Apuntadores a memoria en el Heap.
* Arreglos, Registros, Registros variantes.
* Lectura desde la entrada estándar y Escritura a la salida estándar.
* ¡Amor! :kissing_closed_eyes:

# Estructura de un programa.

Un programa en CHask* tiene la siguiente estructura:

```
<Lista instrucciones>
```

No está permitido un programa cuya lista de instrucciones sea vacía.

Un programa simple en CHask* es mostrar por la salida estándar *Hola Mundo!*
(*Hello World*):

```sh
out "Hello World!"
```

## Tipos de datos

### Escalares.

#### Caracteres.

Son datos de **8 bit (1 B)** en memoria y se caracterizan por ser un carácter
*ASCII* entre comillas simples.

Se distinguen las secuencias de escape por ser aquellos precedidos por un
backslash (`\`):

* `\0` (carácter nulo).
* `\n` (salto de espacio).
* `\t` (tabulación de 4 espacios).
* `\\` (backslash).
* `\"` (comillas dobles).
* `\'` (comilla simple).

La palabra reservada para las variables de tipo carácter es `Cr`. Si no se
inicializan al declararse se le establece `'\0'`.

#### Booleanos.

Son datos de **8 bit (1 B)** en memoria y solo admiten `T` o `F` como valor. 
Nota:`F` corresponde en memoria a los ocho bit en 0 y `T` a cualquier otra
combinación.

La palabra reservada para las variables de tipo booleano es `Bl`. Si no se
inicializan al declararse se le establece el valor `F`.

#### Enteros.

Son datos de 32 bit (4 B) en memoria que pueden ser cualquier cadena no vacía
de números de la base decimal. Su representación es **complemento a 2**, por lo
tanto los enteros están acotados en el rango:

`[-2 147 483 648, 2 147 483 647]`.

La palabra reservada para las variables de tipo entero es `In`. Si no se
inicializan al declararse se le establece el valor `0`.

Se consideran enteros yambién a las cadenas de números de la base decimal
precedida por una cantidad no nula de `0`, es decir, son enteros, los valores:

```sh
01
003
0000000000000000000000000000000000000005
```

#### Números de punto flotante.

Son datos de 64 bit (8 B) en memoria representados según el estándar **IEEE
754 de precisión doble**. Se construyen como:

`<Entero>.<Entero>`

La palabra reservada para las variables de tipo punto flotante es `Fl`.
Si no se inicializan al declararse se le establece el valor `0.0`.

**Ejemplo escalares**

```sh
In a
Cr c
Bl esMayor = F
In b = 2
a = 1
Fl r = 0.5
a = 2
```

En este fragmento de código ocurre lo siguiente:

1. Las variable `a` y `c` de tipo entero y carácter, respectivamente, se declaran
y son inicializadas por default en `0` y `'\0'`, respectivamente.
2. Las variables `esMayor` y `b` de tipo booleano y entero, respectivamente, son declaradas
e inicializadas directamente en `F` y `2`.
3. A la variable `a` se le asigna el valor `1`.
4. A la variable `r` de tipo punto flotante se declara e inicializa con el valor `0.5`.
5. A la variable `a` se le asigna el valor `2`.

### Compuestos.

#### Arreglos.

Es una estructura de datos homogénea de cualquier tipo escalar que se
encuentran ubicados de forma consecutiva en memoria, se admiten arreglos
multidimensionales. 

La representación de un arreglo unidimensional en un programa es
`[elemento0, elemento1, ..., elementoN]`, siendo todos los elementos del mismo
tipo. Son estructuras estáticas y su tamaño debe ser definido en su
declaración. 

Para variables de tipo arreglo la construcción reservada que lo identifica es
`<tipo>[<entero>] <nombre arreglo>`, donde `<tipo>` puede ser otro arreglo.

Son construcciones válidas del tipo arreglo:

```sh
<tipo1>[<entero>]
<tipo2>[<entero1>][<entero2>]
...
<tipoN>[<entero1>][<entero2>]...[<enteroN>]
```

Estas declaraciones hacen referencia a un arreglo unidimensional de longitud
`entero1` de `tipo1`, uno bidimensional de longitud `entero2` de arreglos de
longitud `entero1` de `tipo2` y en líneas generales un arreglo de longitud `enteroN`
de arreglos, de arreglos, de arreglos de longitud `entero2` de arreglos de
longitud `entero1` de `tipoN`.

La inicialización por defecto de los arreglos en CHask* se hace inicializando
cada posición del arreglo en el valor por defecto del tipo dado.

**Ejemplo:**

```sh 
Cr[3] abc
Str[3] nombres = ["Natascha", "Francisco", "Manuel"]
Fl[3] indices = [3.67, 3.20, 3.0]
```

#### Strings.

Son arreglos de caracteres. Son un grupo particular que admiten la
representación de arreglo y de cadena de caracteres, encerrados entre comillas
dobles `""`, es decir,

```sh
['s', 't', 'r', 'i', 'n', 'g', 's'] == "strings"
```

Para variables de tipo `string`, puede utilizarse `Cr[<entero>]` o `Str`. De
ser declarados como `Str` serán inicializados con la cadena vacía `""` por
default.

#### Listas.

Es una estructura de datos heterogénea de cualquier tipo escalar que se
encuentran ubicados de forma no necesariamente consecutiva en memoria, se
admiten listas multidimensionales.

La palabra reservada para su tipo es `Lst`. La lista vacía es `<<>>`. No
se inicializan automáticamente.

**Ejemplo:**

```sh
Lst lista1 = <<'C', 3>>
Lst lista2
```

En este código se declaran dos variables del tipo `Lst`, `lista1` inicializada
en la lista que contiene al `Cr` `'H'` y al `In` `3`, y `lista2` que por
defecto no es inicializada.

#### Registros.

Son estructuras de la forma 

```sh
<
Reg <nombre> 
  <tipo> <nombre>
  <tipo> <nombre>
  ...
  <tipo> <nombre>
>
```

Cada campo del registro corresponde a un tipo escalar o compuesto definido
previamente en el lenguaje o el programa. Cada instancia de registro
corresponde a un tipo de dato también que puede ser parte de otro registro.

Su tamaño en memoria corresponde a la suma de los tamaños individuales de cada
campo que posea. Los tipos que acepta un registro son todos aquellos tipos que
están definidos en CHask*. `Reg` es la palabra reservada para identificar a
una variable de tipo registro.

Para inicializarse en la declaración se puede pasar una expresión con
exactamente el mismo número de argumentos, como se sigue:

```sh
<
Reg registro
  In tenso
  Cr omo
>
registro = {0, 'f'}
```

**Ejemplo:**

```sh
<
Reg Contacto 
  Str nombre
  In edad
  Bl tieneTrabajo
>

Contacto Alex = {"Alex", 15, F}
Contacto sofia
sofia.nombre = "Sofia"
sofia.edad = 29
sofia.tieneTrabajo = T

out "Hola ", sofia.nombre
```

Al ejecutar este código ocurre que la variable `Alex` de tipo `Contacto` es
declarada e inicializada asignando a cada campo los valores `"Alex"`, `15`,
`F` respectivamente, encerrados entre llaves `{}` para poder realizar la
asignación simultánea. Del mismo modo, la variable `sofia` es inicializada
asignando uno a uno, valores a sus campos. Al final de la ejecución se imprime
en pantalla:

```
Hola Sofia
```

#### Registros variantes.

Son estructuras de la forma:

```sh
<
U <nombre> 
  <tipo> <nombre>
  <tipo> <nombre>
  ...
  <tipo> <nombre>
>
```

Su tamaño en memoria corresponde al campo de mayor tamaño. Los tipos
que acepta son todos aquellos que están definidos en CHask*. `U` es la palabra reservada para identificarlos.

Para inicializar una variable de tipo `U` se debe hacer haciendo referencia al
campo que se desea inicializar, no se permite la inicialización de todos los
campos de una variable de tipo `U`.

**Ejemplo:**

```sh
<
Reg Circle 
  Fl centerX
  Fl centerY
  Fl radius
>

<
Reg Rectangle
  Fl topLeftX
  Fl topLeftY
  Fl bottomRightX
  Fl bottomRightY
>

<
U Shape 
  Circle c
  Rectangle r
>

<
areaCirculo(Circle ?c) Fl:
  return 3.14 * c.radius * c.radius
>

Shape sh
sh.c.centerX = 2.1
sh.c.centerY = 5.0
sh.c.radius = 15
out "El area del circulo es ", areaCirculo(sh.c)
```

#### Apuntadores.

Son un tipo de dato especial que guarda el elemento en memoria dinámica (heap)
de forma referencial. En memoria son una palabra del procesador. Su sintaxis
es: `<tipo al que apunta> $ <nombre>`. Por default son inicializados con `Nlp`.

**Ejemplo puntero a entero**
```sh
In $ p = new In
$p = 13
free p
```
**Ejemplo puntero a array de enteros**
```sh
In $ p = new In[20]
$p[1] = 15
free[] p
```

**Ejemplo cambiar variable apuntada**
```sh
In $ p = new In
$p = 15
free p

p = new In
$p = 18
free p
```
La función `free` y `free[]` liberan al puntero p y les asigna `Nlp`.

## Instrucciones y Expresiones.

### Asignación

La asignación se realiza mediante el operador `=`. 
Puede realizarse al momento de declarar una variable o posterior a su
declaración. Dos variables pueden asignarse entre sí, si y solo si son del
mismo tipo Si las variables son del mismo tipo se pueden declarar en la misma
línea. La sintaxis de la declaración de varias variables en la misma línea es:

```sh
<tipo1> <identificador1>, ..., <identificadorN>
<tipo2> <identificadorA> = <Expresión de tipo2>, <identificadorB> = <Expresión de tipo2> ...
```

#### Asignaciones especiales: Incremento (`++`) y Decremento (`--`).

Son asignaciones que funcionan exclusivamente sobre variables de tipo entero y
funcionan como operadores sufijos/postfijos:

* Incremento (`++`): Incrementa el valor de la variable en 1.
* Decremento (`--`): Disminuye el valor de la variable en 1.

**Ejemplo:**

```sh
In n1 = 15, n2 = 13
n1++
n2--
In n3 = n1, n4 = n2
```

Al ejecutar este código se declaran e inicializan las variables `n1` y `n2`
con los valores `15` y `13` respectivamente. Se incrementa la primera y se
disminuye la segunda, luego se declaran e inicializan las variables `n3` y
`n4` con los valores contenidos en `n1` y `n2`, respectivamente.

#### Identificadores de variables.

Un identificador válido para una variable en un programa de CHask* cumple con
las siguientes condiciones:

1. No empieza con números.
2. Puede poseer carácteres `A` hasta la `Z` en mayúscula o minúscula.
3. No incluye el carácter `ñ` ni carácteres acentuados.
4. Puede poseer números del `0` al `9`.
5. Puede poseer los carácteres especiales `_` y `'`.
6. No posee espacios.
7. No corresponde a ninguna palabra reservada de CHask*.

Siendo así, son ***nombres válidos***: `test1`, `yes_we_can`, `maybe'not`. Son
***nombres inválidos***: `1test1`, `yes_we_cañ`, `maybe not`, `while`.

Se diferencian mayúsculas de minúsculas así, `test1`, `tEst1` son
identificadores diferentes.

##### Declaración e inicialización de variables.

Se permite la declaración de variables sin previa inicialización. Si la
variable es de tipo escalar o puntero será inicializada automáticamente según
se indicó en cada tipo escalar.

### Condicional.

Ver sección **Estructuras de Datos. Selección**.

### Repetición.

Ver sección **Estructuras de Datos. Repetición**.

### Entrada/Salida.

Las operaciones de entrada/salida serán realizadas con las instrucciones
`input` y `out` como se sigue:

```sh
<variable> = input [<prompt>]
out <variable>
```

La función `input` recibe un `Str` opcional como `prompt` para el usuario y
lee un `Str` del usuario. Si la variable `lvalue` que recibe el resultado de
`input` es de tipo `In` , `Fl` el lenguaje te convierte el `Str` resultado a
dicho tipo. Su ejecución consiste en una interrupción para leer de la entrada
estándar y retorna lo obtenido sin el carácter de retorno de línea en caso de
un `Str`.

La función `out` toma una variable de tipo `In`, `Str`,`Cr` o `Fl` y lo
muestra en la salida estándar, puede recibir varios argumentos separados por
coma. Las cadenas de carácteres deben estar encerradas entre comillas dobles
(") y sólo debe contener carácteres imprimibles.

Estas funciones se pueden llamar sin utilizar paréntesis.

En CHask* existen las siguientes funciones utiles:

* `convertInToStr`: Recibe un Int `In` regresa un `Str`.
* `convertCrToStr`: Recibe un Char `Cr` regresa un `Str`.
* `convertFlToStr`: Recibe un Float `Fl` regresa un `Str`.
* `convertStrToIn`: Recibe un `Str` y regresa un Int `In`.
* `convertStrToCr` : Recibe un `Str` y regresa un Char `Cr`.
* `convertStrToFl`: Recibe un `Str` y regresa unFloat `Fl`.

### Evaluación condicional (`?`).

Es una expresión de la forma:

`<Expresión Booleana> ? <Función Caso T> : <Funcion Caso F>`

Es una expresión que evalúa la `<Función Caso T>` o la `<Función Caso F>`
dependiendo de si `<Expresión Booleana>` evalúa `T` o `F`.

### Interrupción de evaluación (`break`).

Es una instrucción que permite interrumpir la ejecución de una repetición
`for` o `while` cuando es alcanzada. Sólo interrumpe el bucle más cercano.

### Expresiones Aritméticas.

Corresponden a las expresiones que devuelven valores numéricos después de
evaluarse. Los valores numéricos pueden ser indiferentemente enteros o punto
flotante. Las expresiones aritméticas son aquellas que involucren los
operadores de:

* Adición (`+`): definida tradicionalmente.
* Sustracción (`-`): definida tradicionalmente.
* Multiplicación (`*`): definida tradicionalmente.
* División (`/`): toma dos números y devuelve un punto flotante.
* División entera (`//`): toma dos enteros y regresa un entero que corresponde
al cociente de la división.
* Residuo (`%`): toma dos enteros y regresa un entero que corresponde al resto
de la división.
* Menos unario (`-`): toma un entero y lo multiplica por `-1`.
* Longitud (`#`): toma un arreglo y devuelve la cantidad de elementos que
contiene.

### Expresiones Booleanas.

Corresponden a las expresiones que devuelven valores booleanos `T` o `F`
después de evaluarse.

Las expresiones booleanas que reciben booleanos son aquellas que involucran a
los operadores de:

* Disyunción (`||`): definida tradicionalmente.
* Conjunción (`&&`): definida tradicionalmente.
* Negación (`!`): definida tradicionalmente.

Las expresiones booleanas que reciben cualquier tipo escalar son aquellas que
involucran comparaciones:

* Igualdad (`==`): definida tradicionalmente. Para apuntadores se define como
igualdad de los elementos apuntados.
* Desigualdad (`!=`): definida tradicionalmente. Para apuntadores se define
como desigualdad de los elementos apuntados.
* Mayor que (`>`): definida tradicionalmente para enteros y punto flotante.
Para carácteres se sigue el orden léxico normal.
* Mayor o igual que (`>=`): definida tradicionalmente para enteros y punto
flotante. Para carácteres se sigue el orden léxico normal.
* Menor que (`<`): definida tradicionalmente para enteros y punto flotante.
Para carácteres se sigue el orden léxico normal.
* Menor o igual que (`<=`): definida tradicionalmente para enteros y punto
flotante. Para carácteres se sigue el orden léxico normal.
 
### Expresiones de Carácteres.

Corresponden a las expresiones que devuelven un carácter después de su
evaluación. Son aquellas en las cuales están involucrados los operadores
monádicos prefijos de:

* Mayúscula (`^`): convierte el carácter alfabético dado a su representación en
mayúscula.
* Minúscula (`~`): convierte el carácter alfabético dado a su representación en
minúscula.

### Expresiones para Arreglos y Listas.

Corresponden a las expresiones que devuelven un arreglo o lista después de su
evaluación. Son aquellas en las cuales están involucrados los operadores de:

* Indexación (`[]`): toma un arreglo o lista y un entero `i` en el rango
`[0..#<arreglo o lista> - 1]` y devuelve el elemento contenido en la posición
`i`.
* Concatenación (`::`): toma dos listas y concatena la segunda a la cola del
primero.
* Anexo (`:`): toma un elemento cualquiera y lo agrega a la cabeza de la lista.

### Expresiones para Registros y Registros variantes.

Corresponden a las expresiones que involucran registros y uniones. Son
aquellas en las cuáles está involucrado el operador de acceso (`.`) el cual
toma un registro o una unión y un identificador y si el nombre coincide con
alguno de los campos del registro o la unión, se devuelve el valor almacenado
en el campo.

### Expresiones para Apuntadores.

Corresponden a las expresiones que involucran a variables del tipo apuntador,
el tipo apuntado y el operador de desrreferenciación (`$`) el cual dada una
variable del tipo apuntador, devuelve el elemento que era apuntado por la
variable dada.

Del mismo modo, otra expresión válida para apuntadores es aquella que no
incluye al operador de desrreferenciación. En ese caso la expresión devuelve la
dirección del elemento apuntado.

## Bloques y Alcance de bloques.

Un bloque dentro del programa está delimitado por llaves `<` y `>` y se
obtiene al estar dentro de instrucciones condicionales, de repetición,
funciones y registros.

El alcance es estático (o léxico), esto quiere decir, que dentro de cada
alcance si se hace referencia a una variable que no está dentro de ese alcance
se busca en el alcance donde fue definido el bloque actual y así,
recursivamente hasta alcanzar el alcance global.

## Evaluación.

### Precedencia de los operadores.

En orden descendente, la precedencia de los operadores es:

|    Tipo    |      Primer Nivel      |     Segundo nivel      |       Tercer nivel       |     Cuarto nivel     |
| ---------- | ---------------------- | ---------------------- | ------------------------ | -------------------- |
| Aritmético | Menos Unario, Longitud | División Entera, Resto | Multiplicación, División | Adición, sustracción |
|  Booleano  |        Negación        | Conjunción, Disyunción |                          |     Comparadores     |
|  Carácter  |       Mayúscula        |       Minúscula        |                          |                      |
|  Arreglos  |       Indexación       |                        |                          |                      |
|   Listas   |   Indexación, Anexo    |     Concatenación      |                          |                      |
|  Registro  |         Acceso         |                        |                          |                      |
|  Apuntador |    Desreferenciación   |                        |                          |                      |

### Asociatividad de los operadores.

|    Tipo    |                     Izquierda                    |                    Derecha                   |   No asocia  |
| ---------- | ------------------------------------------------ | -------------------------------------------- | ------------ |
| Aritmético | División, División Entera, Resto, Multiplicación | Adición, sustracción, Menos Unario, Longitud |              |
|  Booleano  |              Conjunción, Disyunción              |                   Negación                   | Comparadores |
|  Carácter  |                                                  |            Mayúsculas, Minúsculas            |              |
|  Arreglos  |                    Indexación                    |                                              |              |
|   Listas   |         Indexación, Anexo, Concatenación         |                                              |              |
|  Registro  |                      Acceso                      |                                              |              |
|  Apuntador |                                                  |               Desreferenciación              |              |

### Orden de evaluación de las expresiones.

Las expresiones se evalúan de izquierda a derecha.

# Estructuras de control de flujo.

## Selección.

La selección en CHask* es de la forma siguiente.

```sh
| <Expresión Booleana>:
  <Lista de Instrucciones>
| <Expresión Booleana>:
  <Lista de Instrucciones>
...
| else:
  <Lista de Instrucciones>
```

Sólo es necesaria una condición, las demás son opcionales y se evaluarán si
la condición anterior es falsa. Si todas son falsas se ejecuta el contenido
de la condición `| else:` en caso de existir. 

**Ejemplo (una sola condición)**:

```sh
Bl puedeConducir = F
In edad = 18
| edad >= 18:
  puedeConducir = T
```

Al ejecutarse este código el nuevo valor de la variable `puedeConducir` es `T`.

### Repetición.

### Determinada.

La repetición determinada es un ciclo `for` como se sigue:

```sh
<
for <variable>[, <Nombre del índice para el arreglo>] in <arreglo>:
  <Lista de Instrucciones>
>
```

En cada iteración, `<variable>` tendrá a un elemento de `<arreglo>` y las
iteraciones culminarán cuando ya no haya más elementos en `<arreglo>` sobre
los cuales iterar. Los corchetes implican que lo que está encerrado en ellos
es opcional en la sintaxis de un programa de CHask*.

`<Nombre del índice para el arreglo>` corresponde a una variable que sirve de
contador al momento de iterar sobre un arreglo.

**Ejemplo:**

```sh
Str[5] edades = ["12", "23", "15", "40", "15"] 
Str[5] nombres = ["Natascha", "Francisco", "Manuel", "Ricardo", "Haskell"] 

<
for edad, i in edades:
  out "Hola ", nombres[i], " tienes ", edad, " años!"
>
```

Al ejecutar este código se imprime en pantalla:

```
Hola Natascha tienes 12 años!
Hola Francisco tienes 23 años!
Hola Manuel tienes 15 años!
Hola Ricardo tienes 40 años!
Hola Haskell tienes 15 años!
```

### Indeterminada.

La repetición indeterminada es un ciclo `while` como se sigue:

```sh
<
while <Expresión Booleana>:
  <Lista de Instrucciones>
>
```
`<Lista de Instrucciones>` se seguirá ejecutando hasta que `<Expresión Booleana>` sea `F`

**Ejemplo:**

```sh
Str[5] edades = ["12", "23", "15", "40", "15"] 
Str[5] nombres = ["Natascha", "Francisco", "Manuel", "Ricardo", "Haskell"] 
In i = 0
<
while i < 5:
  out "Hola ", nombres[i], " tienes ", edades[i], " años!"
  i++
>
```

Al ejecutar este código se imprime en pantalla:

```
Hola Natascha tienes 12 años!
Hola Francisco tienes 23 años!
Hola Manuel tienes 15 años!
Hola Ricardo tienes 40 años!
Hola Haskell tienes 15 años!
```

## Subrutinas.

### Funciones y procedimientos.

Las funciones se definen como estructuras de control de flujo que toman
una cantidad arbitraria de argumentos definidos por el programador,
pudiendo incluso no tomar argumento alguno.

Las funciones toman cualquier tipo de dato y retornan un tipo de dato
escalar (ver **Tipos de datos**).

Los procedimientos son funciones que siempre retornan el valor unit,
el cual no tiene más uso que este.

Para retornar o salir de una función se utiliza `return <variable o constante>`
donde `<variable o constante>` es del mismo tipo que el tipo de retorno de la
función.

La sintaxis de una función es:

```sh
<
nombre(<tipo> <parámetro formal>, <tipo> <parámetro formal>, ..., <tipo> <parámetro formal>) <tipo>:
  <Lista de instrucciones>
  return <variable o constante>
>
```

**Ejemplo:**

```sh
<
calcularGanancia(In precioComprado, In precioVendido) In:
  return precioVendido - precioComprado
>

out "Ganancia de ", calcularGanancia(1500, 2000)
```

Al ejecutar este código se imprime en pantalla:

```
Ganancia de 500
```

##### Pasaje de parámetros

Se admite el paso de parámetros por valor y por referencia.
Se diferencia el pase por referencia por el signo de interrogación
(`?`) prefijo a uno de los parámetros. El pase por valor no requiere
ninguna sintaxis adicional.

La sintaxis es:

```sh
<
función(<tipo> <parámetro por valor>, <tipo> ?<parámetro por referencia>)<tipo de retorno>:
  <Lista de instrucciones>
  return <variable o constante>
>
```

**Ejemplo (Paso por valor y referencia):**

```sh
<
calcularGanancia(Producto ?producto, In precioVendido) In:
  return precioVendido - producto.precioReal
>
<
Reg Producto 
  In precioReal;
>

Producto p
p.precioReal = 1500
out "Ganancia de ", calcularGanancia(p, 2000)
```

Al ejecutar este código se imprime en pantalla:

```
Ganancia de 500
```

**Nota:** Un caso particular es cuando se pasa un puntero a una función que
espera un argumento por referencia, en este caso se pasaría el valor del puntero.

#### Recursión.

CHask* admite la invocación recursiva de funciones en cualquier momento.

Ejemplo de sintaxis:

```sh
<
procedimiento(<tipo> <parámetro>):
  <Lista de instrucciones>
  <Llamada recursiva>
  <Llamada a función>
}

<
función(<tipo> <parámetro>)<tipo>:
  <Lista de instrucciones>
  <Llamada a procedimiento>
  return <variable o constante>
>

<Llamada a función>
```

**Ejemplo factorial:**

```sh
<
factorial(In n) In:
  | n < 0: return 0
  | n == 0: return 1
  | n > 0: return n * factorial(n-1)
>
In numero;
numero = input "Ingresa un numero: "
out "Factorial de ", numero, " es ", factorial(numero)
```

Al ejecutar este código, en pantalla se muestra:

```
Ingresa un numero: 5
Factorial de 5 es 120
```

**Ejemplo:**

```sh
Int[5] primerosCinco = [1, 2, 3, 4, 5]

<
duplicar(Int[] numeros):
  <
  for elemento in numeros:
    elemento = elemento * 2
  >
>

duplicar(?primerosCinco)
<
for numero in primerosCinco:
  out numero
>
```

Al ejecutar este código, se muestra en pantalla:

```
2
4
6
8
10
```

## Comentarios y espacios en blanco.

En CHask* se pueden escribir comentarios de una línea o de varias líneas.
Al escribir `@` se ignorarán todos los carácteres hasta el siguiente salto
de línea. El texto escrito entre `{-` y `-}` será ignorado. Los espacios en
blanco también son ignorados.

## Ejemplos en CHask*.

**Calcular el volúmen de un Cubo**
```sh
{- Calcula el volumen de un Cubo -}
Fl arista, volumen;

arista = input "Introduzca arista: "

volumen = arista * arista * arista;

out "El volumen del cubo es: ", volumen
```

**Dice si un número es par o impar**

```sh
{- Dice si un número es par o impar -}
In numero
numero = input "Introduzca un numero entero: "
|numero % 2 == 0 :
  out "ES PAR"
| else:
  out "ES IMPAR"

```

**Tablas de multiplicar**
```sh
Ch seguir = 's';
In i, numero;
<
while seguir != 'n':
  numero = input "Introduzca un numero entero: "

  out "La tabla de multiplicar del ",numero," es:"

  @ Inicio del anidamiento
  <
  i = 1 
  while i <= 10:
    out numero, " * ", i, " = ", i * numero
    i++
  >
  @ Fin del anidamiento

  seguir = convertStrToCr( input "Desea ver otra tabla (s/n)?: ")
>
```

## Extras.

La presente sección contiene algunas funcionalidades del Chask* que
**tentativamente** pueden ser desarrolladas con el lenguaje.

### Intérprete.

### Hilos.

### Carácteres UTF-8.

### Excepciones.

### Iteradores.
