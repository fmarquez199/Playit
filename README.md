[![N|Solid](http://www.usb.ve/conocer/corporativa/archivos/logos/logotipo/logotip.png)](http://www.usb.ve)

# **EL Lenguaje de Programación Playit.**

**Diseñado por:**

* Manuel Gonzalez 11-10390
* Francisco Javier 12-11163
* Natascha Gamboa 12-11250

Playit, es un **lenguaje imperativo** de propósito general, no orientado a
objetos, compilado y fuertemente tipado, inspirado por el mundo de los
videojuegos, tomando conceptos y filosofías de los mismos. Está diseñado e
implementado por estudiantes de Ingeniería de Computación de la **Universidad 
Simón Bolívar** en el transcurso de la cadena de **Lenguajes de Programación
(CI-4721, CI-4722)**.

**Playit cuenta con:**

* Tipos de datos primitivos (Enteros, Caracteres, Flotantes, Booleanos).
* Apuntadores a memoria en el Heap.
* Arreglos, Registros, Registros variantes.
* Lectura desde la entrada estándar y Escritura a la salida estándar.
* ¡Amor! :kissing_closed_eyes:

---
---
# **Estructura de un programa.**

Un programa en Playit tiene la siguiente estructura:

```
<Declaraciones>
world %<nombre del programa>%:
  <Lista instrucciones>
.~
```

No está permitido un programa cuya lista de instrucciones sea vacía.

Un programa simple en Playit es mostrar por la salida estándar *Hola Mundo!*
(*Hello World*):

```sh
world %HolaMundo%:
  drop ~Hello World!~
.~
```

---
---
## **Extensión del archivo de código fuente.**

Un archivo de código fuente en Playit debe tener la extensión `.game` para ser reconocido por el compilador.

---
---
# **Tipos de datos. Escalares.**

## **Caracteres.**

Son datos de **8 bit (1 B)** en memoria y se caracterizan por ser un carácter
*ASCII* encerrado entre asteriscos (`**`). Su palabra reservada es **`Rune`**, y su valor por defecto es `*\0*`.

Se distinguen las secuencias de escape por ser aquellos precedidos por un
backslash (`\`):

* `*\0*` (carácter nulo).
* `*\n*` (salto de espacio).
* `*\t*` (tabulación de 4 espacios).
* `*\\*` (backslash).
* `*\~*` (virgulilla).
* `*\**` (asterisco).

---
## **Booleanos.**

Son datos de **8 bit (1 B)** en memoria cuyos valores son **`Win`** y **`Lose`**. Su palabra reservada es **`Battle`**, y su valor por defecto es `Lose`.

Nota: `Lose` corresponde en memoria a los ocho bit en 0 y `Win` a cualquier otra
combinación.

---
## **Enteros.**

Son datos de **32 bit (4 B)** en memoria que pueden ser cualquier cadena no vacía
de números de la base decimal. Su representación es **complemento a 2**, por lo
tanto los enteros están acotados en el rango:

`[-2 147 483 648, 2 147 483 647]`.

Su palabra reservada es **`Power`** y su valor por defecto es `0`.

Se consideran enteros a las cadenas de números de la base decimal
precedida por una cantidad no nula de `0`, es decir, son enteros:

```sh
01
003
0000000000000000000000000000000000000005
```

---
## **Números de punto flotante.**

Son datos de **64 bit (8 B)** en memoria representados según el estándar **IEEE
754 de precisión doble**. Se construyen como:

**`<Entero>'<Entero>`**

Su palabra reservada es **`Skill`** y su valor por defecto es  `0'0`.

---
---
# **Tipos de datos. Compuestos.**

## **Strings.**

Son arreglos de caracteres, es decir que admiten la
representación de arreglo y de cadena de caracteres, encerrados entre virgulillas
(`~~`):

```sh
|}*s*, *t*, *r*, *i*, *n*, *g*, *s*{| == ~strings~
```

Pueden definirse con **`Runes`** o **`Rune|}<entero>{|`**. De
ser declarados como `Runes` serán inicializados con la cadena vacía `~~` por
defecto.

---
## **Arreglos.**

Es una estructura de datos homogénea de cualquier tipo escalar que se
encuentran ubicados de forma consecutiva en memoria, se admiten arreglos
multidimensionales. 

La representación de un arreglo unidimensional en un programa es
**`|}`**`elemento0, elemento1, ..., elementoN`**`{|`**, siendo todos los elementos del mismo
tipo. Son estructuras estáticas y su tamaño debe ser definido en su
declaración. 

Se declaran de la siguiente manera:

**`<tipo>|}<entero>{| <nombre arreglo>`**, donde `<tipo>` puede ser otro arreglo.

Son construcciones válidas:

```sh
<tipo1>|}<entero>{|
<tipo2>|}<entero1>{||}<entero2>{|
...
<tipoN>|}<entero1>{||}<entero2>{|...|}<enteroN>{|
```

Para acceder a los elementos de un arreglo se utiliza el operador de indexación
**`|) indice (|`** y su inicialización por defecto se hace inicializando cada 
posición del arreglo en el valor por defecto del tipo dado.

Para acceder a los elementos de un arreglo se utiliza `|) indice (|`

---
## **Listas.**

Es una estructura de datos heterogénea de cualquier tipo escalar que se
encuentran ubicadas, no necesariamente de forma consecutiva, en memoria, se
admiten listas multidimensionales.

Se declaran de la siguiente manera:

**`Kit of <tipo>`**, donde `<tipo>` puede ser otra lista.

Para acceder a un elemento de la lista se utiliza el operador de indexación
**`|> índice <|`** y su inicialización por defecto es la lista vacía **`<<>>`**.

---
## **Registros.**

La palabra reservada para definirlos es **`Inventory`** y su sintaxis es de la forma:

```sh
Inventory <nombre>:
  <tipo> <nombre> [= <inicialización por default>]
  <tipo> <nombre> [= <inicialización por default>]
  ...
  <tipo> <nombre> [= <inicialización por default>]
.~
```

Cada campo del registro corresponde a un tipo escalar o compuesto definido
previamente en el programa. Cada instancia de un registro
corresponde a un tipo de dato que también puede ser parte de otro registro.

Su tamaño en memoria corresponde a la suma de los tamaños individuales de cada
campo que posea. Los tipos que acepta un registro son todos aquellos tipos que
están definidos en **Playit**.

Para inicializar una declaración se usan las llaves **`{}`** y se pasan los valores separados por una coma **`,`** o se hace referencia a sus campos con la palabra reservada **`spawn`**, de la siguiente manera:

```sh
Inventory Registro:
  Power up
  Rune tip
.~
Registro r = {0, *f*}

r spawn up = 5
r spawn tip = *c*
```

---
## **Registros variantes.**

La palabra reservada para definirlos es **`Items`** y su sintaxis es de la forma:

```sh
Items <nombre>:
  <tipo> <nombre>
  <tipo> <nombre>
  ...
  <tipo> <nombre>
.~
```

Su tamaño en memoria corresponde al campo de mayor tamaño. Los tipos
que acepta son todos aquellos que están definidos en **Playit**.

Para inicializar una variable de tipo `Items` se debe referenciar al
campo que se desea inicializar con la palabra reservada **`spawn`**. No se permite la inicialización de todos sus campos.

**Ejemplo:**

```sh
Inventory Circle:
  Skill centerX
  Skill centerY
  Skill radius
.~

Inventory Rectangle:
  Skill topLeftX
  Skill topLeftY
  Skill bottomRightX
  Skill bottomRightY
.~

Items Shape:
  Circle c
  Rectangle r
.~

Shape sh
sh spawn c spawn centerX = 2'1
sh spawn c spawn centerY = 5'0
sh spawn c spawn centerY = 5'0
sh spawn c spawn radius = 15
```

---
## **Apuntadores.**

Son un tipo de dato especial que guarda el elemento en memoria dinámica (heap)
de forma referencial. En memoria son una palabra del procesador. Su sintaxis
es:

**`<tipo al que apunta> puff <nombre>`**.

Se declaran con la palabra reservada **`summon`** y se inicializan por defecto 
apuntando a un espacio vacío en memoria con **`DeathZone`**, para liberar su 
espacio en memoria se usa **`free`**, **`free|}{|`** para arreglos y **`free<<>>`** 
para listas, y se les asigna `DeathZone`.

---
---
# **Instrucciones.**

## **Asignación.**

La asignación se realiza mediante el operador **`=`**. Puede realizarse al momento
de declarar una variable o posterior a su declaración. Dos variables pueden
asignarse entre sí, si y solo si son del mismo tipo. Si las variables son del
mismo tipo se pueden declarar en la misma línea. La sintaxis de la declaración
de varias variables en la misma línea es:

```sh
<tipo1> <identificador1>, ..., <identificadorN>
<tipo2> <identificadorA> = <Expresión de tipo2>, <identificadorB> = <Expresión de tipo2> ...
```

### **Asignaciones especiales: Incremento (`++`) y Decremento (`--`)**.

Son asignaciones que funcionan exclusivamente sobre variables de tipo entero y
funcionan como operadores sufijos/postfijos:

* Incremento (**`++`**): Incrementa el valor de la variable en 1.
* Decremento (**`--`**): Disminuye el valor de la variable en 1.

---
## **Identificadores de variables.**

Un identificador válido para una variable en un programa de **Playit** cumple con
las siguientes condiciones:

1. No empieza con números, **`_`** o **`'`**.
2. Puede poseer caracteres **`A`** hasta la **`Z`** en mayúscula o minúscula.
3. No incluye el carácter **`ñ`** ni caracteres acentuados.
4. Puede poseer números del **`0`** al **`9`**.
5. Puede poseer los caracteres especiales **`_`** y **`'`**.
6. No posee espacios.
7. No corresponde a ninguna palabra reservada de **Playit**.

Siendo así, son ***nombres válidos***: `test1`, `yes_we_can`, `maybe'not`. Y son
***nombres inválidos***: `1test1`, `yes_we_cañ`, `maybe not`.

### **Declaración e inicialización de variables.**

Se permite la declaración de variables sin previa inicialización. Si la
variable es de tipo escalar o puntero será inicializada con los valores por 
defecto descritos para cada tipo escalar.

---
## **Condicional.**

Ver sección **Estructuras de Control de Flujo. Selección**.

---
## **Repetición.**

Ver sección **Estructuras de Control de Flujo. Repetición**.

---
## **Entrada/Salida.**

Las operaciones de entrada/salida serán realizadas con las instrucciones
**`joystick`** y **`drop`** como se sigue:

```sh
<variable> = joystick [<prompt>]
drop <variable>
```

La función `joystick` recibe un `Runes` opcional como `prompt` para el usuario
y lee un `Runes` del usuario. Si `<variable>` es de tipo `Rune`, `Power` o
`Skill` **Playit** convierte el `Runes` leído a dicho tipo. Su ejecución
consiste en una interrupción para leer de la entrada estándar y retorna lo
obtenido sin el carácter `*\n*` en caso de un `Runes`.

La función `drop` toma una variable o constante de tipo `Power`, `Rune`,
`Runes` o `Skill` y muestra su contenido en la salida estándar, puede recibir
varios argumentos separados por coma. Las cadenas de caracteres sólo deben
contener caracteres imprimibles.

Estas funciones se pueden llamar sin utilizar paréntesis, ni utilizar la
sentencia `kill` (Ver sección **Estructuras de Control de Flujo. Subrutinas**).

En **Playit** existen las siguientes funciones útiles:

* `portalRuneToRunes`: Recibe un `Rune` y regresa un `Runes`.
* `portalPowerToRunes`: Recibe un `Power` y regresa un `Runes`.
* `portalSkillToRunes`: Recibe un `Skill` y regresa un `Runes`.
* `portalRunesToPower`: Recibe un `Runes` y regresa un `Power`.
* `portalRunesToRune` : Recibe un `Runes` y regresa un `Rune`.
* `portalRunesToSkill`: Recibe un `Runes` y regresa un `Skill`.

---
## **Evaluación condicional (`?:`).**

Es una expresión de la forma:

**`<Expresión Booleana> ? <Función Caso Win> : <Funcion Caso Lose>`**

Es una expresión que evalúa y regresa el resultado de `<Función Caso Win>` o 
`<Función Caso Lose>` dependiendo de si `<Expresión Booleana>` evalúa `Win` o `Lose`. 

---
---
# **Expresiones.**

## **Aritméticas.**

Corresponden a las expresiones que devuelven valores numéricos después de
evaluarse. Los valores numéricos pueden ser indiferentemente enteros o punto
flotante. Las expresiones aritméticas son aquellas que involucren los
operadores de:

* **Adición (`+`):** definida tradicionalmente.
* **Sustracción (`-`):** definida tradicionalmente.
* **Multiplicación (`*`):** definida tradicionalmente.
* **División (`/`):** toma dos números y devuelve un punto flotante.
* **División entera** (`//`): toma dos enteros y regresa un entero que corresponde
al cociente de la división.
* **Residuo (`%`):** toma dos enteros y regresa un entero que corresponde al resto
de la división.
* **Menos unario** (`-`): toma un entero y lo multiplica por `-1`.
* **Longitud (`#`):** toma un arreglo y devuelve la cantidad de elementos que
contiene.

---
## **Booleanas.**

Corresponden a las expresiones que devuelven valores booleanos **`Win`** o **`Lose`**
después de evaluarse.

Las expresiones booleanas que reciben booleanos son aquellas que involucran a
los operadores de:

* **Disyunción (`||`):** definida tradicionalmente.
* **Conjunción (`&&`):** definida tradicionalmente.
* **Negación (`!`):** definida tradicionalmente.

Las expresiones booleanas que reciben cualquier tipo escalar son aquellas que
involucran comparaciones:

* **Igualdad (`==`):** definida tradicionalmente. Para apuntadores se define como
igualdad de los elementos apuntados.
* **Desigualdad (`!=`):** definida tradicionalmente. Para apuntadores se define
como desigualdad de los elementos apuntados.
* **Mayor que (`>`):** definida tradicionalmente para enteros y punto flotante.
Para caracteres se sigue el orden léxico normal.
* **Mayor o igual que (`>=`):** definida tradicionalmente para enteros y punto
flotante. Para caracteres se sigue el orden léxico normal.
* **Menor que (`<`):** definida tradicionalmente para enteros y punto flotante.
Para caracteres se sigue el orden léxico normal.
* **Menor o igual que (`<=`):** definida tradicionalmente para enteros y punto
flotante. Para caracteres se sigue el orden léxico normal.

---
## **Expresiones de Caracteres.**

Corresponden a las expresiones que devuelven un carácter después de su
evaluación. Son aquellas en las cuales están involucrados los operadores
monádicos prefijos de:

* **Mayúscula (`^`):** convierte el carácter alfabético dado a su representación en
mayúscula.
* **Minúscula (`.`):** convierte el carácter alfabético dado a su representación en
minúscula.

---
## **Expresiones para Arreglos y Listas.**

Corresponden a las expresiones que devuelven un arreglo o lista después de su
evaluación. Son aquellas en las cuales están involucrados los operadores de:

* **Indexación (`|)(|`):** toma un arreglo y un entero `i` en el rango
`[0..#<arreglo> - 1]` y devuelve el elemento contenido en la posición `i`.
* **Indexación (`|><|`):** toma un lista y un entero `i` en el rango
`[0..#<lista> - 1]` y devuelve el elemento contenido en la posición `i`.
* **Concatenación (`::`):** toma dos listas y concatena la segunda a la cola de
la primera.
* **Anexo (`:`):** toma un elemento cualquiera y lo agrega a la cabeza de la lista.

---
## **Expresiones para Registros y Registros variantes.**

Corresponden a las expresiones que involucran registros y uniones. Son
aquellas en las cuáles está involucrado el operador de acceso **`spawn`** el cual
toma un registro o una unión y un identificador y si el nombre coincide con
alguno de los campos del registro o la unión, se devuelve el valor almacenado
en el campo.

---
## **Expresiones para Apuntadores.**

Corresponden a las expresiones que involucran a variables del tipo apuntador,
el tipo apuntado y el operador de desrreferenciación **`puff`** el cual dada una
variable del tipo apuntador, devuelve el elemento que era apuntado por la
variable dada.

Del mismo modo, otra expresión válida para apuntadores es aquella que no
incluye al operador de desrreferenciación. En ese caso la expresión devuelve la
dirección del elemento apuntado.

---
---
# **Bloques y Alcance de bloques.**

Un bloque dentro del programa está delimitado por el nombre que representa la
instrucción y al final **`.~`**, y se obtiene al estar dentro de instrucciones
condicionales, de repetición, subrutinas y registros, y dentro del programa principal.

El alcance es estático (o léxico), esto quiere decir, que dentro de cada
alcance si se hace referencia a una variable que no está dentro de ese alcance
se busca en el alcance donde fue definido el bloque actual y así,
recursivamente hasta alcanzar el alcance global.

---
---
# **Evaluación.**

## **Precedencia de los operadores.**

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

---
## **Asociatividad de los operadores.**

|    Tipo    |                     Izquierda                    |                    Derecha                   |   No asocia  |
| ---------- | ------------------------------------------------ | -------------------------------------------- | ------------ |
| Aritmético | División, División Entera, Resto, Multiplicación | Adición, sustracción, Menos Unario, Longitud |              |
|  Booleano  |              Conjunción, Disyunción              |                   Negación                   | Comparadores |
|  Carácter  |                                                  |            Mayúsculas, Minúsculas            |              |
|  Arreglos  |                    Indexación                    |                                              |              |
|   Listas   |         Indexación, Anexo, Concatenación         |                                              |              |
|  Registro  |                      Acceso                      |                                              |              |
|  Apuntador |                                                  |               Desreferenciación              |              |

---
## **Orden de evaluación de las expresiones.**

Las expresiones se evalúan de izquierda a derecha.

---
---
# **Estructuras de control de flujo.**

## **Selección.**

La palabra reservada para su definición es **`Button`**, para la condición por 
defecto se usa **`notPressed`**. Su sintaxis es de la siguiente forma:

```sh
Button:
| <Expresión Booleana> }
  <Lista de Instrucciones>
| <Expresión Booleana> }
  <Lista de Instrucciones>
...
| notPressed }
  <Lista de Instrucciones>
.~
```

Sólo es necesaria una condición, las demás son opcionales y se evaluarán si
la condición anterior es falsa. Si todas son falsas se ejecuta el contenido
de la condición `| notPressed }` en caso de existir. 

---
## **Repetición.**

### **Determinada.**

Las palabras reservadas para su definición son **`controller`** y **`[lock]`**(opcional), 
teniendo las siguientes sintaxis:

```sh
controller <variable de iteración> = <valor inicial> -> <valor final> [lock <condición>]:
  <Lista de Instrucciones>
.~
```

En esta sintaxis, `<variable de iteración>`, comienza en `<valor inicial>`
hasta `<valor final>`, restringida a una `<condición>`, la cual es opcional.

```sh
controller <variable> <- <lista o arreglo>:
  <Lista de Instrucciones>
.~
```

En esta sintaxis, en cada iteración, `<variable>` tendrá a un elemento de
`<lista o arreglo>` y las iteraciones culminarán cuando ya no haya más
elementos sobre los cuales iterar.

### **Indeterminada.**

Las palabras reservadas para su definición son **`play`** y **`lock`**, 
teniendo la siguiente sintaxis:

```sh
play:
  <Lista de Instrucciones>
lock <Expresión Booleana>
.~
```
`<Lista de Instrucciones>` se seguirá ejecutando hasta que `<Expresión Booleana>` 
sea `Lose`.

### **Interrupción de evaluación (`gameOver`).**

Es una instrucción que permite interrumpir la ejecución de una repetición
`play` o `controller` cuando es alcanzada. Sólo interrumpe el bucle más
cercano.

### **Adelanto de iteración (`keepPlaying`).**

Es una instrucción que permite adelantar las instrucciones dentro de una
iteración, cuando esta instrucción es alcanzada dentro de una repetición
`play` o `controller`.

---
---
# **Subrutinas.**

## **Funciones.**

Las funciones se definen como estructuras de control de flujo que toman
una cantidad arbitraria de argumentos definidos por el programador,
pudiendo incluso no tomar argumento alguno.

Las funciones toman cualquier tipo de dato y retornan un tipo de dato escalar
(ver **Tipos de datos**). La palabra reservada para declararlas es **`monster`**.

Para retornar o salir de una función se utiliza **`unlock <variable o constante>`**
donde `<variable o constante>` es del mismo tipo que el tipo de retorno de la función.

Su sintaxis es de la siguiente forma:

```sh
monster <nombre>(<tipo> <parámetro formal>, ... , <tipo> <parámetro formal>) <tipo>:
  <Lista de instrucciones>
  unlock <variable o constante>
.~
```

---
## **Procedimientos.**

Los procedimientos son funciones que siempre retornan el valor unit,
el cual no tiene más uso que este. La palabra reservada para declararlas es 
**`boss`**.

Su sintaxis es de la siguiente forma:

```sh
boss <nombre>(<tipo> <parámetro formal>, ... , <tipo> <parámetro formal>):
  <Lista de instrucciones>
.~
```
---
## **Llamadas a subrutinas.**

Para realizar una llamada a una subrutina se usa la palabra reservada **`kill`**.

```sh
kill <nombre de la subrutina>
```

---
## **Pasaje de parámetros**

Se admite el paso de parámetros por valor y por referencia. Se diferencia el
pase por referencia por el signo de interrogación (**`?`**) prefijo a uno de los
parámetros. El pase por valor no requiere ninguna sintaxis adicional.

Su sintaxis es:

```sh
monster función(<tipo> <parámetro por valor>, <tipo> ?<parámetro por referencia>) <tipo de retorno>:
  <Lista de instrucciones>
  unlock <variable o constante>
.~
```

**Nota:** Un caso particular es cuando se pasa un puntero a una función que
espera un argumento por referencia, en este caso se pasaría el valor del puntero.

---
## **Recursión.**

**Playit** admite la invocación recursiva de funciones en cualquier momento de 
la siguiente manera:

```sh
boss procedimiento(<Parámetros>):
  <Lista de instrucciones>
  kill <Llamada recursiva a procedimiento>
  kill <Llamada a función>
.~

monster función(<Parámetro>) <tipo>:
  <Lista de instrucciones>
  kill <Llamada a procedimiento>
  unlock <variable o constante>
.~

kill <función>
```

**Ejemplo factorial:**

```sh
monster factorial(Power n) Power:
  Button:
    | n < 0 } unlock 0
    | n == 0 } unlock 1
    | n > 0 } unlock n * kill factorial(n-1)
  .~
.~

Power numero
numero = joystick ~Ingresa un numero: ~
drop ~Factorial de ~, numero, ~ es ~, kill factorial(numero)
```

Al ejecutar este código, en pantalla se muestra:

```
Ingresa un numero: 5
Factorial de 5 es 120
```

---
---
# **Comentarios y espacios en blanco.**

En **Playit** se pueden escribir comentarios de una línea o de varias líneas.
Al escribir **`@`** se ignorarán todos los caracteres hasta el siguiente salto
de línea. El texto escrito entre comillas dobles más comillas simples **`"'`** 
y **`'"`** será ignorado. Los espacios en
blanco también son ignorados.

---
---
# **Ejemplos en Playit.**

**Calcular el volúmen de un Cubo**
```sh
world %VolumenCubo%:
  "' Calcula el volumen de un Cubo '"
  Skill arista, volumen
  
  arista = joystick ~Introduzca arista: ~
  volumen = arista * arista * arista
  
  drop ~el volumen del cubo es: ~, volumen
.~
```

**Dice si un número es par o impar**

```sh
world %ParOImpar%:
  "' Dice si un número es par o impar '"
  Power numero
  numero = joystick ~Introduzca un numero entero: ~

  Button:
  |numero % 2 == 0 }
    drop ~es par~
  | notPressed }
    drop ~es impar~
  .~
.~
```

**Tablas de multiplicar**
```sh
world %Tablas%:
  Rune seguir = *s*
  Power i, numero
  
  play:
    numero = joystick ~Introduzca un numero entero: ~
  
    drop ~La tabla de multiplicar del ~,numero,~ es:~
  
    @ Inicio del anidamiento
    i = 1 
    play:
      drop numero, ~ * ~, i, ~ = ~, i * numero
      i++
    lock i <= 10
    .~
    @ Fin del anidamiento
  
    seguir = kill portalRunesToRune( joystick ~Desea ver otra tabla (s/n)?: ~)
  lock  seguir != *n*
  .~
.~
```

**Ejemplo puntero a entero**
```sh
Power puff p = summon Power
puff p = 13
free p
```
**Ejemplo puntero a array de enteros**
```sh
Power puff p = summon Power|}20{|
puff p|)1(| = 15
free|}{| p
```

**Ejemplo cambiar variable apuntada**
```sh
Power puff p = summon Power
puff p = 15
free p

p = summon Power
puff p = 18
free p
```

**Ejemplo arreglos y listas**
```sh
Kit of Runes edades = <<~12~, ~23~, ~15~, ~40~, ~15~>>
Runes|}5{| nombres = |}~Natascha~, ~Francisco~, ~Manuel~, ~Ricardo~, ~Haskell~{| 

controller Power i = 0 -> 4:
  drop ~Hola ~, nombres|)i(|, ~ tienes ~, edades|)i(|, ~ anyos!~
.~

Kit of Kit of Power dobles = << <<10,5>>, <<3,6>>, <<4,2>> >>

controller Kit of Power kpd <- dobles:
    controller Power d <- dobles:
      drop ~Dobles: ~, d
    .~
.~
```

---
---
# **Entorno Stack.**

Primero clonar el repositorio y entrar en la carpeta.

```sh
$> git clone https://github.com/fmarquez199/CHask-.git
$> cd CHask-
```
## **Instalar stack.**

### ubuntu 19.04
```
$> sudo apt-get install haskell-stack
```

### Manjaro
```
$> sudo pacman -Sy install haskell-stack
```

Luego
```
$> stack upgrade
```

### Otros sistemas
https://docs.haskellstack.org/en/latest/install_and_upgrade/

## **Compilar el compilador playit.**
```sh
stack build
```

### Ejecutar el compilador

```sh
stack exec playit-exe test/casos/<resto de ruta al archivo>.game 
```

### Ejecutar las pruebas

```sh
stack test
```

Esto corre todas las pruebas en **test/casos**.

## **Ejecutar GHCI en stack.**

```sh
stack ghci
```

---
---
# **Extras.**

La presente sección contiene algunas funcionalidades de **Playit** que
**tentativamente** pueden ser desarrolladas con el lenguaje.

+ Intérprete.
+ Hilos.
+ Caracteres UTF-8.
+ Excepciones.
+ Iteradores.
