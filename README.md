# Nombre del Lenguaje.

Hay que decidir un nombre para el lenguaje. Recomiendo dejar para el final.

## Estructura de un programa.

Esta parte hay que definirla.

## Estructuras de control de flujo.

### Selección.

Propongo una estructura similar al `if/elif/else` de Pyhton.

### Repetición.

#### Determinada.

Propongo una estructura similar a un ciclo `for`, pero cuyo uso se limite al de un `foreach`. Hay que revisar la sintaxis del `for`.

#### Indeterminada.

Propongo una estructura similar a un ciclo `while`.

### Subrutinas.

#### Funciones y procedimientos.

Propongo que sean esencialmente lo mismo, salvo porque las funciones retornan los tipos escalares y los procedimientos retornen el valor unit (`()` en Haskell).

##### Pasaje de parámetros.

Como se admite pasaje por valor y por referencia yo propongo distinguir ambos precediendo con dólar (`$`) cuando el pase del parámetro sea por referencia y nada que preceda cuando sea por valor.

#### Recursión.

El lenguaje admite la invocación recursiva de funciones en cualquier momento.

## Tipos de datos

### Escalares.

#### Carácteres.

Son datos de 8 bit (1 B) en memoria y se caracterizan por ser un carácter ASCII Extendido entre comillas simples.

Se distinguen los carácteres de escape por ser aquellos precedidos por un backslash (`\`): `n` (salto de espacio), `t` (tabulación de 4 espacios), `\` (backslash), `"` (comillas dobles), `'` (comilla simple).

#### Booleanos.

Son datos de 8 bit (1 B) en memoria y se caracterizan por ser `T` (*True*) o `F` (*False*) sin estar encerrados entre comillas. `F` corresponde en memoria a los ocho bit en 0 y `T` a cualquier otra combinación.

#### Enteros.

Son datos de 32 bit (4 B) en memoria que pueden ser cualquier cadena no vacía de números de la base decimal. Su representación es **complemento a 2**, por lo tanto los enteros están acotados en el rango: `[-2 147 483 648..2 147 483 647]`.

#### Números de punto flotante.

Son datos de 64 bit (8 B) en memoria representados según el estándar **IEEE 754 de precisión doble**, por tanto están en el rango: `(-∞,..∞)`

### Compuestos.

#### Arreglos.

Son estructuras de datos homogéneas de cualquier tipo, es decir, se admite arreglos multidimensionales de algún tipo escalar. Su representación en un programa es `[elemento0, elemento1, ..., elementoN]`, siendo todos los elementos del mismo tipo. Son estructuras estáticas y su tamaño debe ser definido en su declaración.

#### Strings.

Son arreglos de carácteres. Corresponden a un grupo particular de arreglos por su representación, estos admiten la representación de arreglo y de cadena de carácteres como en los lenguajes tradicionales, encerrados entre comillas dobles (`"`).

#### Registros.

Propongo que sean como los `structs` de C.

#### Registros variantes.

Propongo que sean como los `data` de Haskell.

#### Apuntadores.

Propongo que sean enteros con una dirección de memoria Heap.

