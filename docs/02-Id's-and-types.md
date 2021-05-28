# **Index**

- [**Variables identifiers (id's)**](#variables-identifiers-(id's))
  - [**Declaration and initialization of variables**](#declaration-and-initialization-of-variables)
- [**Data types. Scalars**](#data-types-scalars)
  - [**Characters**](#characters)
  - [**Booleans**](#booleans)
  - [**Integers**](#integers)
  - [**Floating point numbers**](#floating-point-numbers)
- [**Data types. Composite**](#data-types-composite)
  - [**Arrays**](#arrays)
  - [**Strings**](#strings)
  - [**Lists**](#lists)
  - [**Records**](#records)
  - [**Variant Records. Unions**](#variant-records-unions)
  - [**Pointers**](#pointers)

---
---

# **Variables identifiers (id's)**

A valid identifier fo a variable in a **Playit** program, meets the following conditions:

1. Can have characters `A` up to `Z` in upper or lowercase.
2. Can have numbers from `0` to `9`, special characters `_` and `'`, but not start with them.
3. Doesn't have spaces, `ñ` or accented characters.
4. Isn't any reserved word of **Playit**.

> **valid id's:** `test1`, `yes_we_can`, `maybe'not`.
>
> **invalid id's:** `1test1`, `yes_we_cañ`, `maybe not`.

## **Declaration and initialization of variables**

Variable declaration without initialization is allowed. 

If the variable is of scalar or pointer type, it will be initialized with the 
default values described for each scalar type.

---
---
# **Data types. Scalars**

## **Characters**

> Reserved word: **`Rune`**.
>
> Representation: **`*ASCII character*`**.
>
> Default value: `*\0*`.

They are **8 bit (1 B)** data in memory.

Escape sequences are distinguished by being those preceded by a backslash (`\`):

* `*\0*` (null character).
* `*\n*` (newline).
* `*\t*` (4-space tab).
* `*\\*` (backslash).
* `*\~*` (prime).
* `*\**` (asterisk).

---
## **Booleans**

> Reserved word: **`Battle`**.
>
> Default value: **`Lose`**.

They are **8 bit (1 B)** data in memory wich values are **`Win`** and **`Lose`**. 

Note: **`Lose`** corresponds in memory to the eight bits at 0 and **`Win`** to any other combination.

---
## **Integers**

> Reserved word: **`Power`**.
> 
> Default value: `0`.

They are **32 bit (4 B)** data in memory that can be any non-empty string of decimal base numbers. 

They are represented in **2's complement**, therefore the integers are bounded in the range:

> `[-2 147 483 648, 2 147 483 647]`

Strings of numbers in the decimal base are considered integers preceded by a nonzero quantity of `0`:

```Playit
01
003
0000000000005
```

---
## **Floating point numbers**

> Reserved word: **`Skill`**.
>
> Representation: **`Power'Power`**
>
> Default value: `0'0`.

They are **64 bit (8 B)** data in memory represented according to the standard 
*IEEE 754 double precision*.

---
---
# **Data types. Composite**

## **Arrays**

> Declaration: *type* **`|}`** *size* **`{|`** *id*, where *type* can be another array and *size* any other defined variable of type **`Power`**.
>
> Representation: **`|}`** *element0, element1, ..., elementN* **`{|`**, all elements are the same type.
>
> Default value: **`|}`** *default value1 of type, ..., default valueN of type* **`{|`**
>
> Indexing: *array id* **`|)`** *index* **`(|`**, *index* must be a **`Power`**.

It's a homogeneous data structure of any scalar type that is located consecutively in memory. 

They are static structures and their size must be defined in their declaration. 

Multidimensional arays are allowed.

**Example:**

```Playit
type1|} 3 {| array1
type2|} 7 {||} variable1 {| array2
typeN|} variable2 {||} 2 {|...|} 10 {| array3
```

---
## **Strings**

> Reserved word: **`Rune`**, **`Runes`**.
>
> Declaration: 
>
>> **`Runes`** *id*
>
>> **`Rune|}`** *size* **`{|`** *id*
>
> Default value: **`~~`**. Empty string. Note this is when defined with `Runes`.

Characters arrangements that can be represented as an array or string enclosed in primes (`~~`):

**Example:**

```Playit
Runes runes1 equip ~You have a new mission~
Rune|} 5 {| runes2
```

---
## **Lists**

> Reserved word: **`Kit of`**
>
> Declaration: **`Kit of`** *type id*, where *type* can be another list.
>
> Representation: **`<<`** *element0, element1, ..., elementN* **`>>`**, all elements are the same type.
>
> Default value: **`<<>>`**. Empty list.
>
> Indexing: **`|>`** *index* **`<|`**, *index* must be a **`Power`**.

It is a heterogeneous data structure of any scalar type that is located, not 
necessarily consecutively, in memory. 

Multidimensional lists are supported.

---
## **Records**

> Reserved word: **`Inventory`**
> 
> Declaration:
>
> ```Playit
> Inventory id:
>   type field1 equip value
>   ...
>   type fieldN
> .~
> ```
>
> Initialization:
>
>> *Record id* **`equip`** **`{`** *value of field1, ..., value of fieldN* **`}`**. 
Note this overwrites the values that you could have assigned when declaring the record.
>
>> *Record id* **`spawn`** *field id* **`equip`** *value*

Each field of the record corresponds to a scalar or compound type previously 
defined in the program and are defined in **Playit**. Each instance of a record 
corresponds to a data type that can also be part of another record.

Its size in memory corresponds to the sum of the individual sizes of each field it has. 

You can also set an initial value or let it by default. Note that if you want to
initialize the others fields independant of the already initialized, you have to
do it separately by each field with `spawn`.

**Example:**

```Playit
Inventory Register:
  Power up
  Rune tip
.~

Register r equip {0, *f*}

r spawn up equip 5
r spawn tip equip *c*
```

---
## **Variant Records. Unions**

> Reserved word: **`Items`**
>
> Declaration:
>
> ```Playit
> Items id:
>   type field1
>   ...
>   type fieldN
> .~
> ```
> Initialization:
>
>> *id* **`spawn`** *field* **`equip`** *value*. It have to one and only one field to be initialized.

Each field of the record corresponds to a scalar or compound type previously 
defined in the program and are defined in **Playit**. Each instance of a variant 
record corresponds to a data type that can also be part of another record.

Its size in memory corresponds to the largest field. 

**Example:**

```Playit
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
sh spawn c spawn centerX equip 2'1
sh spawn c spawn centerY equip 5'0
sh spawn c spawn centerY equip 5'0
sh spawn c spawn radius  equip 15'0
```

---
## **Pointers**

> Declaration: 
>
>> *type* **`puff`** *id*
>
>> *type* **`puff`** *id* **`equip`** **`summon`** *type*
>
> Default value: **`DeathZone`**
>
> Free memory space:
>
>> **`free`** *id*
>
>> **`free|}{|`** *array id*
>
>> **`free<<>>`** *list id*

They are a special type of data that saves the element in dynamic memory (heap) 
in a referemcial way. In memory they are a processor's word.

By default they point to an empty memory space with **`DeathZone`**.

When you free the memory, **`DeathZone`** its assigned to the variable.

**Examples:**

```Playit
Power puff p equip summon Power
puff p equip 13
free p
```

```Playit
Power|}20{| puff p equip summon Power|}20{|
puff p|)1(| equip 15
free|}{| p
```

```Playit
Power puff p equip summon Power
puff p equip 15
free p

p equip summon Power
puff p equip 18
free p
```

[**Next page ->**](03-Expressions.md/#index)

---
---
# **Table of content** <!-- omit in toc -->

- [**Playit Programming language**](../README.md/#playit-programming-language)
- [**Program's Structure**](01-Program-and-files.md/#programs-structure)
- [**Source code file extension**](01-Program-and-files.md/#source-code-file-extension)
- [**Expressions**](03-Expressions.md/#expressions)
  - [**Aritmetics**](03-Expressions.md/#aritmetics)
  - [**Boolean**](03-Expressions.md/#boolean)
  - [**Characters**](03-Expressions.md/#characters)
  - [**Arrays and Lists**](03-Expressions.md/#arrays-and-lists)
  - [**Records**](03-Expressions.md/#records)
  - [**Pointers**](03-Expressions.md/#pointers)
- [**Expressions evaluation**](03-Expressions.md/#expressions-evaluation)
  - [**Operators precedence**](03-Expressions.md/#operators-precedence)
  - [**Operators associativity**](03-Expressions.md/#operators-associativity)
  - [**Evaluation order**](03-Expressions.md/#evaluation-order)
- [**Block and Scope**](04-Block-Scope-Comments.md/#block-and-scope)
- [**Comments and White spaces**](04-Block-Scope-Comments.md/#comments-and-white-spaces)
- [**Instructions**](05-Instructions.md/#instructions)
  - [**Assignment**](05-Instructions.md/#assignment)
    - [**Special assignments**](05-Instructions.md/#special-assignments)
  - [**Conditional**](05-Instructions.md/#conditional)
  - [**Loops**](05-Instructions.md/#loops)
  - [**Input/Output**](05-Instructions.md/#inputoutput)
  - [**Map**](05-Instructions.md/#map-under-development) (*Under development*)
- [**Flow control structures**](06-Flow-control.md/#flow-control-structures)
  - [**Selection**](06-Flow-control.md/#selection)
  - [**Loops**](06-Flow-control.md/#loops)
    - [**Determined**](06-Flow-control.md/#determined)
    - [**Indetermined**](06-Flow-control.md/#indetermined)
    - [**Loop break**](06-Flow-control.md/#loop-break)
    - [**Skip current iteration**](06-Flow-control.md/#skip-current-iteration)
    - [**Alter current execution**](06-Flow-control.md/#alter-current-execution-under-development) (*Under development*)
- [**Subroutines**](07-Subroutines.md/#subroutines)
  - [**Functions**](07-Subroutines.md/#functions)
  - [**Procedures**](07-Subroutines.md/#procedures)
  - [**Subroutines call**](07-Subroutines.md/#subroutines-call)
  - [**Parameters by value and reference**](07-Subroutines.md/#parameters-by-value-and-reference)
  - [**Recursion**](07-Subroutines.md/#recursion)
- [**Examples**](../README.md/#examples)
- [**Compile and run**](08-Compile-and-run.md/#compile-and-run)
  - [**Run unit tests**](08-Compile-and-run.md/#run-unit-tests)
  - [**Get stack here**](08-Compile-and-run.md/#get-stack-here)
- [**Extras**](../README.md/#extras)
