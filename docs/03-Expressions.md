# **Index**

- [**Expressions**](#expressions)
  - [**Aritmetics**](#aritmetics)
  - [**Boolean**](#boolean)
  - [**Characters**](#characters)
  - [**Arrays and Lists**](#arrays-and-lists)
  - [**Records**](#records)
  - [**Pointers**](#pointers)
- [**Expressions evaluation**](#expressions-evaluation)
  - [**Operators precedence**](#operators-precedence)
  - [**Operators associativity**](#operators-associativity)
  - [**Evaluation order**](#evaluation-order)

---
---

# **Expressions**

## **Aritmetics**

They return numeric values (Powers or Skills) after being evaluated.

> Operators:
>
>> **`+` :** Addition.
>
>> **`-` :** Subtraction.
>
>> **`*` :** Multiply.
>
>> **`/` :** Skill division. Takes 2 Powers and returns a Skill.
>
>> **`//` :** Integer division. Takes 2 Powers and returns a Power that corresponds to the division's quotient.
>
>> **`%` :** Modulus. toma dos enteros y regresa un entero que corresponde al resto de la división.
>
>> **`-` :** Minus. Takes a Power or Skill and multiply it by `-1`.
>
>> **`#` :** Length. Takes an array or list and returns the number of elements it contains.

---
## **Boolean**

They return boolean values (**`Win`** or **`Lose`**) after being evaluated.

> Operators between booleans:
>
>> **`||` :** Disjunction.
>
>> **`&&` :** Conjunction.
>
>> **`!` :** Negation.

> Operators between any scalar type:
>
>> **`==` :** Equality. For *pointers* it compares the pointed elements.
>
>> **`!=` :** Inequality. For *pointers* it compares the pointed elements.
>
>> **`>` :** Greater than. For *characters* the normal lexical order is followed.
>
>> **`>=` :** Greater or equal than. For *characters* the normal lexical order is followed
>
>> **`<` :** Less than. For *characters* the normal lexical order is followed
>
>> **`<=` :** Less or equal than. For *characters* the normal lexical order is followed

> Ternary conditional operator:
>
>> *Boolean expression* **`quest`** ***`Win`** case function* ***`loot`** **`Lose`** case function*
>>
>> Evaluates and returns the result of the ***`Win`** case function* or ***`Lose`** case function*,
depending if the boolean expression evaluates **`Win`** or **`Lose`**.

---
## **Characters**

They return a character after being evaluated.

> Monadic prefix operators:
>
>> **`buff`**: Uppercase. Converts the given alphabetic character to its uppercase representation.
>
>> **`debuff`**: Lowercase. Converts the given alphabetic character to its lowercase representation.

---
## **Arrays and Lists**

They return an array or list after being evaluated.

> Operators:
>
>> **`|)(|` :** Array indexing. Index range `[0..#array - 1]`.
>
>> **`|><|` :** List indexing. Index range `[0..#list - 1]`.
>
>> **`::` :** List concatenation. Takes 2 lists and appends the second to the first.
>
>> **`:` :** List insertion (annex) Prepend a head element to a list.

---
## **Records**

> Access operator: **`spawn`**

It takes a record or union and an id, and if it matches a field, returns the value in it.

To know more go to [Records](#02-Id's-and-types.md/#records) and 
[Unions](#02-Id's-and-types.md/#variant-records-unions) data types section.

---
## **Pointers**

> Dereference operator: **`puff`**

Given a variable of the pointer type, it returns the value that was pointed by the given variable.

In the case that the variable is not dereferenced, its address is the one being used.

To know more go to [Pointers](#02-Id's%20and%20types.md/#pointers) data type section.

---
---
# **Expressions evaluation**

## **Operators precedence**

Precedence in descending order:

|  Operator  |   Firts level   |       Second level        |    Third level     |     Fourth level      |
| :--------- | :-------------: | :-----------------------: | :----------------: | :-------------------: |
| Aritmetic  | Minus, Length   | Integer Division, Modulus | Multiply, Division | Addition, Subtraction |
| Boolean    | Negation        | Conjunction, Disjunction  | Ternary            | Comparators           |
| Character  | Uppercase       | Lowecase                  |                    |                       |
| Arrays     | Indexing        |                           |                    |                       |
| Lists      | Indexing, Annex | Concatenation             |                    |                       |
| Record     | Access          |                           |                    |                       |
| Pointer    | Dereference     |                           |                    |                       |

---
## **Operators associativity**

|  Operator  |                   Izquierda                   |               Derecha                |  No asocia  |
| :--------- | :-------------------------------------------: | :----------------------------------: | :---------: |
| Aritmetic  | Multiply, Division, Integer Division, Modulus | Addition, Subtraction, Minus, Length |             |
| Boolean    | Conjunción, Disyunción, Ternary               | Negation                             | Comparators |
| Character  |                                               | Uppercase, Lowercase                 |             |
| Arrays     | Indexing                                      |                                      |             |
| Lists      | Indexing, Annex, Concatenation                |                                      |             |
| Record     | Access                                        |                                      |             |
| Pointer    |                                               | Dereference                          |             |

---
## **Evaluation order**

Expressions are evaluated from left to right.

[**Next page ->**](04-Block-Scope-Comments.md/#index)

---
---
# **Table of content** <!-- omit in toc -->

- [**Playit Programming language**](../README.md/#playit-programming-language)
- [**Program's Structure**](01-Program-and-files.md/#programs-structure)
- [**Source code file extension**](01-Program-and-files.md/#source-code-file-extension)
- [**Variables identifiers (id's)**](02-Id's-and-types.md/#variables-identifiers-(id's))
  - [**Declaration and initialization of variables**](02-Id's-and-types.md/#declaration-and-initialization-of-variables)
- [**Data types. Scalars**](02-Id's-and-types.md/#data-types-scalars)
  - [**Characters**](02-Id's-and-types.md/#characters)
  - [**Booleans**](02-Id's-and-types.md/#booleans)
  - [**Integers**](02-Id's-and-types.md/#integers)
  - [**Floating point numbers**](02-Id's-and-types.md/#floating-point-numbers)
- [**Data types. Composite**](02-Id's-and-types.md/#data-types-composite)
  - [**Arrays**](02-Id's-and-types.md/#arrays)
  - [**Strings**](02-Id's-and-types.md/#strings)
  - [**Lists**](02-Id's-and-types.md/#lists)
  - [**Records**](02-Id's-and-types.md/#records)
  - [**Variant Records. Unions**](02-Id's-and-types.md/#variant-records-unions)
  - [**Pointers**](02-Id's-and-types.md/#pointers)
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
