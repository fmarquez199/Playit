# **Index**

- [**Instructions**](#instructions)
  - [**Assignment**](#assignment)
    - [**Special assignments**](#special-assignments)
  - [**Conditional**](#conditional)
  - [**Loops**](#loops)
  - [**Input/Output**](#inputoutput)
  - [**Map**](#map-under-development) (*Under development*)

---
---

# **Instructions**

## **Assignment**

> Operator: **`equip`**

It can be done at the time of declaring a variable or after its declaration. 

Two variables can be assigned to each other, if and only if they are of the same type. 

If the variables are of the same type, they can be declared on the same line as follow:

```Playit
type1 id1, ..., idN
type2 idA equip expression, idB equip expression ...
```

### **Special assignments**

They are assignments that work exclusively on variables of type *Power* and 
work as suffix/postfix operators:

> **`level up`**: Increment. Increase the value of the variable by 1.
>
> **`use item`**: Decrement. Decrease the value of the variable by 1.

---
## **Conditional**

See [**Flow control structures. Selection**](06-Flow-control.md/#Selection) section.

---
## **Loops**

See [**Flow control structures. Loops**](06-Flow-control.md/#Loops) section.

---
## **Input/Output**

> Input: variable **`equip joystick`** [*prompt*]
>
>> Takes **`Runes`** as an optional *prompt* for the user and reads **`Runes`** from the user.
>
> Output: **`drop`** *variable*[, *constant*, ...]
>
>> Takes **`Runes`**, **`Rune`**, **`Power`** or **`Skill`**, and shows them in the standard output.
>
> Casting:
>
>>`portalRuneToRunes`: Converts a `Rune` to `Runes`.
>
>>`portalPowerToRunes`: Converts a `Power` to `Runes`.
>
>>`portalSkillToRunes`: Converts a `Skill` to `Runes`.
>
>>`portalRunesToPower`: Converts `Runes` to `Power`.
>
>>`portalRunesToRune` : Converts `Runes` to `Rune`.
>
>>`portalRunesToSkill`: Converts `Runes` to `Skill`.

Its execution consists of an interruption to read from the standard input and 
returns **`Runes`** as default, without the `*\n*` character.

If the variable its a **`Rune`**, **`Power`** or **`Skill`**, **Playit** casts 
the readed **`Runes`** to the corresponding type. ***(Need to be implemented)***

These can be used without the **`kill`** statement described in 
[**Flow control structures. Subroutines**](07-Subroutines.md/#Subroutines) section.

---
## **Map** (*Under development*)

muestra todas las declaraciones hechas (vars, monsters, bosses, inventory, items ...)

[**Next page ->**](06-Flow-control.md/#index)

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
