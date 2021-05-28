# **Index**

- [**Flow control structures**](#flow-control-structures)
  - [**Selection**](#selection)
  - [**Loops**](#loops)
    - [**Determined**](#determined)
    - [**Indetermined**](#indetermined)
    - [**Loop break**](#loop-break)
    - [**Skip current iteration**](#skip-current-iteration)
    - [**Alter current execution**](#alter-current-execution-(*Under-development*)) (*Under development*)

---
---
# **Flow control structures**

## **Selection**

> **`Button:`**
> 
> **`|`** *Boolean expression1* **`}`**
> 
>   *Instructions list*
>
> **`|`** *Boolean expression2* **`}`**
> 
>   *Instructions list*
> 
> ...
> 
> **`|`** **`notPressed`** **`}`**
> 
>   *Instructions list*
> 
> **`.~`**

Only one condition its required.

If one condition **`Lose`**, the next one its evaluated until one **`Win`**.

Only when all others conditions **`Lose`**, execute **`notPressed`** (optional).

---
## **Loops**

### **Determined**

> Count-controlled:
> 
>> **`farm`** *iteration variable* **`equip`** *initial value* **`until`** *final value* [**`lock`** *condition*] **`:`**
>>
>>   Instructions list
>>
>> **`.~`**
>
> The *iteration variable* starts with the *initial value* until the *final value*, restricted by the *condition*.
>
> The **`lock`** *condition* its optional.

> Collection-controlled:
> 
>> **`farm`** *iteration variable* **`in`** *array or list* **`:`**
>>
>>   Instructions list
>>
>> **`.~`**
> 
> The *iteration variable* will have an element of the *array or list*, and the 
iteration will finish when the *array or list* is empty.

### **Indetermined**

> Condition-controlled: While-Do
> 
>> **`dungeon`** **`:`**
>>
>>   Instructions list
>>
>> **`cleared`** *Boolean expression*
>>
>> **`.~`**

The *Instructions list* will keep running until *Boolean expression* **`Lose`**.

### **Loop break**

> Reserved word: **`gameOver`**

Interrups the execution of the loops **`farm`** and **`dungeon`**.

Only interrups the closest loop.

### **Skip current iteration**

> Reserved word: **`keepPlaying`**

Skips the following instructions in the the actual iteration of the loops **`farm`** 
and **`dungeon`**, advancing to the next.

---
### **Alter current execution** (*Under development*)

Dying(Power): despues de Power iteraciones te lanza un boss ultimate y tienes que ingerir hp/mp potions,
al inicio de la ejecucion del programa tienes un nro predeterminado de las potions, las puedes ganar haciendo `algo`,
si ya no tienes potions mueres y termina el programa xD (poder retry?? desde ese punto o desde antes de la interacion)
esto solo si estas peleando contra monsters/boss dentro de una iteracion??
hacer checkpoint antes iteracion para que puedas revivir

[**Next page ->**](07-Subroutines.md/#index)

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
- [**Instructions**](05-Instructions.md/#instructions)
  - [**Assignment**](05-Instructions.md/#assignment)
    - [**Special assignments**](05-Instructions.md/#special-assignments)
  - [**Conditional**](05-Instructions.md/#conditional)
  - [**Loops**](05-Instructions.md/#loops)
  - [**Input/Output**](05-Instructions.md/#inputoutput)
  - [**Map**](05-Instructions.md/#map-(*Under-development*)) (*Under development*)
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
