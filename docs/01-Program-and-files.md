# **Index**

- [**Program's Structure**](01-Program-and-files.md/#programs-structure)
- [**Source code file extension**](01-Program-and-files.md/#source-code-file-extension)

---
---

# **Program's Structure**

```Playit
Statements
play world %>program's name<%:
  Instructions list
.~
```

A program whose instruction list is empty is not allowed.

**Example:**

```Playit
play world %>HolaMundo<%:
  drop ~Hello World!~
.~
```

---
---
# **Source code file extension**

A source code file in Playit must have the extension `.game` to be recognized by the compiler.

[**Next page ->**](02-Id's-and-types.md/#index)

---
---
# **Table of content** <!-- omit in toc -->

- [**Playit Programming language**](../README.md/#playit-programming-language)
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
