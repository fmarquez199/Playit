[![N|Solid](http://www.usb.ve/conocer/corporativa/archivos/logos/logotipo/logotip.png)](http://www.usb.ve)

# **Table of content** <!-- omit in toc -->

- [**Playit Programming language**](#playit-programming-language)
- [**Program's Structure**](docs/01-Program-and-files.md/#programs-structure)
- [**Source code file extension**](docs/01-Program-and-files.md/#source-code-file-extension)
- [**Variables identifiers (id's)**](docs/02-Id's-and-types.md/#variables-identifiers-(id's))
  - [**Declaration and initialization of variables**](docs/02-Id's-and-types.md/#declaration-and-initialization-of-variables)
- [**Data types. Scalars**](docs/02-Id's-and-types.md/#data-types-scalars)
  - [**Characters**](docs/02-Id's-and-types.md/#characters)
  - [**Booleans**](docs/02-Id's-and-types.md/#booleans)
  - [**Integers**](docs/02-Id's-and-types.md/#integers)
  - [**Floating point numbers**](docs/02-Id's-and-types.md/#floating-point-numbers)
- [**Data types. Composite**](docs/02-Id's-and-types.md/#data-types-composite)
  - [**Arrays**](docs/02-Id's-and-types.md/#arrays)
  - [**Strings**](docs/02-Id's-and-types.md/#strings)
  - [**Lists**](docs/02-Id's-and-types.md/#lists)
  - [**Records**](docs/02-Id's-and-types.md/#records)
  - [**Variant Records. Unions**](docs/02-Id's-and-types.md/#variant-records-unions)
  - [**Pointers**](docs/02-Id's-and-types.md/#pointers)
- [**Expressions**](docs/03-Expressions.md/#expressions)
  - [**Aritmetics**](docs/03-Expressions.md/#aritmetics)
  - [**Boolean**](docs/03-Expressions.md/#boolean)
  - [**Characters**](docs/03-Expressions.md/#characters)
  - [**Arrays and Lists**](docs/03-Expressions.md/#arrays-and-lists)
  - [**Records**](docs/03-Expressions.md/#records)
  - [**Pointers**](docs/03-Expressions.md/#pointers)
- [**Expressions evaluation**](docs/03-Expressions.md/#expressions-evaluation)
  - [**Operators precedence**](docs/03-Expressions.md/#operators-precedence)
  - [**Operators associativity**](docs/03-Expressions.md/#operators-associativity)
  - [**Evaluation order**](docs/03-Expressions.md/#evaluation-order)
- [**Block and Scope**](docs/04-Block-Scope-Comments.md/#block-and-scope)
- [**Comments and White spaces**](docs/04-Block-Scope-Comments.md/#comments-and-white-spaces)
- [**Instructions**](docs/05-Instructions.md/#instructions)
  - [**Assignment**](docs/05-Instructions.md/#assignment)
    - [**Special assignments**](docs/05-Instructions.md/#special-assignments)
  - [**Conditional**](docs/05-Instructions.md/#conditional)
  - [**Loops**](docs/05-Instructions.md/#loops)
  - [**Input/Output**](docs/05-Instructions.md/#inputoutput)
  - [**Map**](docs/05-Instructions.md/#map-(*Under-development*)) (*Under development*)
- [**Flow control structures**](docs/06-Flow-control.md/#flow-control-structures)
  - [**Selection**](docs/06-Flow-control.md/#selection)
  - [**Loops**](docs/06-Flow-control.md/#loops)
    - [**Determined**](docs/06-Flow-control.md/#determined)
    - [**Indetermined**](docs/06-Flow-control.md/#indetermined)
    - [**Loop break**](docs/06-Flow-control.md/#loop-break)
    - [**Skip current iteration**](docs/06-Flow-control.md/#skip-current-iteration)
    - [**Alter current execution**](docs/06-Flow-control.md/#alter-current-execution-(*Under-development*)) (*Under development*)
- [**Subroutines**](docs/07-Subroutines.md/#subroutines)
  - [**Functions**](docs/07-Subroutines.md/#functions)
  - [**Procedures**](docs/07-Subroutines.md/#procedures)
  - [**Subroutines call**](docs/07-Subroutines.md/#subroutines-call)
  - [**Parameters by value and reference**](docs/07-Subroutines.md/#parameters-by-value-and-reference)
  - [**Recursion**](docs/07-Subroutines.md/#recursion)
- [**Examples**](#examples)
- [**Compile and run**](docs/08-Compile-and-run.md/#compile-and-run)
  - [**Run unit tests**](docs/08-Compile-and-run.md/#run-unit-tests)
  - [**Get stack here**](docs/08-Compile-and-run.md/#get-stack-here)
- [**Extras**](#extras)

[**Next page ->**](docs/01-Program-and-files.md/#index)

---
---
# **Playit Programming language**

**Desingned by:**

* Manuel Gonzalez 11-10390  (Frontend)
* Francisco Javier 12-11163 (Frontend, Backend)
* Natascha Gamboa 12-11250  (Frontend, Backend) ***Currently working on it.***

Playit, it's a general purpose **imperative language**, not oriented to objects, 
compiled and strongly typed, inspired by the world of videogames, taking concepts
and philosophies from them. It is designed and implemented by Computer Engineering 
students from the **Simón Bolívar University** throughout the subjects 
**Programming Languages I & II (CI-4721, CI-4722).**

**Playit has:**

* Primitive data types (Integers, Characteres, Floating point numbers, Booleans).
* Pointers to memory in Heap.
* Arrays, Records, Variant records.
* Read and write from standard IO.
* ¡LOVE! :kissing_closed_eyes:

---
---
# **Examples**

**Cube volume**

```Playit
play world %>CubeVolume<%:
  "' Calculates the volume of a cube '"
  Skill arista, vol
  
  arista equip joystick ~Introduzca arista: ~
  vol equip arista * arista * arista
  
  drop ~volume: ~, vol
.~
```

**Even or odd**

```Playit
play world %>EvenOdd<%:
  "' Says if a number is even or odd '"
  Power n
  n equip joystick ~input a Power n: ~

  Button:
  | n % 2 == 0 }
    drop ~its even~
  | notPressed }
    drop ~its odd~
  .~
.~
```

**Multiply tables**

```Playit
play world %>Tables<%:
  Rune other equip *s*
  Power i, n
  
  dungeon:
    n equip joystick ~input a Power n: ~
  
    drop ~Multiply table of ~, n, ~ es:~
  
    @ Start nesting
    i equip 1 
    dungeon:
      drop n, ~ * ~, i, ~ = ~, i * n
      i++
    cleared i <= 10
    .~
    @ End nesting
  
    other = joystick ~Do you eant to calculate another table? (y/n): ~
  cleared  other != *n*
  .~
.~
```

**Lopps with arrays and lists**

```Playit
Kit of Runes age equip <<~12~, ~23~, ~15~, ~40~, ~15~>>
Runes|}5{| names equip |}~Natascha~, ~Francisco~, ~Manuel~, ~Ricardo~, ~Haskell~{| 

farming Power i equip 0 until 4:
  drop ~Hola ~, names|)i(|, ~ tienes ~, age|>i<|, ~ años!~
.~

Kit of Kit of Power list2 equip << <<10,5>>, <<3,6>>, <<4,2>> >>

farming Kit of Power kpd in list2:
  farming Power d in kpd:
    drop ~Dobles: ~, d
  .~
.~
```

---
---
# **Extras**

La presente sección contiene algunas funcionalidades de **Playit** que
**tentativamente** pueden ser desarrolladas con el lenguaje.

+ Intérprete.
+ Hilos. cheat
+ Caracteres UTF-8.
+ Excepciones. AoE
+ Iteradores. Quest

Dungeon: record of monsters and only one final boss -> para pasar al siguiente nivel del juego

enter dungeon -> cleared -> mission/raid completed -> world level up / rank up

mission = level/stage + quests
raid
respawn
rank
skill tree
upgrade
