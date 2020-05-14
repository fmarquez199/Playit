.text
sw $a0, 0($sp)
sw $a1, -4($sp)
sw $a2, -8($sp)
sw $a3, -12($sp)
sw $t0, -16($sp)
sw $t1, -20($sp)
sw $t2, -24($sp)
sw $t3, -28($sp)
sw $t4, -32($sp)
sw $t5, -36($sp)
sw $t6, -40($sp)
sw $t7, -44($sp)
sw $t8, -48($sp)
sw $t9, -52($sp)
addi $sp, $sp, -56
Call _main
sw $ra, 0($sp)
sw $s0, -4($sp)
sw $s1, -8($sp)
sw $s2, -12($sp)
sw $s3, -16($sp)
sw $s4, -20($sp)
sw $s5, -24($sp)
sw $s6, -28($sp)
sw $s7, -32($sp)
sw $fp, -36($sp)
addi $sp, $sp, -40
_main: li $9, 0
li $8, 
1: li $8, Dime lo que deseas hacer: calcular factorial (f) o salir (x): 
addi $a0, $8, 0
li $v0, 4
syscall
la $a0, string
addi $a1, $zero, 80
li $v0, 8
syscall
li $10, x
