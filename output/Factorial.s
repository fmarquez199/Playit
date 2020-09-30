.data
10act: .buffer 80

10exit: .buffer 80

1f: .buffer 4
13f: .buffer 4
14f: .buffer 4

10fac: .buffer 80

6factorial: .buffer 4


10is: .buffer 80


1msg: .buffer 80


10num: .buffer 4


10way: .buffer 80


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
Call main
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
main: 
li $9, 0
li $17, 0
li $16, 0
add $8, $8, $17
sw $8, 0($16)
1: 
li $14, 0
li $13, 62
add $8, $8, $14
sw $8, 0($13)
li $14, 1
li $13, D
add $8, $8, $14
sw $8, 0($13)
li $14, 2
li $13, i
add $8, $8, $14
sw $8, 0($13)
li $14, 3
li $13, m
add $8, $8, $14
sw $8, 0($13)
li $14, 4
li $13, e
add $8, $8, $14
sw $8, 0($13)
li $14, 5
li $13,  
add $8, $8, $14
sw $8, 0($13)
li $14, 6
li $13, l
add $8, $8, $14
sw $8, 0($13)
li $14, 7
li $13, o
add $8, $8, $14
sw $8, 0($13)
li $14, 8
li $13,  
add $8, $8, $14
sw $8, 0($13)
li $14, 9
li $13, q
add $8, $8, $14
sw $8, 0($13)
li $14, 10
li $13, u
add $8, $8, $14
sw $8, 0($13)
li $14, 11
li $13, e
add $8, $8, $14
sw $8, 0($13)
li $14, 12
li $13,  
add $8, $8, $14
sw $8, 0($13)
li $14, 13
li $13, d
add $8, $8, $14
sw $8, 0($13)
li $14, 14
li $13, e
add $8, $8, $14
sw $8, 0($13)
li $14, 15
li $13, s
add $8, $8, $14
sw $8, 0($13)
li $14, 16
li $13, e
add $8, $8, $14
sw $8, 0($13)
li $14, 17
li $13, a
add $8, $8, $14
sw $8, 0($13)
li $14, 18
li $13, s
add $8, $8, $14
sw $8, 0($13)
li $14, 19
li $13,  
add $8, $8, $14
sw $8, 0($13)
li $14, 20
li $13, h
add $8, $8, $14
sw $8, 0($13)
li $14, 21
li $13, a
add $8, $8, $14
sw $8, 0($13)
li $14, 22
li $13, c
add $8, $8, $14
sw $8, 0($13)
li $14, 23
li $13, e
add $8, $8, $14
sw $8, 0($13)
li $14, 24
li $13, r
add $8, $8, $14
sw $8, 0($13)
li $14, 25
li $13, :
add $8, $8, $14
sw $8, 0($13)
li $14, 26
li $13,  
add $8, $8, $14
sw $8, 0($13)
li $14, 27
li $13, c
add $8, $8, $14
sw $8, 0($13)
li $14, 28
li $13, a
add $8, $8, $14
sw $8, 0($13)
li $14, 29
li $13, l
add $8, $8, $14
sw $8, 0($13)
li $14, 30
li $13, c
add $8, $8, $14
sw $8, 0($13)
li $14, 31
li $13, u
add $8, $8, $14
sw $8, 0($13)
li $14, 32
li $13, l
add $8, $8, $14
sw $8, 0($13)
li $14, 33
li $13, a
add $8, $8, $14
sw $8, 0($13)
li $14, 34
li $13, r
add $8, $8, $14
sw $8, 0($13)
li $14, 35
li $13,  
add $8, $8, $14
sw $8, 0($13)
li $14, 36
li $13, f
add $8, $8, $14
sw $8, 0($13)
li $14, 37
li $13, a
add $8, $8, $14
sw $8, 0($13)
li $14, 38
li $13, c
add $8, $8, $14
sw $8, 0($13)
li $14, 39
li $13, t
add $8, $8, $14
sw $8, 0($13)
li $14, 40
li $13, o
add $8, $8, $14
sw $8, 0($13)
li $14, 41
li $13, r
add $8, $8, $14
sw $8, 0($13)
li $14, 42
li $13, i
add $8, $8, $14
sw $8, 0($13)
li $14, 43
li $13, a
add $8, $8, $14
sw $8, 0($13)
li $14, 44
li $13, l
add $8, $8, $14
sw $8, 0($13)
li $14, 45
li $13,  
add $8, $8, $14
sw $8, 0($13)
li $14, 46
li $13, (
add $8, $8, $14
sw $8, 0($13)
li $14, 47
li $13, f
add $8, $8, $14
sw $8, 0($13)
li $14, 48
li $13, )
add $8, $8, $14
sw $8, 0($13)
li $14, 49
li $13,  
add $8, $8, $14
sw $8, 0($13)
li $14, 50
li $13, o
add $8, $8, $14
sw $8, 0($13)
li $14, 51
li $13,  
add $8, $8, $14
sw $8, 0($13)
li $14, 52
li $13, s
add $8, $8, $14
sw $8, 0($13)
li $14, 53
li $13, a
add $8, $8, $14
sw $8, 0($13)
li $14, 54
li $13, l
add $8, $8, $14
sw $8, 0($13)
li $14, 55
li $13, i
add $8, $8, $14
sw $8, 0($13)
li $14, 56
li $13, r
add $8, $8, $14
sw $8, 0($13)
li $14, 57
li $13,  
add $8, $8, $14
sw $8, 0($13)
li $14, 58
li $13, (
add $8, $8, $14
sw $8, 0($13)
li $14, 59
li $13, x
add $8, $8, $14
sw $8, 0($13)
li $14, 60
li $13, )
add $8, $8, $14
sw $8, 0($13)
li $14, 61
li $13, :
add $8, $8, $14
sw $8, 0($13)
li $14, 62
li $13,  
add $8, $8, $14
sw $8, 0($13)
addi $a0, $8, 0
li $v0, 4
syscall
la $a0, string
addi $a1, $zero, 80
li $v0, 8
syscall
li $11, 0
li $10, 1
add $12, $12, $11
sw $12, 0($10)
li $11, 1
li $10, x
add $12, $12, $11
sw $12, 0($10)
