.data
_x:
.space 12
.text
main:
	sw $ra, 0($sp)
	addiu $sp, $sp, -4
	sw $fp, 0($sp)
	addiu $sp, $sp, -4
	addu $fp, $sp, 8
	subu $sp, $sp, 40
	la $t0, L0
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	# Pushing addr of local variable pingas
	addiu $t0, $fp, -8
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	addiu $t0, $t0, 12
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 4
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	# Pushing addr of local variable pingas
	addiu $t0, $fp, -8
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	addiu $t0, $t0, 0
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	addiu $t0, $t0, 8
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Pushing addr of local variable pingas
	addiu $t0, $fp, -8
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	addiu $t0, $t0, 0
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	lw $t0, 9($t0)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	# Pushing addr of local variable k
	addiu $t0, $fp, -32
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Pushing value of local variable p
	lw $t0, -24($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	lw $t0, 4($t0)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	addi $t0, $zero, 1
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	beq $t0, $zero, L1
	addi $sp, $sp, -32
	# Pushing addr of local variable x
	addiu $t0, $fp, -40
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 4
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	# Pushing addr of local variable z
	addiu $t0, $fp, -44
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Pushing value of local variable x
	lw $t0, -40($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	addi $sp, $sp, 32
	j L2
L1:
	addi $sp, $sp, -32
	# Pushing addr of local variable x
	addiu $t0, $fp, -40
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 4
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	# Pushing addr of local variable y
	addiu $t0, $fp, -44
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Pushing value of local variable x
	lw $t0, -40($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	# Pushing addr of local variable y
	addiu $t0, $fp, -44
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Pushing value of local variable y
	lw $t0, -44($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	addi $sp, $sp, 32
L2:
	# Pushing addr of local variable late
	addiu $t0, $fp, -36
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 5
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
main_exit:
	lw $ra, 0($fp)
	lw $t0, -4($fp)
	move $sp, $fp
	move $fp, $t0
	addi $v0, $zero, 10
	syscall
.text
_funny:
	sw $ra, 0($sp)
	addiu $sp, $sp, -4
	sw $fp, 0($sp)
	addiu $sp, $sp, -4
	addu $fp, $sp, 8
	subu $sp, $sp, 12
	# Pushing addr of local variable x
	addiu $t0, $fp, -8
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 1
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
L3:
	# Pushing value of local variable x
	lw $t0, -8($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	beq $t0, $zero, L4
	addi $sp, $sp, -16
	# Pushing addr of local variable y
	addiu $t0, $fp, -12
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 1
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
L5:
	# Pushing value of local variable y
	lw $t0, -12($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	beq $t0, $zero, L6
	addi $sp, $sp, -16
	# Pushing addr of local variable x
	addiu $t0, $fp, -16
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 1
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
L7:
	# Pushing value of local variable x
	lw $t0, -16($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	beq $t0, $zero, L8
	addi $sp, $sp, -16
	# Pushing addr of local variable y
	addiu $t0, $fp, -20
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 1
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
L9:
	# Pushing value of local variable y
	lw $t0, -20($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	beq $t0, $zero, L10
	addi $sp, $sp, -16
	# Pushing addr of local variable x
	addiu $t0, $fp, -24
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 1
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
L11:
	# Pushing value of local variable x
	lw $t0, -24($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	beq $t0, $zero, L12
	addi $sp, $sp, -16
	# Pushing addr of local variable y
	addiu $t0, $fp, -28
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 1
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
L13:
	# Pushing value of local variable y
	lw $t0, -28($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	beq $t0, $zero, L14
	addi $sp, $sp, -16
	# Pushing addr of local variable x
	addiu $t0, $fp, -32
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 1
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
L15:
	# Pushing value of local variable x
	lw $t0, -32($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	beq $t0, $zero, L16
	addi $sp, $sp, -16
	# Pushing addr of local variable y
	addiu $t0, $fp, -36
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 1
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
L17:
	# Pushing value of local variable y
	lw $t0, -36($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	beq $t0, $zero, L18
	addi $sp, $sp, 0
	# Pushing addr of local variable y
	addiu $t0, $fp, -36
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 0
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	addi $sp, $sp, 0
	j L17
L18:
	# Pushing addr of local variable x
	addiu $t0, $fp, -32
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 0
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	addi $sp, $sp, 16
	j L15
L16:
	# Pushing addr of local variable y
	addiu $t0, $fp, -28
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 0
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	addi $sp, $sp, 16
	j L13
L14:
	# Pushing addr of local variable x
	addiu $t0, $fp, -24
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 0
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	addi $sp, $sp, 16
	j L11
L12:
	# Pushing addr of local variable y
	addiu $t0, $fp, -20
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 0
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	addi $sp, $sp, 16
	j L9
L10:
	# Pushing addr of local variable x
	addiu $t0, $fp, -16
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 0
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	addi $sp, $sp, 16
	j L7
L8:
	# Pushing addr of local variable y
	addiu $t0, $fp, -12
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 0
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	addi $sp, $sp, 16
	j L5
L6:
	# Pushing addr of local variable x
	addiu $t0, $fp, -8
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 0
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Popping rvalue for assignment
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	# Popping lvalue for assignment
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	sw $t1, 0($t0)
	# Putting value back on stack (for chains)
	sw $t1, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
	addi $sp, $sp, 16
	j L3
L4:
_funny_exit:
	lw $ra, 0($fp)
	lw $t0, -4($fp)
	move $sp, $fp
	move $fp, $t0
	jr $ra
.text
_Wingas:
	sw $ra, 0($sp)
	addiu $sp, $sp, -4
	sw $fp, 0($sp)
	addiu $sp, $sp, -4
	addu $fp, $sp, 8
	subu $sp, $sp, 8
	la $31, L19
	la $t0, _Wingas
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	jr $t0
L19:
	sw $v0, 0($sp)
	addiu $sp, $sp, -4
	lw $zero, 4($sp)
	addiu $sp, $sp, 4
_Wingas_exit:
	lw $ra, 0($fp)
	lw $t0, -4($fp)
	move $sp, $fp
	move $fp, $t0
	jr $ra
.text
_add1:
	sw $ra, 0($sp)
	addiu $sp, $sp, -4
	sw $fp, 0($sp)
	addiu $sp, $sp, -4
	addu $fp, $sp, 8
	subu $sp, $sp, 8
_add1_exit:
	lw $ra, 0($fp)
	lw $t0, -4($fp)
	move $sp, $fp
	move $fp, $t0
	jr $ra
L0:
.asciiz "John"
