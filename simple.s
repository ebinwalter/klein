.data
_arr:
.space 20
.text
main:
	sw $ra, 0($sp)
	addiu $sp, $sp, -4
	sw $fp, 0($sp)
	addiu $sp, $sp, -4
	addu $fp, $sp, 8
	subu $sp, $sp, 16
	# Pushing addr of local variable x
	addiu $t0, $fp, -8
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Pushing addr of global variable arr
	la $t0, _arr
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 1
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	li $t2, 4
	mul $t1, $t1, $t2
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	add $t0, $t0, $t1
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
	addiu $t0, $fp, -12
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Pushing addr of global variable arr
	la $t0, _arr
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 2
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	li $t2, 4
	mul $t1, $t1, $t2
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	add $t0, $t0, $t1
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
	# Pushing addr of local variable x
	addiu $t0, $fp, -8
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	# Pushing addr of global variable arr
	la $t0, _arr
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 1
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	lw $t1, 4($sp)
	addiu $sp, $sp, 4
	li $t2, 4
	mul $t1, $t1, $t2
	lw $t0, 4($sp)
	addiu $sp, $sp, 4
	add $t0, $t0, $t1
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
	# Pushing value of local variable x
	lw $t0, -8($fp)
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
	# Pushing value of local variable y
	lw $t0, -12($fp)
	sw $t0, 0($sp)
	addiu $sp, $sp, -4
	addi $t0, $zero, 10
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
