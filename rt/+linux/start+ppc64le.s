	.text
	.globl _start
	.type _start,@function
_start:
	addis %r2, %r12, .TOC.-_start@ha
	addi %r2, %r2, .TOC.-_start@l

	mr %r3, %r1

	clrrdi %r1, %r1, 4

	b rt.start_ha

	.size _start, .-_start
