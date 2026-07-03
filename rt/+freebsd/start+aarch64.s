.text
.global _start
_start:
	mov x29, #0
	mov x30, #0
	mov x9, sp
	and sp, x9, #-16
	b rt.start_ha
