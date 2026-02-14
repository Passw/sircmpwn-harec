.section .text.rt.syscall0
.global rt.syscall0
rt.syscall0:
	mr %r0, %r3
	sc
	bns 1f
	neg %r3, %r3
1:
	blr

.section .text.rt.syscall1
.global rt.syscall1
rt.syscall1:
	mr %r0, %r3
	mr %r3, %r4
	sc
	bns 1f
	neg %r3, %r3
1:
	blr

.section .text.rt.syscall2
.global rt.syscall2
rt.syscall2:
	mr %r0, %r3
	mr %r3, %r4
	mr %r4, %r5
	sc
	bns 1f
	neg %r3, %r3
1:
	blr

.section .text.rt.syscall3
.global rt.syscall3
rt.syscall3:
	mr %r0, %r3
	mr %r3, %r4
	mr %r4, %r5
	mr %r5, %r6
	sc
	bns 1f
	neg %r3, %r3
1:
	blr

.section .text.rt.syscall4
.global rt.syscall4
rt.syscall4:
	mr %r0, %r3
	mr %r3, %r4
	mr %r4, %r5
	mr %r5, %r6
	mr %r6, %r7
	sc
	bns 1f
	neg %r3, %r3
1:
	blr

.section .text.rt.syscall5
.global rt.syscall5
rt.syscall5:
	mr %r0, %r3
	mr %r3, %r4
	mr %r4, %r5
	mr %r5, %r6
	mr %r6, %r7
	mr %r7, %r8
	sc
	bns 1f
	neg %r3, %r3
1:
	blr

.section .text.rt.syscall6
.global rt.syscall6
rt.syscall6:
	mr %r0, %r3
	mr %r3, %r4
	mr %r4, %r5
	mr %r5, %r6
	mr %r6, %r7
	mr %r7, %r8
	mr %r8, %r9
	sc
	bns 1f
	neg %r3, %r3
1:
	blr
