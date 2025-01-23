#  head.s contains the 32-bit startup code.
#  Two L3 task multitasking. The code of tasks are in kernel area,
#  just like the Linux. The kernel code is located at 0x10000.
.code32
SCRN_SEL	= 0x18
TSS0_SEL	= 0x20
LDT0_SEL	= 0x28
TSS1_SEL	= 0X30
LDT1_SEL	= 0x38
.global startup_32
.text
startup_32:
	movl $0x10,%eax
	mov %ax,%ds
#	mov %ax,%es
	lss init_stack,%esp

# setup base fields of descriptors.
	call setup_idt
	call setup_gdt
	movl $0x10,%eax		# reload all the segment registers
	mov %ax,%ds		# after changing gdt.
	mov %ax,%es
	mov %ax,%fs
	mov %ax,%gs
	lss init_stack,%esp

# setup up timer 8253 chip.
	movb $0x36, %al# 控制字：设置通道 0 工作在方式 3、计数初值采用二进制。
	movl $0x43, %edx# 8253 芯片控制字寄存器写端口。
	outb %al, %dx
	movl $11930, %eax        # timer frequency 100 HZ
	movl $0x40, %edx# 通道 0 的端口。
    outb %al, %dx # 分两次把初始计数值写入通道 0。
	movb %ah, %al
	outb %al, %dx

# setup timer & system call interrupt descriptors.
	movl $0x00080000, %eax
	movw $timer_interrupt, %ax
	movw $0x8E00, %dx
	movl $0x08, %ecx              # The PC default timer int.
	lea idt(,%ecx,8), %esi
	movl %eax,(%esi)
	movl %edx,4(%esi)
	movw $system_interrupt, %ax
	movw $0xef00, %dx
	movl $0x80, %ecx
	lea idt(,%ecx,8), %esi
	movl %eax,(%esi)
	movl %edx,4(%esi)

# unmask the timer interrupt.
#	movl $0x21, %edx
#	inb %dx, %al
#	andb $0xfe, %al
#	outb %al, %dx

# Move to user mode (task 0)
    pushfl                   # 将EFLAGS压栈
    andl $0xffffbfff, (%esp) # EFLAGS 的NT 标志位（第14 位）置0
    popfl
    movl $TSS0_SEL, %eax     # 把任务0的TSS段选择符加载到TR
    ltr %ax
    movl $LDT0_SEL, %eax    # 把任务0的LDT加载到LDTR
    lldt %ax
    movl $0, current        # 任务号
    sti
    pushl $0x17             # 数据段选择符
    pushl $init_stack       # 栈指针
    pushfl                  # EFLAGS
    pushl $0x0f             # 代码段选择符
    pushl $task0            # task0程序入口
    iret

/****************************************/
setup_gdt:
	lgdt lgdt_opcode
	ret

setup_idt:
	lea ignore_int,%edx
	movl $0x00080000,%eax
	movw %dx,%ax		/* selector = 0x0008 = cs */
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present */
	lea idt,%edi
	mov $256,%ecx
rp_sidt:
	movl %eax,(%edi)
	movl %edx,4(%edi)
	addl $8,%edi
	dec %ecx
	jne rp_sidt
	lidt lidt_opcode
	ret

# -----------------------------------
write_char:
	push %gs
	pushl %ebx
#	pushl %eax
	mov $SCRN_SEL, %ebx
	mov %bx, %gs
	movl scr_loc, %ebx
	shl $1, %ebx
	movb %al, %gs:(%ebx)
	shr $1, %ebx
	incl %ebx
	cmpl $2000, %ebx
	jb 1f
	movl $0, %ebx
1:	movl %ebx, scr_loc
#	popl %eax
	popl %ebx
	pop %gs
	ret

/***********************************************/
/* This is the default interrupt "handler" :-) */
.align 2
ignore_int:
	push %ds
	pushl %eax
	movl $0x10, %eax
	mov %ax, %ds
	movl $67, %eax            /* print 'C' */
	call write_char
	popl %eax
	pop %ds
	iret

/* Timer interrupt handler */
.align 2
timer_interrupt:
	push %ds
	pushl %eax
	movl $0x10, %eax		#让DS指向内核数据段
	mov %ax, %ds
	movb $0x20, %al			#允许其他硬件中断（向8259A发送E0I命令）
	outb %al, $0x20
	movl $1, %eax			#判断当前任务，如果是任务1就去执行任务0，是任务0就去执行任务1
	cmpl %eax, current
	je 1f
	movl %eax, current		#如果当前任务是0，把1存入current，跳转到1
	ljmp $TSS1_SEL, $0		#执行跳转
	jmp 2f
1:	movl $0, current		#如果...(和上面的全反过来)
	ljmp $TSS0_SEL, $0		#执行跳转
2:	popl %eax
	pop %ds
	iret

/* system call handler */
.align 2
system_interrupt:
	push %ds
	pushl %edx
	pushl %ecx
	pushl %ebx
	pushl %eax
	movl $0x10, %edx   #让DS指向内核数据段
	mov %dx, %ds
	call write_char    #调用显示字符子程序，显示AL中的字符
	popl %eax
	popl %ebx
	popl %ecx
	popl %edx
	pop %ds
	iret

/*********************************************/
current:.long 0
scr_loc:.long 0

.align 2
lidt_opcode:
	.word 256*8-1		# idt contains 256 entries
	.long idt		# This will be rewrite by code.
lgdt_opcode:
	.word (end_gdt-gdt)-1	# so does gdt
	.long gdt		# This will be rewrite by code.

	.align 8
idt:	.fill 256,8,0		# idt is uninitialized

gdt:	.quad 0x0000000000000000	/* NULL descriptor */
	.quad 0x00c09a00000007ff	/* 8Mb 0x08, base = 0x00000 */
	.quad 0x00c09200000007ff	/* 8Mb 0x10 */
	.quad 0x00c0920b80000002	/* screen 0x18 - for display */

	.word 0x0068, tss0, 0xe900, 0x0	# TSS0 descr 0x20
	.word 0x0040, ldt0, 0xe200, 0x0	# LDT0 descr 0x28
	.word 0x0068, tss1, 0xe900, 0x0	# TSS1 descr 0x30
	.word 0x0040, ldt1, 0xe200, 0x0	# LDT1 descr 0x38
end_gdt:
	.fill 128,4,0
init_stack:                          # Will be used as user stack for task0.
	.long init_stack
	.word 0x10

/*************************************/
.align 8
ldt0:	.quad 0x0000000000000000
	.quad 0x00c0fa00000003ff	# 0x0f, base = 0x00000
	.quad 0x00c0f200000003ff	# 0x17

tss0:	.long 0 			/* back link */
	.long krn_stk0, 0x10		/* esp0, ss0 */
	.long 0, 0, 0, 0, 0		/* esp1, ss1, esp2, ss2, cr3 */
	.long 0, 0, 0, 0, 0		/* eip, eflags, eax, ecx, edx */
	.long 0, 0, 0, 0, 0		/* ebx esp, ebp, esi, edi */
	.long 0, 0, 0, 0, 0, 0 		/* es, cs, ss, ds, fs, gs */
	.long LDT0_SEL, 0x8000000	/* ldt, trace bitmap */

	.fill 128,4,0
krn_stk0:
#	.long 0

/************************************/
.align 8
ldt1:	.quad 0x0000000000000000
	.quad 0x00c0fa00000003ff	# 0x0f, base = 0x00000
	.quad 0x00c0f200000003ff	# 0x17

tss1:	.long 0 			/* back link */
	.long krn_stk1, 0x10		/* esp0, ss0 */
	.long 0, 0, 0, 0, 0		/* esp1, ss1, esp2, ss2, cr3 */
	.long task1, 0x200		/* eip, eflags */
	.long 0, 0, 0, 0		/* eax, ecx, edx, ebx */
	.long usr_stk1, 0, 0, 0		/* esp, ebp, esi, edi */
	.long 0x17,0x0f,0x17,0x17,0x17,0x17 /* es, cs, ss, ds, fs, gs */
	.long LDT1_SEL, 0x8000000	/* ldt, trace bitmap */

	.fill 128,4,0
krn_stk1:

/************************************/
task0:
	movl $0x17, %eax		#让DS指向任务的局部数据段
	movw %ax, %ds			# ?
	movb $65, %al              /* print 'A' */
	int $0x80				#显示打印的字符
	movl $0xfff, %ecx		#循环 延时
1:	loop 1b
	jmp task0				#继续打印显示

task1:
	movl $0x17, %eax
	movw %ax, %ds			# ？
	movb $66, %al              /* print 'B' */
	int $0x80				#显示
	movl $0xfff, %ecx
1:	loop 1b
	jmp task1

	.fill 128,4,0			#看起来是栈空间
usr_stk1:
