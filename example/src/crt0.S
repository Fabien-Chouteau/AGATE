	.syntax unified

#if defined(__SOFTFP__)
	.cpu cortex-m0
#else
        /* FPU support means at least cortex-m4 compatibility */
        .cpu cortex-m4
#endif

        .thumb

	.text
	.globl __vectors
	.p2align 9
	.section .vectors,"a"
__vectors:
	/* Cortex-M core interrupts */
	.word   __stack_end          /* stack top address */
	.word   _start               /* 1 Reset */
	.word   NMI_Handler          /* 2 NMI. */
	.word   HardFault_Handler    /* 3 Hard fault. */
	.word   MemMang_Handler      /* 4 Mem manage. */
	.word   BusFault_Handler     /* 5 Bus fault. */
	.word   UsageFault_Handler   /* 6 Usage fault. */
	.word   Default_Handler      /* 7 reserved. */
	.word   Default_Handler      /* 8 reserved. */
	.word   Default_Handler      /* 9 reserved. */
	.word   Default_Handler      /* 10 reserved. */
	.word   SVC_Handler          /* 11 SVCall. */
	.word   DebugMon_Handler     /* 12 Breakpoint. */
	.word   Default_Handler      /* 13 reserved. */
	.word   PendSV_Handler       /* 14 PendSV. */
	.word   SysTick_Handler      /* 15 Systick. */
	/* MCU interrupts */
        .word __unknown_interrupt_handler /* 0 */
        .word __unknown_interrupt_handler /* 1 */
        .word __unknown_interrupt_handler /* 2 */
        .word __unknown_interrupt_handler /* 3 */
        .word __unknown_interrupt_handler /* 4 */
        .word __unknown_interrupt_handler /* 5 */
        .word __unknown_interrupt_handler /* 6 */
        .word __unknown_interrupt_handler /* 7 */
        .word __unknown_interrupt_handler /* 8 */
        .word __unknown_interrupt_handler /* 9 */
        .word __unknown_interrupt_handler /* 10 */
        .word __unknown_interrupt_handler /* 11 */
        .word __unknown_interrupt_handler /* 12 */
        .word __unknown_interrupt_handler /* 13 */
        .word __unknown_interrupt_handler /* 14 */
        .word __unknown_interrupt_handler /* 15 */
        .word __unknown_interrupt_handler /* 16 */
        .word __unknown_interrupt_handler /* 17 */
        .word __unknown_interrupt_handler /* 18 */
        .word __unknown_interrupt_handler /* 19 */
        .word __unknown_interrupt_handler /* 20 */
        .word __unknown_interrupt_handler /* 21 */
        .word __unknown_interrupt_handler /* 22 */
        .word __unknown_interrupt_handler /* 23 */
        .word __unknown_interrupt_handler /* 24 */
        .word __unknown_interrupt_handler /* 25 */
        .word __unknown_interrupt_handler /* 26 */
        .word __unknown_interrupt_handler /* 27 */
        .word __unknown_interrupt_handler /* 28 */
        .word __unknown_interrupt_handler /* 29 */
        .word __unknown_interrupt_handler /* 30 */
        .word __unknown_interrupt_handler /* 31 */
        .word __unknown_interrupt_handler /* 32 */
        .word __unknown_interrupt_handler /* 33 */
        .word __unknown_interrupt_handler /* 34 */
        .word __unknown_interrupt_handler /* 35 */
        .word __unknown_interrupt_handler /* 36 */
        .word __unknown_interrupt_handler /* 37 */
        .word __unknown_interrupt_handler /* 38 */
        .word __unknown_interrupt_handler /* 39 */
        .word __unknown_interrupt_handler /* 40 */
        .word __unknown_interrupt_handler /* 41 */
        .word __unknown_interrupt_handler /* 42 */
        .word __unknown_interrupt_handler /* 43 */
        .word __unknown_interrupt_handler /* 44 */
        .word __unknown_interrupt_handler /* 45 */
        .word __unknown_interrupt_handler /* 46 */
        .word __unknown_interrupt_handler /* 47 */
        .word __unknown_interrupt_handler /* 48 */
        .word __unknown_interrupt_handler /* 49 */
        .word __unknown_interrupt_handler /* 50 */
        .word __unknown_interrupt_handler /* 51 */
        .word __unknown_interrupt_handler /* 52 */
        .word __unknown_interrupt_handler /* 53 */
        .word __unknown_interrupt_handler /* 54 */
        .word __unknown_interrupt_handler /* 55 */
        .word __unknown_interrupt_handler /* 56 */
        .word __unknown_interrupt_handler /* 57 */
        .word __unknown_interrupt_handler /* 58 */
        .word __unknown_interrupt_handler /* 59 */
        .word __unknown_interrupt_handler /* 60 */
        .word __unknown_interrupt_handler /* 61 */
        .word __unknown_interrupt_handler /* 62 */
        .word __unknown_interrupt_handler /* 63 */
        .word __unknown_interrupt_handler /* 64 */
        .word __unknown_interrupt_handler /* 65 */
        .word __unknown_interrupt_handler /* 66 */
        .word __unknown_interrupt_handler /* 67 */
        .word __unknown_interrupt_handler /* 68 */
        .word __unknown_interrupt_handler /* 69 */
        .word __unknown_interrupt_handler /* 70 */
        .word __unknown_interrupt_handler /* 71 */
        .word __unknown_interrupt_handler /* 72 */
        .word __unknown_interrupt_handler /* 73 */
        .word __unknown_interrupt_handler /* 74 */
        .word __unknown_interrupt_handler /* 75 */
        .word __unknown_interrupt_handler /* 76 */
        .word __unknown_interrupt_handler /* 77 */
        .word __unknown_interrupt_handler /* 78 */
        .word __unknown_interrupt_handler /* 79 */
        .word __unknown_interrupt_handler /* 80 */
        .word __unknown_interrupt_handler /* 81 */


	.text

   .weak      __unknown_interrupt_handler
   .thumb_set __unknown_interrupt_handler,__gnat_irq_trap

	.thumb_func
.weak __gnat_irq_trap
.type __gnat_irq_trap, %function
__gnat_irq_trap:
0:	b 0b
	.size __gnat_irq_trap, . - __gnat_irq_trap

	.thumb_func
.weak Default_Handler
.type Default_Handler, %function
Default_Handler:
0:	b 0b
	.size Default_Handler, . - Default_Handler

	.thumb_func
.weak NMI_Handler
.type NMI_Handler, %function
NMI_Handler:
0:	b 0b
	.size NMI_Handler, . - NMI_Handler

	.thumb_func
.weak DebugMon_Handler
.type DebugMon_Handler, %function
DebugMon_Handler:
0:	b 0b
	.size DebugMon_Handler, . - DebugMon_Handler

	.thumb_func
.weak SVC_Handler
.type SVC_Handler, %function
SVC_Handler:
0:	b 0b
	.size SVC_Handler, . - SVC_Handler

	.thumb_func
.weak PendSV_Handler
.type PendSV_Handler, %function
PendSV_Handler:
0:	b 0b
	.size PendSV_Handler, . - PendSV_Handler

	.thumb_func
.weak SysTick_Handler
.type SysTick_Handler, %function
SysTick_Handler:
0:	b 0b
	.size SysTick_Handler, . - SysTick_Handler

	.thumb_func
fault:	b fault


        /*********/
        /* .data */
        /*********/
        .section .data.argv
argv_str:
        .ascii  "main\0"

        .align 4
argv:
        .word argv_str
        .word 0

        /**********/
        /* _start */
        /**********/

	.text
	.thumb_func
	.globl _start

_start:

	/* Set the stack pointer */
	ldr	r1,=__stack_end
	mov     sp, r1

	/* Copy .data */
	.thumb_func
_startup_copy_data:
	ldr	r0,=__data_start
	ldr	r1,=__data_words
	ldr	r2,=__data_load
	cmp     r1,#0
	beq     1f
0:	ldr	r4,[r2]
	str	r4,[r0]
        adds    r2,#4
        adds    r0,#4
	subs	r1,r1,#1
	bne	0b
1:
        .size _startup_copy_data, . - _startup_copy_data

	/* Copy .CCM_data */
	.thumb_func
_startup_copy_CCM_data:
	ldr	r0,=__CCM_data_start
	ldr	r1,=__CCM_data_words
	ldr	r2,=__CCM_data_load
	cmp     r1,#0
	beq     1f
0:	ldr	r4,[r2]
	str	r4,[r0]
        adds    r2,#4
        adds    r0,#4
	subs	r1,r1,#1
	bne	0b
1:
        .size _startup_copy_CCM_data, . - _startup_copy_CCM_data

	/* Clear .bss */
	.thumb_func
_startup_clear_bss:
	ldr	r0,=__bss_start
	ldr	r1,=__bss_words
	movs	r2,#0
	cmp     r1,#0
	beq     1f
0:	str	r2,[r0]
        adds    r0,#4
	subs	r1,r1,#1
	bne	0b
1:
        .size _startup_clear_bss, . - _startup_clear_bss

	/* Clear .CCM_bss */
	.thumb_func
_startup_clear_CCM_bss:
	ldr	r0,=__CCM_bss_start
	ldr	r1,=__CCM_bss_words
	movs	r2,#0
	cmp     r1,#0
	beq     1f
0:	str	r2,[r0]
        adds    r0,#4
	subs	r1,r1,#1
	bne	0b
1:
        .size _startup_clear_CCM_bss, . - _startup_clear_CCM_bss


#if !defined(__SOFTFP__)
        /**************/
        /* Enable FPU */
        /**************/

        movw     r0,#0xED88
        movt     r0,#0xE000
        ldr      r1,[r0]
        orr      r1,r1,#(0xF << 20)
        str      r1,[r0]

        dsb
        isb

#endif

        /* Call static constructors */
.weak __libc_init_array
        ldr     r5,=__libc_init_array
        cmp     r5,#0
        beq     .skip_libc_init
        blx     r5
.skip_libc_init:

        /* Call main, with argc, argv */
        mov     r0,#1
        ldr	r1,=argv
        bl	main

        /* Save main's return value */
        mov r4, r0

        /* static destructors */
.weak __libc_fini_array
        ldr     r5,=__libc_fini_array
        cmp     r5,#0
        beq     .skip_libc_fini
        blx     r5
.skip_libc_fini:

        /* Restore main's return value */
        mov r0, r4

        bl	_exit

hang:   b .
