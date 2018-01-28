  .section      .text.entry	
  .align 2
  .global trap_entry
trap_entry:

  csrr tp, mscratch // Get task context pointer from mscratch

  //  Store the context
  sw x1, 1*4(tp)
  sw x2, 2*4(tp)
  sw x3, 3*4(tp) 
  // sw x4, 4*4(tp) Don't modify TP on the context. TP is always the same.
  sw x5, 5*4(tp)
  sw x6, 6*4(tp)
  sw x7, 7*4(tp)
  sw x8, 8*4(tp)
  sw x9, 9*4(tp)
  sw x10, 10*4(tp)
  sw x11, 11*4(tp)
  sw x12, 12*4(tp)
  sw x13, 13*4(tp)
  sw x14, 14*4(tp)
  sw x15, 15*4(tp)
  sw x16, 16*4(tp)
  sw x17, 17*4(tp)
  sw x18, 18*4(tp)
  sw x19, 19*4(tp)
  sw x20, 20*4(tp)
  sw x21, 21*4(tp)
  sw x22, 22*4(tp)
  sw x23, 23*4(tp)
  sw x24, 24*4(tp)
  sw x25, 25*4(tp)
  sw x26, 26*4(tp)
  sw x27, 27*4(tp)
  sw x28, 28*4(tp)
  sw x29, 29*4(tp)
  sw x30, 30*4(tp)
  sw x31, 31*4(tp)

  csrr t0, mepc
  sw t0, 0*4(tp) // Save PC in the task context

  // Switch to the kernel stack
  // This loads the value of the symbol, not its address
  lw sp, __agate_kernel_stack_pointer 

  //  Call the trap handler with mcause as the first argument
  csrr a0, mcause
  call trap_handler

  csrr tp, mscratch // Get task context pointer from mscratch

  lw t0, 0*4(tp) // Load task PC from the task context
  csrw mepc, t0

  lw x1, 1*4(tp)
  lw x2, 2*4(tp)
  lw x3, 3*4(tp)
  // x4 is tp alias, we load it later
  lw x5, 5*4(tp)
  lw x6, 6*4(tp)
  lw x7, 7*4(tp)
  lw x8, 8*4(tp)
  lw x9, 9*4(tp)
  lw x10, 10*4(tp)
  lw x11, 11*4(tp)
  lw x12, 12*4(tp)
  lw x13, 13*4(tp)
  lw x14, 14*4(tp)
  lw x15, 15*4(tp)
  lw x16, 16*4(tp)
  lw x17, 17*4(tp)
  lw x18, 18*4(tp)
  lw x19, 19*4(tp)
  lw x20, 20*4(tp)
  lw x21, 21*4(tp)
  lw x22, 22*4(tp)
  lw x23, 23*4(tp)
  lw x24, 24*4(tp)
  lw x25, 25*4(tp)
  lw x26, 26*4(tp)
  lw x27, 27*4(tp)
  lw x28, 28*4(tp)
  lw x29, 29*4(tp)
  lw x30, 30*4(tp)
  lw x31, 31*4(tp)

  lw x4, 4*4(tp) //  x4 is tp alias

  mret

.weak trap_handler
trap_handler:
1:
  j 1b
	
