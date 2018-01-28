------------------------------------------------------------------------------
--                                                                          --
--                Copyright (C) 2017-2018, Fabien Chouteau                  --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--     1. Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--     2. Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--     3. Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS    --
--   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT      --
--   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR  --
--   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   --
--   HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, --
--   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT       --
--   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,  --
--   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY  --
--   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT    --
--   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE  --
--   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   --
--                                                                          --
------------------------------------------------------------------------------

with System;                use System;
with System.Machine_Code;   use System.Machine_Code;
with AGATE.Arch.RISCV;      use AGATE.Arch.RISCV;
with AGATE_Arch_Parameters; use AGATE_Arch_Parameters;

package body AGATE.Arch is

   Kernel_Stack_Pointer : Word;
   pragma Export (Asm, Kernel_Stack_Pointer, "__agate_kernel_stack_pointer");

   --------------------
   -- Idle_Procedure --
   --------------------

   procedure Idle_Procedure is
   begin
      loop
         Asm ("wfi", Volatile => True);
      end loop;
   end Idle_Procedure;

   -----------------------------
   -- Initialize_Task_Context --
   -----------------------------

   procedure Initialize_Task_Context
     (T : Task_ID)
   is
      SP : constant Address := T.Stack (T.Stack'Last)'Address + 1 - 32 * 4;
   begin
      T.Stack_Pointer := Process_Stack_Pointer (SP);
      T.Context (Ctx_SP_Index) := Word (To_Integer (SP));
      T.Context (Ctx_PC_Index) := Word (To_Integer (T.Proc.all'Address));
   end Initialize_Task_Context;

   ------------------
   -- Jump_In_Task --
   ------------------

   procedure Jump_In_Task
     (T : Task_ID)
   is
      Mstat : Mstatus_Reg;
      Ctx : Task_Context renames T.Context;
   begin
      Mstat := Mstatus;

      --  Set privilege mode when returning from machine interrupt
      Mstat.MPP := 0; -- User

      --  Set interrupt enable bit that will be loaded when returning from
      --  machine trap.
      Mstat.MPIE := True;

      Write_Mstatus (Mstat);

      --  Use the current stack as kernel stack pointer
      Asm ("mv %0, sp",
           Outputs => Word'Asm_Output ("=r", Kernel_Stack_Pointer),
           Volatile => True);

      --  Save the task context pointer in the mscratch register
      Write_Mcratch (UInt32 (To_Integer (Ctx (Ctx'First)'Address)));

      --  Theard Pointer, unused at the time...
      --  Ctx (Ctx_TP_Index) :=

      Asm ("mv sp, %0"         & ASCII.LF &
           "csrw mepc, %1"     & ASCII.LF &
           "mret"              & ASCII.LF,
           Inputs => (Word'Asm_Input ("r", Ctx (Ctx_SP_Index)),  -- SP
                      Word'Asm_Input ("r", Ctx (Ctx_PC_Index))), -- PC
           Volatile => True);
      loop
         null;
      end loop;
   end Jump_In_Task;

end AGATE.Arch;
