------------------------------------------------------------------------------
--                                                                          --
--                Copyright (C) 2017-2020, Fabien Chouteau                  --
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

with Ada.Text_IO;
with System.Machine_Code;  use System.Machine_Code;

with Cortex_M_SVD.SCB;     use Cortex_M_SVD.SCB;

package body AGATE.Arch is

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
      type Stack_Array is array (1 .. 8) of Word
        with Pack, Size => 8 * 32;

      Context : Stack_Array
        with Address => T.Stack (T.Stack'Last)'Address + 1 - 8 * 32;

   begin
      --  xPSR
      Context (8) := 2**24; -- Set the thumb bit

      --  PC
      Context (7) := Word (To_Integer (T.Proc.all'Address));

      Ada.Text_IO.Put_Line ("Set start PC address: " &
                              Hex (UInt32 (Context (7))));

      --  LR
      Context (6) := 0;

      --  R12
      Context (5) := 0;

      --  R3
      Context (4) := 0;

      --  R2
      Context (3) := 0;

      --  R1
      Context (2) := 0;

      --  R0
      Context (1) := 0;

      T.Stack_Pointer := Process_Stack_Pointer (Context (1)'Address);
   end Initialize_Task_Context;

   ------------------
   -- Jump_In_Task --
   ------------------

   procedure Jump_In_Task
     (T : Task_ID)
   is
   begin

      Ada.Text_IO.Put_Line ("Starting task at PSP: " & Image (T.Stack_Pointer));

      Asm (
           --  Set thread mode stack (PSP)
           "msr psp, %0" & ASCII.LF &

           --  Switch thread mode to use PSP, and thread mode unpriviledge
           "msr control, %1" & ASCII.LF &

           --  Call task procedure
           "blx %2",
           Inputs => (Process_Stack_Pointer'Asm_Input ("r", T.Stack_Pointer),
                      Word'Asm_Input ("r", 2#11#),
                      Task_Procedure'Asm_Input ("r", T.Proc)),
           Volatile => True);

      raise Program_Error;
   end Jump_In_Task;

end AGATE.Arch;
