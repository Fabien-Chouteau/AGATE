------------------------------------------------------------------------------
--                                                                          --
--                   Copyright (C) 2017, Fabien Chouteau                    --
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

with AGATE;                      use AGATE;
with AGATE.Tasking.Dynamic_Task;
with Ada.Text_IO;
with AGATE.SysCalls;
with System.Machine_Code; use System.Machine_Code;
with Test_Static_Tasks;
with AGATE.Semaphores;
with AGATE.Semaphores.Dynamic;

package body Test_Dynamic_Tasks is

   Dyn_Semaphore : AGATE.Semaphores.Semaphore_ID :=
     AGATE.Semaphores.Dynamic.Create;

   -------------
   -- T1_Proc --
   -------------

   procedure T1_Proc is
   begin
      Ada.Text_IO.Put_Line ("---> Dynamic T1 Signal Static_Semaphore");
      AGATE.SysCalls.Signal (Test_Static_Tasks.Static_Semaphore.ID);
      Ada.Text_IO.Put_Line ("---> Dynamic T1 Signal Dynamic_Semaphore");
      AGATE.SysCalls.Signal (Test_Static_Tasks.Static_Semaphore.ID);
      loop
--           Ada.Text_IO.Put_Line ("---> Dynamic T1 Clock:" &
--                                   AGATE.SysCalls.Clock'Img);
--           AGATE.SysCalls.Yield;
--           for I in 1 .. 100_000 loop
--              null;
--           end loop;
         Asm ("wfi", Volatile => True);
      end loop;
   end T1_Proc;

   -------------
   -- T2_Proc --
   -------------

   procedure T2_Proc is
      Now : Time;
   begin
      loop
         Now := Time (AGATE.SysCalls.Clock);
         Ada.Text_IO.Put_Line ("---> Dynamic T2 Clock:" & Now'Img);
         AGATE.SysCalls.Delay_Until (Now + 1_000_000);
      end loop;
   end T2_Proc;

   ------------
   -- Create --
   ------------

   procedure Create is
      T : AGATE.Task_ID;
   begin
      T := AGATE.Tasking.Dynamic_Task.Create
        (Stack_Size     => 1024,
         Sec_Stack_Size => 1024,
         Heap_Size      => 1024,
         Priority       => 0,
         Proc           => T1_Proc'Access);

      AGATE.Tasking.Register (T, "Dynamic T1");

      T := AGATE.Tasking.Dynamic_Task.Create
        (Stack_Size     => 1024,
         Sec_Stack_Size => 1024,
         Heap_Size      => 1024,
         Priority       => 3,
         Proc           => T2_Proc'Access);
      AGATE.Tasking.Register (T, "Dynamic T2");
   end Create;

   ----------------------
   -- Dyamic_Semaphore --
   ----------------------

   function Dyamic_Semaphore
     return AGATE.Semaphores.Semaphore_ID
   is (Dyn_Semaphore);

end Test_Dynamic_Tasks;
