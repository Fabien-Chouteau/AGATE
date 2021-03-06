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

with AGATE;                      use AGATE;
with AGATE.API;                  use AGATE.API;
with AGATE.API.Dynamic_Task;
with Test_Static_Tasks;
with AGATE.API.Dynamic_Semaphore;

package body Test_Dynamic_Tasks is

   Dyn_Semaphore : constant AGATE.Semaphore_ID :=
     AGATE.API.Dynamic_Semaphore.Create (Name => "Dynamic Sem");

   T1, T2 : AGATE.Task_ID := Invalid_Task;
   pragma Unreferenced (T2, T1);

   procedure T1_Proc;
   procedure T2_Proc;

   -------------
   -- T1_Proc --
   -------------

   procedure T1_Proc is
      Now : Time;
   begin
      Print_Line ("---> Dynamic T1 Signal Dynamic_Semaphore");
      AGATE.API.Signal (Dyn_Semaphore);
      loop
         Now := AGATE.API.Clock;
         Print_Line ("---> Dynamic T1 Clock:" & Now'Img);
         AGATE.API.Delay_Until (Now + Test_Static_Tasks.Test_Time_Unit);
      end loop;
   end T1_Proc;

   -------------
   -- T2_Proc --
   -------------

   procedure T2_Proc is
      Now : Time;
   begin
      Print_Line ("---> Dynamic T2 Wait_For_Signal on Dynamic_Semaphore");

      AGATE.API.Wait_For_Signal (Dyn_Semaphore);
      Print_Line ("---> Dynamic T2 released");

      loop
         Now := AGATE.API.Clock;
         Print_Line ("---> Dynamic T2 Clock:" & Now'Img);
         AGATE.API.Delay_Until (Now + Test_Static_Tasks.Test_Time_Unit);
      end loop;
   end T2_Proc;

   ------------
   -- Create --
   ------------

   procedure Create is
   begin
      T1 := AGATE.API.Dynamic_Task.Create
        (Stack_Size     => 4096,
         Sec_Stack_Size => 0,
         Heap_Size      => 0,
         Priority       => 0,
         Proc           => T1_Proc'Access,
         Name           => "Dynamic T1");

      T2 := AGATE.API.Dynamic_Task.Create
        (Stack_Size     => 4096,
         Sec_Stack_Size => 0,
         Heap_Size      => 0,
         Priority       => 3,
         Proc           => T2_Proc'Access,
         Name           => "Dynamic T2");
   end Create;

   ----------------------
   -- Dyamic_Semaphore --
   ----------------------

   function Dyamic_Semaphore
     return AGATE.Semaphore_ID
   is (Dyn_Semaphore);

end Test_Dynamic_Tasks;
