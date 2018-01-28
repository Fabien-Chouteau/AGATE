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

with AGATE.API.Static_Task;
with AGATE.API.Static_Semaphore;
with AGATE.API.Static_Mutex;

package Test_Static_Tasks is

   Test_Time_Unit : constant := 1_000_000;

   procedure T1_Proc;

   package T1 is new AGATE.API.Static_Task
     (Stack_Size     => 512,
      Sec_Stack_Size => 0,
      Heap_Size      => 0,
      Priority       => 1,
      Proc           => T1_Proc'Access,
      Name           => "Static T1");

   procedure T2_Proc;

   package T2 is new AGATE.API.Static_Task
     (Stack_Size     => 512,
      Sec_Stack_Size => 0,
      Heap_Size      => 0,
      Priority       => 2,
      Proc           => T2_Proc'Access,
      Name           => "Static T2");

   package Static_Semaphore is new AGATE.API.Static_Semaphore
     (Name => "Static Sem");

   package Static_Mutex is new AGATE.API.Static_Mutex
     (Priority => 5,
      Name     => "Static Mutex prio 2");

end Test_Static_Tasks;
