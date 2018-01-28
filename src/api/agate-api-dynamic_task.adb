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

with AGATE.Scheduler; use AGATE.Scheduler;

package body AGATE.API.Dynamic_Task is

   ------------
   -- Create --
   ------------

   function Create
     (Stack_Size     : Storage_Count;
      Sec_Stack_Size : Storage_Count;
      Heap_Size      : Storage_Count;
      Priority       : Task_Priority;
      Proc           : Task_Procedure;
      Name           : String)
      return Task_ID
   is
      Stack     : Task_Stack_Access     := new Task_Stack (1 .. Stack_Size);
      Sec_Stack : Task_Sec_Stack_Access := new Task_Sec_Stack (1 .. Sec_Stack_Size);
      Heap      : Task_Heap_Access      := new Task_Heap (1 .. Heap_Size);

      The_Task  : Task_Object_Access :=
        new Task_Object (Proc      => Proc,
                         Base_Prio => Internal_Task_Priority (Priority),
                         Stack     => Stack,
                         Sec_Stack => Sec_Stack,
                         Heap      => Heap);
   begin
      Register (Task_ID (The_Task), Name);
      return Task_ID (The_Task);
   end Create;

end AGATE.API.Dynamic_Task;
