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

with Ada.Text_IO;   use Ada.Text_IO;

with AGATE.Tasking; use AGATE.Tasking;
with AGATE.Traces;

package body AGATE.Mutexes is

   ---------------
   -- Wait_Lock --
   ---------------

   procedure Wait_Lock (Mut : Mutex_ID) is
      T : constant Task_Object_Access := Task_Object_Access (Current_Task);
   begin
      if Mut.Owner = null then
         Mut.Owner := T;
         Traces.Lock (Mut, Current_Task);
      else
         --  Suspend the current task
         Tasking.Suspend (Tasking.Semaphore);

         --  Add it to the waiting queue
         Insert_Task (Mut.all, T);

         if Context_Switch_Needed then
            Trigger_Context_Switch;
         end if;
      end if;
   end Wait_Lock;

   --------------
   -- Try_Lock --
   --------------

   function Try_Lock (Mut : Mutex_ID) return Boolean is
   begin
      if Mut.Owner = null then
         Mut.Owner := Task_Object_Access (Current_Task);
         Traces.Lock (Mut, Current_Task);
         return True;
      else
         return False;
      end if;
   end Try_Lock;

   -------------
   -- Release --
   -------------

   procedure Release (Mut : Mutex_ID) is
   begin
      if Mut.Owner = null then
         raise Program_Error;
      end if;

      Traces.Release (Mut, Current_Task);

      Mut.Owner := Mut.Waiting_List;

      if Mut.Owner /= null then
         Mut.Waiting_List := Mut.Owner.Next;

         Traces.Lock (Mut, Current_Task);

         Tasking.Resume (Task_ID (Mut.Owner));
      end if;
   end Release;

   -----------------
   -- Insert_Task --
   -----------------

   procedure Insert_Task
     (Mut : in out Mutex;
      T   : Task_Object_Access)
   is
   begin
      --  TODO: This is LIFO, so probably not the best... :)
      T.Next := Mut.Waiting_List;
      Mut.Waiting_List := T;
   end Insert_Task;

end AGATE.Mutexes;
