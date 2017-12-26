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

with Ada.Text_IO;              use Ada.Text_IO;

with Ada.Unchecked_Conversion;
with AGATE.Tasking;            use AGATE.Tasking;

package body AGATE.Semaphores is

   function To_UInt32_Internal is new Ada.Unchecked_Conversion
     (Semaphore_ID, UInt32);

   function To_ID_Internal is new Ada.Unchecked_Conversion
     (UInt32, Semaphore_ID);

   -----------
   -- Count --
   -----------

   function Count
     (Sem : Semaphore_ID)
      return Semaphore_Count
   is (Semaphore_Access (Sem).all.Count);

   ------------
   -- Signal --
   ------------

   procedure Signal
     (Sem : Semaphore_ID)
   is
   begin
      Signal (Semaphore_Access (Sem).all);
   end Signal;


   ---------------------
   -- Wait_For_Signal --
   ---------------------

   procedure Wait_For_Signal
     (Sem : Semaphore_ID)
   is
   begin
      Wait_For_Signal (Semaphore_Access (Sem).all);
   end Wait_For_Signal;

   ---------------
   -- To_UInt32 --
   ---------------

   function To_UInt32
     (ID : Semaphore_ID)
      return UInt32
   is (To_UInt32_Internal (ID));

   -----------
   -- To_ID --
   -----------

   function To_ID
     (ID : UInt32)
      return Semaphore_ID
   is (To_ID_Internal (ID));

   -----------
   -- Count --
   -----------

   function Count
     (Sem : Semaphore)
      return Semaphore_Count
   is (Sem.Count);

   ------------
   -- Signal --
   ------------

   procedure Signal
     (Sem : in out Semaphore)
   is
   begin
      if Sem.Waiting_List /= null then
         Resume_One_Task (Sem);
      else
         Sem.Count := Sem.Count + 1;
      end if;
   end Signal;

   ---------------------
   -- Wait_For_Signal --
   ---------------------

   procedure Wait_For_Signal
     (Sem : in out Semaphore)
   is
   begin
      if Sem.Count >= 1 then
         Sem.Count := Sem.Count - 1;
      else
         declare
            T : constant Task_Object_Access := Task_Object_Access (Current_Task);
         begin
            --  Suspend the current task
            Tasking.Suspend (Tasking.Semaphore);

            --  Add it to the waiting queue
            Insert_Task (Sem, T);

            if Context_Switch_Needed then
               Trigger_Context_Switch;
            end if;
         end;
      end if;
   end Wait_For_Signal;

   -----------------
   -- Insert_Task --
   -----------------

   procedure Insert_Task
     (Sem : in out Semaphore;
      T   : Task_Object_Access)
   is
   begin
      --  TODO: This is LIFO, so probably not the best... :)
      T.Next := Sem.Waiting_List;
      Sem.Waiting_List := T;
   end Insert_Task;

   ---------------------
   -- Resume_One_Task --
   ---------------------

   procedure Resume_One_Task
     (Sem : in out Semaphore)
   is
      T : Task_Object_Access;
   begin
      T := Sem.Waiting_List;
      Sem.Waiting_List := T.Next;

      Tasking.Resume (Task_ID (T));
   end Resume_One_Task;


end AGATE.Semaphores;
