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

with HAL;                     use HAL;
with System.Storage_Elements; use System.Storage_Elements;

package AGATE is

   type Task_ID is private;

   type Task_Priority is new Integer;

   type Task_Procedure is access procedure;

   type Task_Status is (Created, Ready, Running, Suspended_Alarm,
                        Suspended_Semaphore, Suspended_Mutex);

   function Image (Status : Task_Status) return String;

   function Name (ID : Task_ID) return String;

   function Image (ID : Task_ID) return String;

   type Time is new UInt64;

private

   type Task_Stack is new Storage_Array
     with Alignment => 8 * 8;

   type Task_Stack_Access is access all Task_Stack;

   type Task_Sec_Stack is new Storage_Array;
   type Task_Sec_Stack_Access is access all Task_Sec_Stack;

   type Task_Heap is new Storage_Array;
   type Task_Heap_Access is access all Task_Heap;

   subtype Task_Name is String (1 .. 10);

   type Process_Stack_Pointer is new System.Address;

   Null_PSP : constant Process_Stack_Pointer :=
     Process_Stack_Pointer (System.Null_Address);

   function Image (P : Process_Stack_Pointer) return String
   is (To_Integer (System.Address (P))'Img);


   type Task_Context is array (4 .. 12) of UInt32
     with Pack, Size => 9 * 32;

   type Task_Object;

   type Task_Object_Access is access all Task_Object;

   type Task_Object (Proc      : not null Task_Procedure;
                     Priority  : Task_Priority;
                     Stack     : Task_Stack_Access;
                     Sec_Stack : Task_Sec_Stack_Access;
                     Heap      : Task_Heap_Access)
   is limited record
      Next          : Task_Object_Access := null;
      Stack_Pointer : Process_Stack_Pointer := Null_PSP;
      Name          : Task_Name := (others => ' ');
      Context       : Task_Context := (others => 0);
      Alarm_Time    : Time := 0;
      Status        : Task_Status := Created;
   end record;

   type Task_ID is new Task_Object_Access;

end AGATE;
