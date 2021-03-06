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

with HAL;                     use HAL;
with System.Storage_Elements; use System.Storage_Elements;
with Tools;                   use Tools;

with AGATE_Arch_Parameters;

private with AGATE_Types_Data.Traces;

package AGATE is

   subtype Word is AGATE_Arch_Parameters.Word;

   -- Task --

   type Task_ID is private;

   Invalid_Task : constant Task_ID;

   type Internal_Task_Priority is range -1 .. 256;
   subtype Task_Priority is Internal_Task_Priority range 0 .. 256;

   type Task_Procedure is access procedure;

   type Task_Status is (Created, Ready, Running, Fault, Suspended_Alarm,
                        Suspended_Semaphore, Suspended_Mutex);

   subtype Task_Name is String (1 .. 10);

   function Image (Status : Task_Status) return String;

   function Name (ID : Task_ID) return String;

   function Image (ID : Task_ID) return String;

   -- Time --

   type Time is new UInt64;

   -- Semaphore --

   type Semaphore_Count is new Natural;

   type Semaphore_ID is private;

   Invalid_Semaphore : constant Semaphore_ID;

   -- Mutex --

   type Mutex_ID is private;

   Invalid_Mutex : constant Mutex_ID;

   type Process_Stack_Pointer is new System.Address;

private

   -- Task --

   type Task_Stack is new Storage_Array;

   pragma Warnings (Off, "suspiciously large alignment");
   for Task_Stack'Alignment use 8 * 8;
   pragma Warnings (On, "suspiciously large alignment");

   type Task_Stack_Access is access all Task_Stack;

   type Task_Sec_Stack is new Storage_Array;
   type Task_Sec_Stack_Access is access all Task_Sec_Stack;

   type Task_Heap is new Storage_Array;
   type Task_Heap_Access is access all Task_Heap;

   Null_PSP : constant Process_Stack_Pointer :=
     Process_Stack_Pointer (System.Null_Address);

   function Image (P : Process_Stack_Pointer) return String
   is (Hex (UInt32 (To_Integer (System.Address (P)))));

   type Task_Object;

   type Task_Object_Access is access all Task_Object;

   type Task_Object (Proc      : not null Task_Procedure;
                     Base_Prio : Internal_Task_Priority;
                     Stack     : Task_Stack_Access;
                     Sec_Stack : Task_Sec_Stack_Access;
                     Heap      : Task_Heap_Access)
   is limited record
      Canary        : UInt32;
      Current_Prio  : Internal_Task_Priority;
      Next          : Task_Object_Access := null;
      Stack_Pointer : Process_Stack_Pointer := Null_PSP;
      Name          : Task_Name := (others => ' ');
      Context       : AGATE_Arch_Parameters.Task_Context;
      Alarm_Time    : Time := 0;
      Status        : Task_Status := Created;
      All_Task_Next : Task_Object_Access := null;

      Trace_Data    : AGATE_Types_Data.Traces.Task_Data;
   end record;

   type Task_ID is new Task_Object_Access;

   Invalid_Task : constant Task_ID := null;

   All_Tasks_List : Task_Object_Access := null;

   procedure Set_Stack_Canary (T : Task_ID);
   procedure Check_Stack_Canary (T : Task_ID);
   procedure Check_All_Stack_Canaries;

   -- Semaphore --

   type Semaphore (Initial_Count : Semaphore_Count := 0)
   is limited record
      Count        : Semaphore_Count := Initial_Count;
      Waiting_List : Task_Object_Access := null;

      Trace_Data    : AGATE_Types_Data.Traces.Semaphore_Data;
   end record;

   type Semaphore_Access is access all Semaphore;
   type Semaphore_ID is new Semaphore_Access;
   pragma No_Strict_Aliasing (Semaphore_ID);

   Invalid_Semaphore : constant Semaphore_ID := null;

   function To_Word (ID : Semaphore_ID) return Word;
   function To_ID (ID : Word) return Semaphore_ID;

   -- Mutex --

   type Mutex (Prio : Internal_Task_Priority)
   is limited record
      Owner        : Task_Object_Access := null;
      Waiting_List : Task_Object_Access := null;

      Trace_Data    : AGATE_Types_Data.Traces.Mutex_Data;
   end record;

   type Mutex_Access is access all Mutex;
   type Mutex_ID is new Mutex_Access;
   pragma No_Strict_Aliasing (Mutex_ID);

   Invalid_Mutex : constant Mutex_ID := null;

   function To_Word (ID : Mutex_ID) return Word;
   function To_ID (ID : Word) return Mutex_ID;

end AGATE;
