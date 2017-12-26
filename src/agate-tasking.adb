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

with Ada.Text_IO;         use Ada.Text_IO;
with HAL;                 use HAL;
with System.Machine_Code; use System.Machine_Code;
with System;              use System;
with Cortex_M_SVD.SCB;    use Cortex_M_SVD.SCB;
with AGATE.Interrupts;    use AGATE.Interrupts;

package body AGATE.Tasking is

   PendSV_Interrupt_ID : constant Interrupt_ID := -2;
   Running_Task : Task_Object_Access := null;

   Ready_Tasks : Task_Object_Access := null;

   procedure Set_PSP (Addr : Process_Stack_Pointer);
   function PSP return Process_Stack_Pointer;

   function Current_Task_Context return System.Address;
   pragma Export (C, Current_Task_Context, "current_task_context");

   procedure Jump_In_Task (T : Task_Object)
     with No_Return;

   function In_Ready_Tasks (ID : Task_ID) return Boolean;
   procedure Print_Ready_Tasks;

   procedure Extract
     (ID : Task_ID)
     with Pre => ID = Task_To_Run;

   procedure Insert
     (ID : Task_ID)
     with Pre => not In_Ready_Tasks (ID);

   procedure Context_Switch_Handler;
   pragma Machine_Attribute (Context_Switch_Handler, "naked");
   pragma Export (C, Context_Switch_Handler, "PendSV_Handler");

   -----------------------------
   -- Initialize_Task_Context --
   -----------------------------

   procedure Initialize_Task_Context
     (T : in out Task_Object)
   is
      type Stack_Array is array (1 .. 8) of UInt32
        with Pack, Size => 8 * 32;

      Context : Stack_Array
        with Address => T.Stack (T.Stack'Last)'Address + 1 - 8 * 32;

   begin
      --  xPSR
      Context (8) := 2**24; -- Set the thumb bit

      --  PC
      Context (7) := UInt32 (To_Integer (T.Proc.all'Address));

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

   -------------
   -- Set_PSP --
   -------------

   procedure Set_PSP
     (Addr : Process_Stack_Pointer)
   is
   begin
--        Ada.Text_IO.Put_Line ("Set_PSP:" & Image (Addr));

      Asm ("msr psp, %0",
           Inputs  => Process_Stack_Pointer'Asm_Input ("r", Addr),
           Volatile => True);
   end Set_PSP;


   ---------
   -- PSP --
   ---------

   function PSP
     return Process_Stack_Pointer
   is
      Ret : Process_Stack_Pointer;
   begin
      Asm ("mrs %0, psp",
           Outputs  => Process_Stack_Pointer'Asm_Output ("=r", Ret),
           Volatile => True);

--        Ada.Text_IO.Put_Line ("Get_PSP:" & Image (Ret));
      return Ret;
   end PSP;

   ------------------
   -- Jump_In_Task --
   ------------------

   procedure Jump_In_Task
     (T : Task_Object)
   is
   begin
--        Ada.Text_IO.Put_Line ("Starting task at PSP:" & Image (T.Stack_Pointer));

      Set_PSP (T.Stack_Pointer);

      --  Return to Thread mode, exception return uses non-floating-point
      --  state from the PSP and execution uses PSP after return.
      Asm ("mov lr, 0xFFFFFFFD" & ASCII.LF &
           "bx lr",
           Volatile => True);
      loop
         null;
      end loop;
   end Jump_In_Task;

   --------------
   -- Register --
   --------------

   procedure Register
     (ID   : Task_ID;
      Name : String)
   is
      T : constant Task_Object_Access := Task_Object_Access (ID);
   begin
      T.Status := Ready;

      T.Name (Task_Name'First .. Task_Name'First + Name'Length - 1) := Name;

      Initialize_Task_Context (T.all);
      Insert (Task_ID (T));
   end Register;

   -----------
   -- Start --
   -----------

   procedure Start is
      First_Context : System.Address := Null_Address;
   begin
      if Ready_Tasks = null then
         raise Program_Error with "No task to run";
      end if;

      Running_Task := Ready_Tasks;
      Jump_In_Task (Running_Task.all);
   end Start;

   ------------------
   -- Current_Task --
   ------------------

   function Current_Task
     return Task_ID
   is (Task_ID (Running_Task));

   --------------------------
   -- Current_Task_Context --
   --------------------------

   function Current_Task_Context
     return System.Address
   is
   begin
      return Running_Task.Context'Address;
   end Current_Task_Context;

   -----------------
   -- Task_To_Run --
   -----------------

   function Task_To_Run
     return Task_ID
   is (Task_ID (Ready_Tasks));


   --------------------
   -- In_Ready_Tasks --
   --------------------

   function In_Ready_Tasks
     (ID : Task_ID)
      return Boolean
   is
      T : Task_Object_Access := Ready_Tasks;
   begin
      while T /= null and then Task_ID (T) /= ID loop
         T := T.Next;
      end loop;

      return Task_ID (T) = ID;
   end In_Ready_Tasks;

   ----------------------
   -- Print_Read_Tasks --
   ----------------------

   procedure Print_Ready_Tasks
   is
      T : Task_Object_Access := Ready_Tasks;
   begin
      Ada.Text_IO.Put_Line ("Ready tasks:");
      while T /= null loop
         Ada.Text_IO.Put_Line ("   - " & Image (Task_ID (T)));
         T := T.Next;
      end loop;
   end Print_Ready_Tasks;

   -------------
   -- Extract --
   -------------

   procedure Extract
     (ID : Task_ID)
   is
   begin
      if Task_ID (Ready_Tasks) /= ID then
         raise Program_Error;
      end if;

      Ready_Tasks := Ready_Tasks.Next;
      Task_Object_Access (ID).Next := null;
   end Extract;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (ID : Task_ID)
   is
      Acc  : Task_Object_Access := Task_Object_Access(ID);
      Cur  : Task_Object_Access := Ready_Tasks;
      Prev : Task_Object_Access := null;
   begin
      Acc.Next := null;

      while Cur /= null and then Cur.Priority > Acc.Priority loop
         Prev := Cur;
         Cur := Cur.Next;
      end loop;

      Acc.Next := Cur;
      if Prev = null then
         --  Head insertion
         Ready_Tasks := Acc;
      else
         Prev.Next := Acc;
      end if;
   end Insert;

   -----------
   -- Yield --
   -----------

   procedure Yield
   is
   begin

--        Ada.Text_IO.Put_Line ("Yield Pre Task_To_Run:" & Image (Task_To_Run));
--        Print_Ready_Tasks;

      Extract (Current_Task);
      Insert (Current_Task);

      Current_Task.Status := Ready;

--        Ada.Text_IO.Put_Line ("Yield Post Task_To_Run:" & Image (Task_To_Run));
--        Print_Ready_Tasks;

      if Context_Switch_Needed then
         Trigger_Context_Switch;
      end if;
   end Yield;

   ------------
   -- Resume --
   ------------

   procedure Resume
     (ID : Task_ID)
   is
   begin
      Task_Object_Access (ID).Status := Ready;
      Insert (ID);

      if Context_Switch_Needed then
         Trigger_Context_Switch;
      end if;
   end Resume;

   -------------
   -- Suspend --
   -------------

   procedure Suspend
     (Reason : Suspend_Reason)
   is
   begin
      Extract (Current_Task);
      Current_Task.Status := (case Reason is
                                 when Alarm     => Suspended_Alarm,
                                 when Semaphore => Suspended_Semaphore);
   end Suspend;

   ---------------------------
   -- Context_Switch_Needed --
   ---------------------------

   function Context_Switch_Needed
     return Boolean
   is (Task_To_Run /= Current_Task);

   ----------------------------
   -- Trigger_Context_Switch --
   ----------------------------

   procedure Trigger_Context_Switch
   is
   begin
      SCB_Periph.ICSR.PENDSVSET := True;
   end Trigger_Context_Switch;

   ----------------------------
   -- Context_Switch_Handler --
   ----------------------------

   procedure Context_Switch_Handler is
   begin

      Asm (Template =>
             "push {lr}" & ASCII.LF &
             "bl current_task_context" & ASCII.LF &
             "stm  r0, {r4-r12}", -- Save extra context
           Volatile => True);

--        Ada.Text_IO.Put_Line ("Context switch handler");

      SCB_Periph.ICSR.PENDSVCLR := True;

      Running_Task.Stack_Pointer := PSP;

      Set_PSP (Ready_Tasks.Stack_Pointer);

      Running_Task := Ready_Tasks;
      Running_Task.Status := Running;

      Asm (Template =>
             "bl current_task_context" & ASCII.LF &
             "ldm  r0, {r4-r12}"      & ASCII.LF & -- Load extra context
             "pop {pc}",
           Volatile => True);
   end Context_Switch_Handler;

   --------
   -- ID --
   --------

   function ID (T : Task_Object_Access) return Task_ID
   is (Task_ID (T));

   -----------
   -- Image --
   -----------

   function Image (ID : Task_ID) return String
   is
      T : constant Task_Object_Access := Task_Object_Access (ID);
   begin
      return To_Integer (T.all'Address)'Img;
   end Image;

end AGATE.Tasking;
