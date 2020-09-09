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

with System;               use System;
with AGATE.Traces;
with AGATE.Scheduler.Context_Switch;
with AGATE.Arch;           use AGATE.Arch;
with AGATE.Timer;

package body AGATE.Scheduler is

   Alarm_List : Task_Object_Access := null;

   Idle_Stack     : aliased Task_Stack := (1 .. 512 => 0);
   Idle_Sec_Stack : aliased Task_Sec_Stack := (1 .. 0 => 0);
   Idle_Heap      : aliased Task_Heap := (1 .. 0 => 0);

   Idle_Task : aliased Task_Object
     (Proc      =>  AGATE.Arch.Idle_Procedure'Access,
      Base_Prio => -1,
      Stack     => Idle_Stack'Access,
      Sec_Stack => Idle_Sec_Stack'Access,
      Heap      => Idle_Heap'Access);

   function In_Ready_Tasks (ID : Task_ID) return Boolean;

   procedure Extract (ID : Task_ID);

   procedure Insert (ID : Task_ID)
     with Pre => not In_Ready_Tasks (ID);

   procedure Insert_Alarm
     (T : Task_Object_Access);

   procedure Update_Alarm_Time;

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
      T.Current_Prio := T.Base_Prio;

      Initialize_Task_Context (ID);

      Insert (Task_ID (T));

      Traces.Register (Task_ID (T), T.Name);
   end Register;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      if Ready_Tasks = null then
         raise Program_Error with "No task to run";
      end if;

      Register (Idle_Task'Access, "idle");

      Running_Task := Ready_Tasks;
      Jump_In_Task (Task_ID (Running_Task));
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
      return Running_Task.Context (Running_Task.Context'First)'Address;
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
      --  Ada.Text_IO.Put_Line ("Ready tasks:");
      while T /= null loop
         --  Ada.Text_IO.Put_Line ("   - " & Image (Task_ID (T)));
         T := T.Next;
      end loop;
   end Print_Ready_Tasks;

   -------------
   -- Extract --
   -------------

   procedure Extract
     (ID : Task_ID)
   is
      Prev, Curr : Task_Object_Access;
   begin
      if Task_ID (Ready_Tasks) = ID then

         --  Extract head

         Ready_Tasks := Ready_Tasks.Next;
         ID.Next := null;

      else

         --  Extract from inside the list

         Prev := null;
         Curr := Ready_Tasks;

         while Curr /= null and then Task_ID (Curr) /= ID loop
            Prev := Curr;
            Curr := Curr.Next;
         end loop;

         if Task_ID (Curr) = ID then
            Prev.Next := Curr.Next;
            ID.Next := null;
         end if;
      end if;
   end Extract;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (ID : Task_ID)
   is
      Acc  : constant Task_Object_Access := Task_Object_Access (ID);
      Cur  : Task_Object_Access := Ready_Tasks;
      Prev : Task_Object_Access := null;
   begin
      Acc.Next := null;

      while Cur /= null and then Cur.Current_Prio >= Acc.Current_Prio loop
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

      Extract (Current_Task);
      Insert (Current_Task);

      Current_Task.Status := Ready;

      if Context_Switch_Needed then
         Context_Switch.Switch;
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

      Traces.Resume (ID);

      if Context_Switch_Needed then
         Context_Switch.Switch;
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
                                 when Semaphore => Suspended_Semaphore,
                                 when Mutex     => Suspended_Mutex);

      Traces.Suspend (Current_Task);
   end Suspend;

   -----------
   -- Fault --
   -----------

   procedure Fault is
   begin
      Extract (Current_Task);
      Current_Task.Status := Fault;
      Traces.Fault (Current_Task);
   end Fault;

   ---------------------
   -- Change_Priority --
   ---------------------

   procedure Change_Priority
     (New_Prio : Task_Priority)
   is
      T : constant Task_ID := Current_Task;
   begin
      if New_Prio < T.Base_Prio then
         raise Program_Error with
           "Cannot set priority below the task base priority";
      end if;

      Traces.Change_Priority (T, New_Prio);

      if New_Prio >= T.Current_Prio then

         --  The current task already have the highest priority of the ready
         --  task, by the properties of the FIFO within priorities scheduling.
         --  So the current task position in the ready tasks list won't change,
         --  We don't have to re-insert it.
         T.Current_Prio := New_Prio;
      else

         Extract (T);
         T.Current_Prio := New_Prio;
         Insert (T);
      end if;
   end Change_Priority;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until
     (Wake_Up_Time : Time)
   is
      T   : constant Task_Object_Access := Task_Object_Access (Current_Task);
      Now : constant Time := AGATE.Timer.Clock;
   begin
      if Wake_Up_Time <= Now then
         Scheduler.Yield;
      else
         Scheduler.Suspend (Alarm);
         T.Alarm_Time := Wake_Up_Time;
         Insert_Alarm (T);

         if Context_Switch_Needed then
            Context_Switch.Switch;
         end if;
      end if;
   end Delay_Until;

   ------------------
   -- Insert_Alarm --
   ------------------

   procedure Insert_Alarm
     (T : Task_Object_Access)
   is
      Cur : Task_Object_Access := Alarm_List;
      Prev : Task_Object_Access := null;
   begin

      while Cur /= null and then Cur.Alarm_Time < T.Alarm_Time loop
         Prev := Cur;
         Cur := Cur.Next;
      end loop;

      if Prev = null then
         --  Head insertion
         T.Next := Alarm_List;
         Alarm_List := T;
      else
         Prev.Next := T;
         T.Next := Cur;
      end if;

      Update_Alarm_Time;
   end Insert_Alarm;

   ---------------------------
   -- Wakeup_Expired_Alarms --
   ---------------------------

   procedure Wakeup_Expired_Alarms
   is
      Now : constant Time := AGATE.Timer.Clock;
      T   : Task_Object_Access := Alarm_List;
   begin
      while Alarm_List /= null and then Alarm_List.Alarm_Time <= Now loop
         T := Alarm_List;
         Alarm_List := T.Next;
         Scheduler.Resume (Task_ID (T));
      end loop;

      Update_Alarm_Time;
   end Wakeup_Expired_Alarms;

   -----------------------
   -- Update_Alarm_Time --
   -----------------------

   procedure Update_Alarm_Time
   is
   begin
      if Alarm_List = null then
         AGATE.Timer.Set_Alarm (Time'Last);
      else
         AGATE.Timer.Set_Alarm (Alarm_List.Alarm_Time);
      end if;
   end Update_Alarm_Time;

   ---------------------------
   -- Context_Switch_Needed --
   ---------------------------

   function Context_Switch_Needed
     return Boolean
   is (Task_To_Run /= Current_Task);

   -----------------------
   -- Do_Context_Switch --
   -----------------------

   procedure Do_Context_Switch renames AGATE.Scheduler.Context_Switch.Switch;

end AGATE.Scheduler;
