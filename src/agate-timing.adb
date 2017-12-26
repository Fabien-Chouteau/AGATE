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

with Ada.Text_IO;

with Cortex_M_SVD.SysTick; use Cortex_M_SVD.SysTick;
with AGATE.Interrupts;     use AGATE.Interrupts;
with AGATE.Tasking;        use AGATE.Tasking;
with Cortex_M_SVD.SCB;     use Cortex_M_SVD.SCB;

package body AGATE.Timing is

   SysTick_Int_ID : constant Interrupt_ID := -1;

   Tick_Period : constant := 168_000_000 / 1_000;
   Next_Tick   : Time;

   Alarm_List : Task_Object_Access := null;

   procedure Initialize;
   procedure Timer_Handler;
   procedure Clear_Timer_Interrupt;
   procedure Wakeup_Expired_Alarms (Now : Time);
   procedure Print_Alarm_List;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      CSR : UInt32 with Volatile, Address => SysTick_Periph.CSR'Address;

   begin

      Next_Tick := Time (Tick_Period);

      Interrupts.Register (Timer_Handler'Access, SysTick_Int_ID, 0);

      SysTick_Periph.RVR.RELOAD := Tick_Period - 1;
      SysTick_Periph.CSR.ENABLE := Enable;

      Clear_Timer_Interrupt;


      --  SysTick_Periph.CSR.TICKINT := Enable;
      --  Workaround...
      CSR := CSR or 2**1;

      Enable (SysTick_Int_ID);
   end Initialize;

   ---------------------------
   -- Clear_Timer_Interrupt --
   ---------------------------

   procedure Clear_Timer_Interrupt
   is
   begin
      SCB_Periph.ICSR.PENDSTCLR := True;
   end Clear_Timer_Interrupt;

   -------------------
   -- Timer_Handler --
   -------------------

   procedure Timer_Handler
   is
   begin
      Wakeup_Expired_Alarms (Clock);
      Clear_Timer_Interrupt;
   end Timer_Handler;

   -----------
   -- Clock --
   -----------

   function Clock return Time
   is
      Ret   : Time;
      Count : UInt24;
   begin
      Count := SysTick_Periph.CVR.CURRENT;

      if SysTick_Periph.CSR.COUNTFLAG then
         Ret := Next_Tick;
         Next_Tick := Next_Tick + Tick_Period;
      else
         Ret := Next_Tick - Time (Count);
      end if;

      return Time (Ret);
   end Clock;

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
   end Insert_Alarm;

   ---------------------------
   -- Wakeup_Expired_Alarms --
   ---------------------------

   procedure Wakeup_Expired_Alarms
     (Now : Time)
   is
      T : Task_Object_Access := Alarm_List;
   begin
--        Ada.Text_IO.Put_Line ("Wakeup_Expired_Alarms");
      while Alarm_List /= null and then Alarm_List.Alarm_Time <= Now loop
         T := Alarm_List;
         Alarm_List := T.Next;
         Tasking.Resume (Task_ID (T));
      end loop;
   end Wakeup_Expired_Alarms;

   ----------------------
   -- Print_Alarm_List --
   ----------------------

   procedure Print_Alarm_List
   is
      T : Task_Object_Access := Alarm_List;
   begin
      Ada.Text_IO.Put_Line ("Alarm list:");
      if T = null then
         Ada.Text_IO.Put_Line ("   No alarms");
      else
         while T /= null loop
            Ada.Text_IO.Put_Line ("   - " & T.Alarm_Time'Img &
                                    " - " & Image (Task_ID (T)));
            T := T.Next;
         end loop;
      end if;
   end Print_Alarm_List;

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until
     (Wake_Up_Time : Time)
   is
      T   : Task_Object_Access := Task_Object_Access (Current_Task);
      Now : Time := Clock;
   begin
      if Wake_Up_Time <= Now then
         Tasking.Yield;
      else
         Tasking.Suspend (Alarm);
         T.Alarm_Time := Wake_Up_Time;
         Insert_Alarm (T);

         if Context_Switch_Needed then
            Trigger_Context_Switch;
         end if;
      end if;
   end Delay_Until;

begin
   Initialize;
end AGATE.Timing;
