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

with AGATE_Arch_Parameters;        use AGATE_Arch_Parameters;
with AGATE.Traps;                  use AGATE.Traps;
with AGATE.Tasking;                use AGATE.Tasking;
with AGATE.Tasking.Context_Switch;

package body AGATE.Timing is

   Mtime_Lo : Word with Volatile_Full_Access, Address => Mtime_Lo_Addr;
   Mtime_Hi : Word with Volatile_Full_Access, Address => Mtime_Hi_Addr;

   Mtimecmp_Lo : Word with Volatile_Full_Access, Address => Mtimecmp_Lo_Addr;
   Mtimecmp_Hi : Word with Volatile_Full_Access, Address => Mtimecmp_Hi_Addr;

   Current_Compare_Time : Time;

   Alarm_List : Task_Object_Access := null;

   procedure Initialize;
   procedure Timer_Handler;
   procedure Wakeup_Expired_Alarms (Now : Time);
   procedure Update_Comparator;

   Timer_Trap_ID : constant := -17;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register (Timer_Handler'Access, Timer_Trap_ID, 0);
      Update_Comparator;
      Disable (Timer_Trap_ID);
   end Initialize;

   -------------------
   -- Timer_Handler --
   -------------------

   procedure Timer_Handler is
   begin
      Wakeup_Expired_Alarms (Clock);
   end Timer_Handler;

   -----------------------
   -- Update_Comparator --
   -----------------------

   procedure Update_Comparator is
   begin
      if Alarm_List = null then
         Current_Compare_Time := Time'Last;
         Disable (Timer_Trap_ID);

      else
         Current_Compare_Time := Alarm_List.Alarm_Time;
      end if;

      Mtimecmp_Lo := Word (Current_Compare_Time and 16#FFFF_FFFF#);
      Mtimecmp_Hi := Word (Shift_Right (Current_Compare_Time, 32) and 16#FFFF_FFFF#);

      if Current_Compare_Time /= Time'Last then
         Enable (Timer_Trap_ID);
      end if;
   end Update_Comparator;

   -----------
   -- Clock --
   -----------

   function Clock return Time
   is
      Hi, Lo : Word;
   begin
      Hi := Mtime_Hi;
      Lo := Mtime_Lo;

      if Mtime_Hi /= Hi then
         return Shift_Left (Time (Hi) + 1, 32);
      else
         return Shift_Left (Time (Hi), 32) or Time (Lo);
      end if;
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

      Update_Comparator;
   end Insert_Alarm;

   ---------------------------
   -- Wakeup_Expired_Alarms --
   ---------------------------

   procedure Wakeup_Expired_Alarms
     (Now : Time)
   is
      T : Task_Object_Access := Alarm_List;
   begin
      while Alarm_List /= null and then Alarm_List.Alarm_Time <= Now loop
         T := Alarm_List;
         Alarm_List := T.Next;
         Tasking.Resume (Task_ID (T));
      end loop;
      Update_Comparator;
   end Wakeup_Expired_Alarms;

   ----------------------
   -- Print_Alarm_List --
   ----------------------

   procedure Print_Alarm_List
   is
   begin
      null;
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
            Context_Switch.Switch;
         end if;
      end if;
   end Delay_Until;

begin
   Initialize;
end AGATE.Timing;

