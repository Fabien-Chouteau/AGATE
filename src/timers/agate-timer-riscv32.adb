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

package body AGATE.Timer is

   Mtime_Lo : Word with Volatile_Full_Access, Address => Mtime_Lo_Addr;
   Mtime_Hi : Word with Volatile_Full_Access, Address => Mtime_Hi_Addr;

   Mtimecmp_Lo : Word with Volatile_Full_Access, Address => Mtimecmp_Lo_Addr;
   Mtimecmp_Hi : Word with Volatile_Full_Access, Address => Mtimecmp_Hi_Addr;

   procedure Initialize;
   procedure Timer_Handler;

   Timer_Trap_ID : constant := -17;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register (Timer_Handler'Access, Timer_Trap_ID, 0);
      Set_Alarm (Time'Last);
   end Initialize;

   -------------------
   -- Timer_Handler --
   -------------------

   procedure Timer_Handler is
   begin
      AGATE.Tasking.Wakeup_Expired_Alarms;
   end Timer_Handler;

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

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Alarm_Time : Time)
   is
   begin
      Disable (Timer_Trap_ID);

      Mtimecmp_Lo := Word (Alarm_Time and 16#FFFF_FFFF#);
      Mtimecmp_Hi := Word (Shift_Right (Alarm_Time, 32) and 16#FFFF_FFFF#);

      if Alarm_Time /= Time'Last then
         Enable (Timer_Trap_ID);
      end if;
   end Set_Alarm;

begin
   Initialize;
end AGATE.Timer;

