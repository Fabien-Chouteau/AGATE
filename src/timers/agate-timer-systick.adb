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

with Cortex_M_SVD.SysTick;  use Cortex_M_SVD.SysTick;
with AGATE.Traps;           use AGATE.Traps;
with AGATE.Scheduler;       use AGATE.Scheduler;
with Cortex_M_SVD.SCB;      use Cortex_M_SVD.SCB;
with AGATE_Arch_Parameters; use AGATE_Arch_Parameters;

package body AGATE.Timer is

   Tick_Period : constant := 168_000_000 / 1_000;
   Next_Tick   : Time;

   procedure Initialize;
   procedure Timer_Handler;
   procedure Clear_Timer_Interrupt;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      CSR : Word with Volatile, Address => SysTick_Periph.CSR'Address;

   begin

      Next_Tick := Time (Tick_Period);

      Traps.Register (Timer_Handler'Access, SysTick_Trap_ID, 0);

      SysTick_Periph.RVR.RELOAD := Tick_Period - 1;
      SysTick_Periph.CSR.ENABLE := Enable;

      Clear_Timer_Interrupt;

      --  SysTick_Periph.CSR.TICKINT := Enable;
      --  Workaround...
      CSR := CSR or 2**1;

      Enable (SysTick_Trap_ID);
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
      AGATE.Scheduler.Wakeup_Expired_Alarms;
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

      return Ret;
   end Clock;

   ---------------
   -- Set_Alarm --
   ---------------

   procedure Set_Alarm (Alarm_Time : Time)
   is
   begin
      --  There's no alarm timer in the SysTick implementation. Instead the
      --  system gets an interrupt at a given frequency (the tick).
      null;
   end Set_Alarm;

begin
   Initialize;
end AGATE.Timer;
