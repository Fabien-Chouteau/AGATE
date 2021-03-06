------------------------------------------------------------------------------
--                                                                          --
--                   Copyright (C) 2018, Fabien Chouteau                    --
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

pragma Warnings (Off, "is an internal GNAT unit");
with System.Machine_Reset;
pragma Warnings (On, "is an internal GNAT unit");

with AGATE.SysCalls;  use AGATE.SysCalls;
with AGATE.Scheduler;
with AGATE.Timer;
with AGATE.Traces;
with AGATE.Semaphores;
with AGATE.Mutexes;
with AGATE.Console;

package body AGATE.API is

   procedure Initialize;
   function Do_Yield (Arg1, Arg2, Arg3 : Word) return UInt64;
   function Do_Clock (Arg1, Arg2, Arg3 : Word) return UInt64;
   function Do_Delay_Until (Arg1, Arg2, Arg3 : Word) return UInt64;
   function Do_Sem_Wait (Arg1, Arg2, Arg3 : Word) return UInt64;
   function Do_Sem_Signal (Arg1, Arg2, Arg3 : Word) return UInt64;
   function Do_Shutdown (Arg1, Arg2, Arg3 : Word) return UInt64;
   function Do_Wait_Lock (Arg1, Arg2, Arg3 : Word) return UInt64;
   function Do_Try_Lock (Arg1, Arg2, Arg3 : Word) return UInt64;
   function Do_Release (Arg1, Arg2, Arg3 : Word) return UInt64;
   function Do_Print (Arg1, Arg2, Arg3 : Word) return UInt64;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register (Yield, Do_Yield'Access);
      Register (Clock, Do_Clock'Access);
      Register (Delay_Until, Do_Delay_Until'Access);
      Register (Sem_Signal, Do_Sem_Signal'Access);
      Register (Sem_Wait, Do_Sem_Wait'Access);
      Register (Shutdown, Do_Shutdown'Access);
      Register (Mutex_Wait_Lock, Do_Wait_Lock'Access);
      Register (Mutex_Try_Lock, Do_Try_Lock'Access);
      Register (Mutex_Release, Do_Release'Access);
      Register (Print, Do_Print'Access);
   end Initialize;

   -----------
   -- Start --
   -----------

   procedure Start renames Scheduler.Start;

   --------------
   -- Do_Yield --
   --------------

   function Do_Yield
     (Arg1, Arg2, Arg3 : Word)
      return UInt64
   is
      pragma Unreferenced (Arg3, Arg2, Arg1);
   begin
      AGATE.Scheduler.Yield;
      return 0;
   end Do_Yield;

   --------------
   -- Do_Clock --
   --------------

   function Do_Clock
     (Arg1, Arg2, Arg3 : Word)
      return UInt64
   is
      pragma Unreferenced (Arg3, Arg2, Arg1);
   begin
      return UInt64 (AGATE.Timer.Clock);
   end Do_Clock;

   --------------------
   -- Do_Delay_Until --
   --------------------

   function Do_Delay_Until
     (Arg1, Arg2, Arg3 : Word)
      return UInt64
   is
      pragma Unreferenced (Arg2, Arg3);
   begin
      Scheduler.Delay_Until (Time (Arg1));
      return 0;
   end Do_Delay_Until;

   -----------------
   -- Do_Sem_Wait --
   -----------------

   function Do_Sem_Wait
     (Arg1, Arg2, Arg3 : Word)
      return UInt64
   is
      pragma Unreferenced (Arg3, Arg2);
   begin
      AGATE.Semaphores.Wait_For_Signal (To_ID (Arg1));
      return 0;
   end Do_Sem_Wait;

   -------------------
   -- Do_Sem_Signal --
   -------------------

   function Do_Sem_Signal
     (Arg1, Arg2, Arg3 : Word)
      return UInt64
   is
      pragma Unreferenced (Arg3, Arg2);
   begin
      Semaphores.Signal (To_ID (Arg1));
      return 0;
   end Do_Sem_Signal;

   -----------------
   -- Do_Shutdown --
   -----------------

   function Do_Shutdown
     (Arg1, Arg2, Arg3 : Word)
      return UInt64
   is
      pragma Unreferenced (Arg3, Arg2, Arg1);
   begin
      Traces.Shutdown;
      System.Machine_Reset.Stop;
      return 0;
   end Do_Shutdown;

   ------------------
   -- Do_Wait_Lock --
   ------------------

   function Do_Wait_Lock
     (Arg1, Arg2, Arg3 : Word)
      return UInt64
   is
      pragma Unreferenced (Arg3, Arg2);
   begin
      AGATE.Mutexes.Wait_Lock (To_ID (Arg1));
      return 0;
   end Do_Wait_Lock;

   -----------------
   -- Do_Try_Lock --
   -----------------

   function Do_Try_Lock
     (Arg1, Arg2, Arg3 : Word)
      return UInt64
   is
      pragma Unreferenced (Arg3, Arg2);
   begin
      return (if Mutexes.Try_Lock (To_ID (Arg1)) then 1 else 0);
   end Do_Try_Lock;

   ----------------
   -- Do_Release --
   ----------------

   function Do_Release
     (Arg1, Arg2, Arg3 : Word)
      return UInt64
   is
      pragma Unreferenced (Arg3, Arg2);
   begin
      Mutexes.Release (To_ID (Arg1));
      return 0;
   end Do_Release;

   --------------
   -- Do_Print --
   --------------

   function Do_Print
     (Arg1, Arg2, Arg3 : Word)
      return UInt64
   is
      pragma Unreferenced (Arg3);
      Addr : constant System.Address := To_Address (Integer_Address (Arg1));
      Len  : Word renames Arg2;
      Str  : array (1 .. Len) of Character with Address => Addr;
   begin
      for C of Str loop
         AGATE.Console.Print (C);
      end loop;
      return 0;
   end Do_Print;

   -----------
   -- Yield --
   -----------

   procedure Yield
   is
   begin
      Call (Yield);
   end Yield;

   -----------
   -- Clock --
   -----------

   function Clock return Time
   is (Time (Call (Clock)));

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until
     (Wakeup_Time : Time)
   is
      Unref : UInt64 with Unreferenced;
   begin
      Unref := Call (Delay_Until, Word (Wakeup_Time));
   end Delay_Until;

   ---------------------
   -- Wait_For_Signal --
   ---------------------

   procedure Wait_For_Signal
     (ID : Semaphore_ID)
   is
   begin
      Call (Sem_Wait, To_Word (ID));
   end Wait_For_Signal;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (ID : Semaphore_ID)
   is
   begin
      Call (Sem_Signal, To_Word (ID));
   end Signal;

   ---------------
   -- Wait_Lock --
   ---------------

   procedure Wait_Lock
     (ID : Mutex_ID)
   is
   begin
      Call (Mutex_Wait_Lock, To_Word (ID));
   end Wait_Lock;

   --------------
   -- Try_Lock --
   --------------

   function Try_Lock
     (ID : Mutex_ID)
      return Boolean
   is
      Ret : UInt64;
   begin
      Ret := Call (Mutex_Try_Lock, To_Word (ID));
      return Ret /= 0;
   end Try_Lock;
   -------------
   -- Release --
   -------------

   procedure Release
     (ID : Mutex_ID)
   is
   begin
      Call (Mutex_Release, To_Word (ID));
   end Release;

   ---------------------
   -- Shutdown_System --
   ---------------------

   procedure Shutdown_System
   is
   begin
      Call (Shutdown);
   end Shutdown_System;

   -----------
   -- Print --
   -----------

   procedure Print
     (Str : String)
   is
   begin
      Call (Print,
            Word (To_Integer (Str (Str'First)'Address)),
            Word (Str'Length));
   end Print;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Str : String) is
   begin
      Print (Str & ASCII.CR & ASCII.LF);
   end Print_Line;

begin
   Initialize;
end AGATE.API;
