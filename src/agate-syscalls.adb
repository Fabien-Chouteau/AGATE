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

with System.Machine_Code;     use System.Machine_Code;

pragma Warnings (Off, "is an internal GNAT unit");
with System.Machine_Reset;
pragma Warnings (On, "is an internal GNAT unit");

with System.Storage_Elements; use System.Storage_Elements;

with AGATE.Interrupts;        use AGATE.Interrupts;
with AGATE.Timing;
with AGATE.Tasking;

package body AGATE.SysCalls is

   SVCall_Interrupt_ID : constant Interrupt_ID := -5;

   type Syscall_ID is (Yield, Clock, Delay_Until, Sem_Signal, Sem_Wait,
                       Shutdown);

   type Syscall_Handler is access
     function (Arg1, Arg2, Arg3 : UInt32) return UInt32;

   SysCall_Handler_Table : array (Syscall_ID) of Syscall_Handler
     := (others => null);

   function Trig (ID               : Syscall_ID;
                  Arg1, Arg2, Arg3 : UInt32 := 0)
                  return UInt32;

   procedure Trig (ID               : Syscall_ID;
                   Arg1, Arg2, Arg3 : UInt32 := 0);

   procedure Initialize;
   procedure SVCall_Handler;

   function Do_Yield (Arg1, Arg2, Arg3 : UInt32) return UInt32;
   function Do_Clock (Arg1, Arg2, Arg3 : UInt32) return UInt32;
   function Do_Delay_Until (Arg1, Arg2, Arg3 : UInt32) return UInt32;
   function Do_Sem_Wait (Arg1, Arg2, Arg3 : UInt32) return UInt32;
   function Do_Sem_Signal (Arg1, Arg2, Arg3 : UInt32) return UInt32;
   function Do_Shutdown (Arg1, Arg2, Arg3 : UInt32) return UInt32;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
   begin
      Interrupts.Register (SVCall_Handler'Access,
                           SVCall_Interrupt_ID,
                           0);

      SysCall_Handler_Table (Yield) := Do_Yield'Access;
      SysCall_Handler_Table (Clock) := Do_Clock'Access;
      SysCall_Handler_Table (Delay_Until) := Do_Delay_Until'Access;
      SysCall_Handler_Table (Sem_Signal) := Do_Sem_Signal'Access;
      SysCall_Handler_Table (Sem_Wait) := Do_Sem_Wait'Access;
      SysCall_Handler_Table (Shutdown) := Do_Shutdown'Access;
   end Initialize;

   --------------
   -- Do_Yield --
   --------------

   function Do_Yield
     (Arg1, Arg2, Arg3 : UInt32)
      return UInt32
   is
   begin
--        Ada.Text_IO.Put_Line ("Do_Yield!!!");
      Tasking.Yield;
      return 0;
   end Do_Yield;

   --------------
   -- Do_Clock --
   --------------

   function Do_Clock
     (Arg1, Arg2, Arg3 : UInt32)
      return UInt32
   is
   begin
--        Ada.Text_IO.Put_Line ("Do_Clock");
      return UInt32 (Timing.Clock);
   end Do_Clock;

   --------------------
   -- Do_Delay_Until --
   --------------------

   function Do_Delay_Until
     (Arg1, Arg2, Arg3 : UInt32)
      return UInt32
   is
   begin
--        Ada.Text_IO.Put_Line ("Do_Delay_Until!!!");
      Timing.Delay_Until (Time (Arg1));
      return 0;
   end Do_Delay_Until;

   -----------------
   -- Do_Sem_Wait --
   -----------------

   function Do_Sem_Wait
     (Arg1, Arg2, Arg3 : UInt32)
      return UInt32
   is
   begin
      Semaphores.Wait_For_Signal (To_ID (Arg1));
      return 0;
   end Do_Sem_Wait;

   -------------------
   -- Do_Sem_Signal --
   -------------------

   function Do_Sem_Signal
     (Arg1, Arg2, Arg3 : UInt32)
      return UInt32
   is
   begin
      Semaphores.Signal (To_ID (Arg1));
      return 0;
   end Do_Sem_Signal;

   -----------------
   -- Do_Shutdown --
   -----------------

   function Do_Shutdown
     (Arg1, Arg2, Arg3 : UInt32)
      return UInt32
   is
   begin
      System.Machine_Reset.Stop;
      return 0;
   end Do_Shutdown;

   --------------------
   -- SVCall_Handler --
   --------------------

   procedure SVCall_Handler
   is
      type Stack_Array is array (1 .. 4) of UInt32
        with Pack, Size => 4 * 32;

      PSP : System.Address;
      Ret : UInt32;
      ID  : Syscall_ID;
   begin
      Asm ("mrs %0, psp",
           Outputs  => System.Address'Asm_Output ("=r", PSP),
           Volatile => True);

      if PSP mod Stack_Array'Alignment /= 0 then
         Ada.Text_IO.Put_Line ("Invalid PSP address");
      else
         declare

            Args : Stack_Array
              with Address => PSP;
         begin
--              Ada.Text_IO.Put_Line ("SVCall PSP:" & To_Integer (PSP)'Img);
--              for Index in Args'Range loop
--                 Ada.Text_IO.Put_Line ("SVCall Args (" & Index'Img & ") =>" &
--                                         Args (Index)'Img);
--              end loop;

            if Args (1) in
              Syscall_ID'Pos (Syscall_ID'First) .. Syscall_ID'Pos (Syscall_ID'Last)
            then
               ID := Syscall_ID'Val (Args (1));
               if SysCall_Handler_Table (ID) /= null then
                  Ret := SysCall_Handler_Table (ID) (Args (2),
                                                     Args (3),
                                                     Args (4));
               else
                  raise Program_Error with "No handler for Syscall";
               end if;
            else
               raise Program_Error with "Invalid syscall ID";
            end if;
         end;
      end if;

      Asm ("mrs r1, psp"  & ASCII.LF &
           "str %0, [r1]",
           Inputs   => UInt32'Asm_Input ("r", Ret),
           Volatile => True,
           Clobber  => "r1");
   end SVCall_Handler;

   ----------
   -- Trig --
   ----------

   function Trig (ID               : Syscall_ID;
                  Arg1, Arg2, Arg3 : UInt32 := 0)
                  return UInt32
   is
      Ret : UInt32;
   begin
      Asm ("mov r0, %1" & ASCII.LF &
           "mov r1, %2" & ASCII.LF &
           "mov r2, %3" & ASCII.LF &
           "mov r3, %4" & ASCII.LF &
           "svc 1" & ASCII.LF &
           "mov %0, r0",
           Outputs => UInt32'Asm_Output ("=r", Ret),
           Inputs => (Syscall_ID'Asm_Input ("r", ID),
                      UInt32'Asm_Input ("r", Arg1),
                      UInt32'Asm_Input ("r", Arg2),
                      UInt32'Asm_Input ("r", Arg3)),
           Clobber => "r0,r1",
           Volatile => True);
      return Ret;
   end Trig;

   procedure Trig (ID               : Syscall_ID;
                   Arg1, Arg2, Arg3 : UInt32 := 0)
   is
      Unref : UInt32 with Unreferenced;
   begin
      Unref := Trig (ID, Arg1, Arg2, Arg3);
   end Trig;

   -----------
   -- Yield --
   -----------

   procedure Yield
   is
   begin
        Trig (Yield);
   end Yield;

   -----------
   -- Clock --
   -----------

   function Clock return UInt32
   is (Trig (Clock));

   -----------------
   -- Delay_Until --
   -----------------

   procedure Delay_Until
     (Wakeup_Time : Time)
   is
      Unref : UInt32 with Unreferenced;
   begin
      Unref := Trig (Delay_Until, UInt32 (Wakeup_Time));
   end Delay_Until;

   ---------------------
   -- Wait_For_Signal --
   ---------------------

   procedure Wait_For_Signal
     (ID : Semaphore_ID)
   is
      Unref : UInt32 with Unreferenced;
   begin
      Unref := Trig (Sem_Wait, To_UInt32 (ID));
   end Wait_For_Signal;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (ID : Semaphore_ID)
   is
      Unref : UInt32 with Unreferenced;
   begin
      Unref := Trig (Sem_Signal, To_UInt32 (ID));
   end Signal;

   ---------------------
   -- Shutdown_System --
   ---------------------

   procedure Shutdown_System
   is
      Unref : UInt32 with Unreferenced;
   begin
      Unref := Trig (Shutdown);
   end Shutdown_System;
begin
   Initialize;
end AGATE.SysCalls;
