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

with Ada.Text_IO;

with System.Machine_Code; use System.Machine_Code;
with AGATE.Arch.ArmvX_m;

package body AGATE.SysCalls is

   SysCall_Handler_Table : array (Syscall_ID) of Syscall_Handler
     := (others => null);

   procedure SVCall_Handler;
   pragma Export (C, SVCall_Handler, "SVC_Handler");

   --------------------
   -- SVCall_Handler --
   --------------------

   procedure SVCall_Handler
   is
      type Stack_Array is array (1 .. 4) of Word
        with Pack, Size => 4 * 32;

      SP : System.Address;

      Ret    : UInt64;
      Ret_Lo : Word;
      Ret_Hi : Word;
      ID     : Syscall_ID;
   begin

      Check_All_Stack_Canaries;

      SP := System.Address (Arch.ArmvX_m.PSP);

      if SP mod Stack_Array'Alignment /= 0 then
         Ada.Text_IO.Put_Line ("Invalid SP address");
      else
         declare

            Args : Stack_Array
              with Address => SP;
         begin
            if Args (1) in
              Syscall_ID'Pos (Syscall_ID'First) ..
              Syscall_ID'Pos (Syscall_ID'Last)
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

      Ret_Lo := Word (Ret and 16#FFFF_FFFF#);
      Ret_Hi := Word (Shift_Right (Ret, 32) and 16#FFFF_FFFF#);
      Asm ("mrs r1, psp"  & ASCII.LF &
           "str %0, [r1]" & ASCII.LF &
           "str %1, [r1, 4]",
           Inputs   => (Word'Asm_Input ("r", Ret_Lo),
                        Word'Asm_Input ("r", Ret_Hi)),
           Volatile => True,
           Clobber  => "r1");
   end SVCall_Handler;

   ----------
   -- Call --
   ----------

   function Call (ID               : Syscall_ID;
                  Arg1, Arg2, Arg3 : Word := 0)
                  return UInt64
   is
      Ret_Lo, Ret_Hi : Word;
   begin
      Asm ("mov r0, %2" & ASCII.LF &
           "mov r1, %3" & ASCII.LF &
           "mov r2, %4" & ASCII.LF &
           "mov r3, %5" & ASCII.LF &
           "svc 1"      & ASCII.LF &
           "mov %0, r0" & ASCII.LF &
           "mov %1, r1",
           Outputs => (Word'Asm_Output ("=r", Ret_Lo),
                       Word'Asm_Output ("=r", Ret_Hi)),
           Inputs => (Syscall_ID'Asm_Input ("r", ID),
                      Word'Asm_Input ("r", Arg1),
                      Word'Asm_Input ("r", Arg2),
                      Word'Asm_Input ("r", Arg3)),
           Clobber => "r0,r1,r2,r3",
           Volatile => True);
      return Shift_Left (UInt64 (Ret_Hi), 32) or UInt64 (Ret_Lo);
   end Call;

   ----------
   -- Call --
   ----------

   procedure Call (ID               : Syscall_ID;
                   Arg1, Arg2, Arg3 : Word := 0)
   is
      Unref : UInt64 with Unreferenced;
   begin
      Unref := Call (ID, Arg1, Arg2, Arg3);
   end Call;

   ---------------
   -- Registred --
   ---------------

   function Registred (ID : Syscall_ID)
                       return Boolean
   is (SysCall_Handler_Table (ID) /= null);

   --------------
   -- Register --
   --------------

   procedure Register
     (ID      : Syscall_ID;
      Handler : not null Syscall_Handler)
   is
   begin
      if not Registred (ID) then
         SysCall_Handler_Table (ID) := Handler;
      end if;
   end Register;

end AGATE.SysCalls;
