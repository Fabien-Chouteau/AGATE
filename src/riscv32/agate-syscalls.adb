------------------------------------------------------------------------------
--                                                                          --
--                Copyright (C) 2017-2018, Fabien Chouteau                  --
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


with System.Machine_Code;     use System.Machine_Code;

with System.Storage_Elements; use System.Storage_Elements;

with AGATE.Traps;             use AGATE.Traps;
with AGATE.Timer;
with AGATE.Tasking;
with AGATE.Traces;
with AGATE_Arch_Parameters;   use AGATE_Arch_Parameters;

package body AGATE.SysCalls is

   SysCall_Handler_Table : array (Syscall_ID) of Syscall_Handler
     := (others => null);

   procedure Initialize;
   procedure Syscall_Trap_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
   begin
      Register (Syscall_Trap_Handler'Access, -4, 0);
      Register (Syscall_Trap_Handler'Access, -3, 0);
      Register (Syscall_Trap_Handler'Access, -2, 0);
      Register (Syscall_Trap_Handler'Access, -1, 0);
   end Initialize;

   --------------------------
   -- Syscall_Trap_Handler --
   --------------------------

   procedure Syscall_Trap_Handler is
      ID_Word , Arg1, Arg2, Arg3 : Word;
      ID : Syscall_ID;
      Ctx : Task_Context renames Tasking.Current_Task.Context;
      Ret : UInt64;
   begin
      --  Increase the task PC to not execute the ecall instruction again
      Ctx (Ctx_PC_Index) := Ctx (Ctx_PC_Index) + 4;

      ID_Word := Ctx (Ctx_A0_Index);
      Arg1    := Ctx (Ctx_A1_Index);
      Arg2    := Ctx (Ctx_A2_Index);
      Arg3    := Ctx (Ctx_A3_Index);

      if ID_Word in
        Syscall_ID'Pos (Syscall_ID'First) .. Syscall_ID'Pos (Syscall_ID'Last)
      then
         ID := Syscall_ID'Val (ID_Word);
         if SysCall_Handler_Table (ID) /= null then
            Ret := SysCall_Handler_Table (ID) (Arg1, Arg2, Arg3);
            Ctx (Ctx_A0_Index) := Word (Ret and 16#FFFF_FFFF#);
            Ctx (Ctx_A1_Index) := Word (Shift_Right (Ret, 32) and 16#FFFF_FFFF#);
         else
            raise Program_Error with "No handler for Syscall";
         end if;
      else
         raise Program_Error with "Invalid syscall ID";
      end if;
   end Syscall_Trap_Handler;

   ----------
   -- Call --
   ----------

   function Call (ID               : Syscall_ID;
                  Arg1, Arg2, Arg3 : Word := 0)
                  return UInt64
   is
      Ret_Lo : Word;
      Ret_Hi : Word;
   begin
      Asm ("mv a0, %2" & ASCII.LF &
           "mv a1, %3" & ASCII.LF &
           "mv a2, %4" & ASCII.LF &
           "mv a3, %5" & ASCII.LF &
           "ecall" & ASCII.LF &
           "mv %0, a0" & ASCII.LF &
           "mv %1, a1" & ASCII.LF,
           Outputs  => (Word'Asm_Output ("=r", Ret_Lo),
                        Word'Asm_Output ("=r", Ret_Hi)),
           Inputs   => (Syscall_ID'Asm_Input ("r", ID),
                        Word'Asm_Input ("r", Arg1),
                        Word'Asm_Input ("r", Arg2),
                        Word'Asm_Input ("r", Arg3)),
           Clobber  => "a0, a1, a2, a3",
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

begin
   Initialize;
end AGATE.SysCalls;
