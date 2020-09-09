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

with System.Machine_Code;  use System.Machine_Code;

package body AGATE.Arch.ArmvX_m is

   -------------
   -- Set_PSP --
   -------------

   procedure Set_PSP
     (Addr : Process_Stack_Pointer)
   is
   begin
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
      return Ret;
   end PSP;

   -------------
   -- Set_MSP --
   -------------

   procedure Set_MSP
     (Addr : Process_Stack_Pointer)
   is
   begin
      Asm ("msr msp, %0",
           Inputs  => Process_Stack_Pointer'Asm_Input ("r", Addr),
           Volatile => True);
   end Set_MSP;

   ---------
   -- MSP --
   ---------

   function MSP
     return Process_Stack_Pointer
   is
      Ret : Process_Stack_Pointer;
   begin
      Asm ("mrs %0, msp",
           Outputs  => Process_Stack_Pointer'Asm_Output ("=r", Ret),
           Volatile => True);
      return Ret;
   end MSP;

   -----------------
   -- Set_Control --
   -----------------

   procedure Set_Control (Val : Word) is
   begin
      Asm ("msr control, %0",
           Inputs  => Word'Asm_Input ("r", Val),
           Volatile => True);
   end Set_Control;

   -------------
   -- Control --
   -------------

   function Control return Word is
      Ret : Word;
   begin
      Asm ("mrs %0, control",
           Outputs  => Word'Asm_Output ("=r", Ret),
           Volatile => True);
      return Ret;
   end Control;

   -------------------
   -- Enable_Faults --
   -------------------

   procedure Enable_Faults is
   begin
      Asm ("cpsie f",
           Volatile => True);
   end Enable_Faults;

   --------------------
   -- Disable_Faults --
   --------------------

   procedure Disable_Faults is
   begin
      Asm ("cpsid f",
           Volatile => True);
   end Disable_Faults;

   ----------------
   -- Enable_IRQ --
   ----------------

   procedure Enable_IRQ is
   begin
      Asm ("cpsie i",
           Volatile => True);
   end Enable_IRQ;

   -----------------
   -- Disable_IRQ --
   -----------------

   procedure Disable_IRQ is
   begin
      Asm ("cpsid i",
           Volatile => True);
   end Disable_IRQ;

   -----------------------------
   -- Set_Thread_Unprivileged --
   -----------------------------

   procedure Set_Thread_Unprivileged is
   begin
      Set_Control (Control or 1);
   end Set_Thread_Unprivileged;

   ----------------------------
   -- Set_Thread_Mode_On_PSP --
   ----------------------------

   procedure Set_Thread_Mode_On_PSP is
   begin
      Set_Control (Control or 2);
   end Set_Thread_Mode_On_PSP;

end AGATE.Arch.ArmvX_m;
