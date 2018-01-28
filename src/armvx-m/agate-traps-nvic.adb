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

with Ada.Text_IO;         use Ada.Text_IO;
with System.Machine_Code; use System.Machine_Code;
with Cortex_M_SVD.SCB;    use Cortex_M_SVD.SCB;
with Cortex_M_SVD.NVIC;   use Cortex_M_SVD.NVIC;

package body AGATE.Traps is

   Handlers_Table : array (Trap_ID) of Trap_Handler
     := (others => null);
   Priorities_Table : array (Trap_ID) of Trap_Priority
     := (others => Trap_Priority'First);
   pragma Unreferenced (Priorities_Table);

   procedure IRQ_Handler;
   pragma Export (C, IRQ_Handler, "__gnat_irq_trap");

   procedure Initialize;
   procedure Hard_Fault_Handler;

   ------------------------
   -- Hard_Fault_Handler --
   ------------------------

   procedure Hard_Fault_Handler
   is

      function PSP return System.Address;

      function PSP
        return System.Address
      is
         Ret : System.Address;
      begin
         Asm ("mrs %0, psp",
              Outputs  => System.Address'Asm_Output ("=r", Ret),
              Volatile => True);

         return Ret;
      end PSP;

      type Stack_Array is array (1 .. 8) of Word
        with Pack, Size => 8 * 32;

      Context : Stack_Array
        with Address => PSP;

      SCB : SCB_Peripheral renames SCB_Periph;
   begin
      Put_Line ("In HardFault");
      if SCB_Periph.HFSR.FORCED then
         Put_Line ("Forced HardFault");

         Put_Line ("Vector table read         : " & SCB.HFSR.VECTTBL'Img);

         Put_Line ("MemManage:");
         Put_Line (" - Instruction access     : " & SCB.MMSR.IACCVIOL'Img);
         Put_Line (" - Data access            : " & SCB.MMSR.DACCVIOL'Img);
         Put_Line (" - Unstacking from excp   : " & SCB.MMSR.MUNSTKERR'Img);
         Put_Line (" - Stacking for excp      : " & SCB.MMSR.MSTKERR'Img);
         Put_Line (" - FPU Lazy state preserv : " & SCB.MMSR.MLSPERR'Img);

         if SCB.MMSR.MMARVALID then
            Put_Line ("-> address : " & SCB.MMAR'Img);
         end if;

         Put_Line ("BusFault:");
         Put_Line (" - Instruction bus error  : " & SCB.BFSR.IBUSERR'Img);
         Put_Line (" - Precise data bus error : " & SCB.BFSR.PRECISERR'Img);
         Put_Line (" - Imprecise data bus err : " & SCB.BFSR.IMPRECISERR'Img);
         Put_Line (" - Unstacking from excp   : " & SCB.BFSR.UNSTKERR'Img);
         Put_Line (" - Stacking for excp      : " & SCB.BFSR.STKERR'Img);
         Put_Line (" - FPU Lazy state preserv : " & SCB.BFSR.LSPERR'Img);

         if SCB.BFSR.BFARVALID then
            Put_Line ("-> address : " & SCB.BFAR'Img);
         end if;

         Put_Line ("UsageFault:");
         Put_Line (" - Undefined instruction  : " & SCB.UFSR.UNDEFINSTR'Img);
         Put_Line (" - Invalid state          : " & SCB.UFSR.INVSTATE'Img);
         Put_Line (" - Invalid PC load        : " & SCB.UFSR.INVPC'Img);
         Put_Line (" - No co-processor        : " & SCB.UFSR.NOCP'Img);
         Put_Line (" - Unaligned access       : " & SCB.UFSR.UNALIGNED'Img);
         Put_Line (" - Division by zero       : " & SCB.UFSR.DIVBYZERO'Img);

         Put_Line ("R0 : " & Hex (UInt32 (Context (1))));
         Put_Line ("R1 : " & Hex (UInt32 (Context (2))));
         Put_Line ("R2 : " & Hex (UInt32 (Context (3))));
         Put_Line ("R3 : " & Hex (UInt32 (Context (4))));
         Put_Line ("R12: " & Hex (UInt32 (Context (5))));
         Put_Line ("LR : " & Hex (UInt32 (Context (6))));
         Put_Line ("PC : " & Hex (UInt32 (Context (7))));
         Put_Line ("PSR: " & Hex (UInt32 (Context (8))));
         loop
            null;
         end loop;
      end if;
   end Hard_Fault_Handler;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      Vector : Word;
      pragma Import (C, Vector, "__vectors");

      Vector_Address : constant Word := Word (To_Integer (Vector'Address));
   begin
      SCB_Periph.VTOR := UInt32 (Vector_Address);
      Register (Hard_Fault_Handler'Access, -13, 0);
   end Initialize;

   --------------
   -- Register --
   --------------

   procedure Register
     (Handler  : Trap_Handler;
      ID       : Trap_ID;
      Priority : Trap_Priority)
   is
   begin
      Handlers_Table (ID) := Handler;
      Priorities_Table (ID) := Priority;
   end Register;

   ------------
   -- Enable --
   ------------

   procedure Enable (ID : Trap_ID)
   is
   begin
      if ID >= 0 then
         declare
            Reg_Index : constant Natural := Natural (ID) / 32;
            Bit       : constant UInt32 := 2**(Natural (ID) mod 32);
         begin
            NVIC_Periph.NVIC_ISER (Reg_Index) := Bit;
         end;
      end if;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (ID : Trap_ID)
   is
   begin
      if ID >= 0 then
         declare
            Reg_Index : constant Natural := Natural (ID) / 32;
            Bit       : constant UInt32 := 2**(Natural (ID) mod 32);
         begin
            NVIC_Periph.NVIC_ICER (Reg_Index) := Bit;
         end;
      end if;
   end Disable;

   -----------------
   -- IRQ_Handler --
   -----------------

   procedure IRQ_Handler
   is
      ID : constant Trap_ID :=
        Trap_ID (Integer (SCB_Periph.ICSR.VECTACTIVE) - 16);
   begin
      if Handlers_Table (ID) /= null then
         Handlers_Table (ID).all;
      else
         Ada.Text_IO.Put_Line ("No handler for: " & ID'Img);
         loop
            null;
         end loop;
      end if;
   end IRQ_Handler;

begin
   Initialize;
end AGATE.Traps;
