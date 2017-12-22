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
with HAL;                     use HAL;
with System.Storage_Elements; use System.Storage_Elements;
with Cortex_M_SVD.SCB;        use Cortex_M_SVD.SCB;
with Cortex_M_SVD.NVIC;       use Cortex_M_SVD.NVIC;

package body AGATE.Interrupts is

   Handlers_Table : array (Interrupt_ID) of Interrupt_Handler
     := (others => null);
   Priorities_Table : array (Interrupt_ID) of Interrupt_Priority
     := (others => Interrupt_Priority'First);

   procedure IRQ_Handler;
   pragma Export (C, IRQ_Handler, "__gnat_irq_trap");

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
   is
      Vector : UInt32;
      pragma Import (C, Vector, "__vectors");

      Vector_Address : constant UInt32 := UInt32 (To_Integer(Vector'Address));
   begin
      SCB_Periph.VTOR := Vector_Address;
   end Initialize;

   --------------
   -- Register --
   --------------

   procedure Register
     (Handler  : Interrupt_Handler;
      ID       : Interrupt_ID;
      Priority : Interrupt_Priority)
   is
   begin
      Handlers_Table (ID) := Handler;
      Priorities_Table (ID) := Priority;
   end Register;

   ------------
   -- Enable --
   ------------

   procedure Enable (ID : Interrupt_ID)
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

   -----------------
   -- IRQ_Handler --
   -----------------

   procedure IRQ_Handler
   is
      ID : constant Interrupt_ID :=
        Interrupt_ID (Integer (SCB_Periph.ICSR.VECTACTIVE) - 16);
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
end AGATE.Interrupts;
