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

with AGATE.Arch.RISCV; use AGATE.Arch.RISCV;

package body AGATE.Traps is

   procedure Initialize;
   procedure Trap_Entry;
   pragma Import (C, Trap_Entry, "trap_entry");

   procedure Risc_Trap_Handler (Mcause : Word);
   pragma Export (C, Risc_Trap_Handler, "trap_handler");

   Handlers_Table : array (Trap_ID) of Trap_Handler :=
     (others => null);

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Unused : Word;
   begin
      Write_Mtvec (Word (To_Integer (Trap_Entry'Address)));
   end Initialize;

   -----------------------
   -- Risc_Trap_Handler --
   -----------------------

   procedure Risc_Trap_Handler (Mcause : Word)
   is
      Code     : constant Integer := Integer (Mcause and 16#FFFF#);
      ID       : Trap_ID;
      Bad_Addr : constant Word := Mbadaddr;
      pragma Unreferenced (Bad_Addr);
   begin
      if (Mcause and 16#8000_0000#) /= 0 then
         --  Asynchronous
         if Code <= 11 then
            ID := Trap_ID (-24 + Code);
         else
            raise Program_Error;
         end if;
      else
         --  Synchronous
         if Code <= 11 then
            ID := Trap_ID (-12 + Code);
         else
            raise Program_Error;
         end if;
      end if;

      if Handlers_Table (ID) /= null then
         Handlers_Table (ID).all;
      else
         raise Program_Error with "No handler for this trap";
      end if;
   end Risc_Trap_Handler;

   --------------
   -- Register --
   --------------

   procedure Register
     (Handler  : Trap_Handler;
      ID       : Trap_ID;
      Priority : Trap_Priority)
   is
      pragma Unreferenced (Priority);
   begin
      Handlers_Table (ID) := Handler;
   end Register;

   ------------
   -- Enable --
   ------------

   procedure Enable (ID : Trap_ID)
   is
      IE : Interrupt_Register;
   begin
      case ID is
         when -17 =>  -- MTimer
            IE := Mie;
            IE.MTI := True;
            Write_Mie (IE);
         when others =>
            raise Program_Error with "Unsupported trap";
      end case;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (ID : Trap_ID)
   is
      IE : Interrupt_Register;
   begin
      case ID is
         when -17 =>  -- MTimer
            IE := Mie;
            IE.MTI := False;
            Write_Mie (IE);
         when others =>
            raise Program_Error with "Unsupported trap";
      end case;
   end Disable;

begin
   Initialize;
end AGATE.Traps;
