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

with FE310_SVD.GPIO;   use FE310_SVD.GPIO;
with FE310_SVD.UART;   use FE310_SVD.UART;
with AGATE.Arch.RISCV; use AGATE.Arch.RISCV;

package body AGATE.Console is

   procedure Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      GPIO0_Periph.IO_FUNC_SEL.Arr (17) := False;
      GPIO0_Periph.IO_FUNC_SEL.Arr (18) := False;

      GPIO0_Periph.IO_FUNC_EN.Arr (18) := True;
      GPIO0_Periph.IO_FUNC_EN.Arr (17) := True;

      UART0_Periph.DIV.DIV := UInt16 ((CPU_Frequency / 115200)) - 1;
      UART0_Periph.TXCTRL.ENABLE := True;

      for I in 1 .. 1_000 loop
         null;
      end loop;
   end Initialize;

   -----------
   -- Print --
   -----------

   procedure Print (C : Character) is
   begin
      while UART0_Periph.TXDATA.FULL loop
         null;
      end loop;

      UART0_Periph.TXDATA.DATA := Character'Pos (C);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Str : String) is
   begin
      for C of Str loop
         Print (C);
      end loop;
   end Print;

   ----------------
   -- Print_Line --
   ----------------

   procedure Print_Line (Str : String) is
   begin
      Print (Str);
      Print (ASCII.CR);
      Print (ASCII.LF);
   end Print_Line;

begin
   Initialize;
end AGATE.Console;
