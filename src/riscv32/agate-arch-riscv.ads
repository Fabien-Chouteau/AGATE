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

package AGATE.Arch.RISCV is

   function Stvec return Word
     with Inline_Always;

   procedure Write_Stvec (Val : Word)
     with Inline_Always;

   function Mtvec return Word
     with Inline_Always;

   procedure Write_Mtvec (Val : Word)
     with Inline_Always;

   function Mscratch return Word
     with Inline_Always;

   procedure Write_Mcratch (Val : Word)
     with Inline_Always;

   function Mbadaddr return Word
     with Inline_Always;

   type Mstatus_Reg is record
      UIE    : Boolean;
      SIE    : Boolean;
      HIE    : Boolean;
      MIE    : Boolean;
      UPIE   : Boolean;
      SPIE   : Boolean;
      HPIE   : Boolean;
      MPIE   : Boolean;
      SPP    : Bit;
      HPP    : UInt2;
      MPP    : UInt2;
      FS     : UInt2;
      XS     : UInt2;
      MPRV   : Boolean;
      PUM    : Boolean;
      MXR    : Boolean;
      WPRI_1 : UInt4; -- Reserved Writes Preserve Values, Reads Ignore
      VM     : UInt5;
      WPRI_2 : UInt2;   -- Reserved Writes Preserve Values, Reads Ignore
      SD     : Boolean;
   end record
     with Pack, Size => 32;

   for Mstatus_Reg use record
      UIE    at 0 range 0 .. 0;
      SIE    at 0 range 1 .. 1;
      HIE    at 0 range 2 .. 2;
      MIE    at 0 range 3 .. 3;
      UPIE   at 0 range 4 .. 4;
      SPIE   at 0 range 5 .. 5;
      HPIE   at 0 range 6 .. 6;
      MPIE   at 0 range 7 .. 7;
      SPP    at 0 range 8 .. 8;
      HPP    at 0 range 9 .. 10;
      MPP    at 0 range 11 .. 12;
      FS     at 0 range 13 .. 14;
      XS     at 0 range 15 .. 16;
      MPRV   at 0 range 17 .. 17;
      PUM    at 0 range 18 .. 18;
      MXR    at 0 range 19 .. 19;
      WPRI_1 at 0 range 20 .. 23;
      VM     at 0 range 24 .. 28;
      WPRI_2 at 0 range 29 .. 30;
      SD     at 0 range 31 .. 31;
   end record;

   function Mstatus return Mstatus_Reg
     with Inline_Always;

   procedure Write_Mstatus (Val : Mstatus_Reg)
     with Inline_Always;

   type Interrupt_Register is record
      USI      : Boolean;
      SSI      : Boolean;
      HSI      : Boolean;
      MSI      : Boolean;
      UTI      : Boolean;
      STI      : Boolean;
      HTI      : Boolean;
      MTI      : Boolean;
      UEI      : Boolean;
      SEI      : Boolean;
      HEI      : Boolean;
      MEI      : Boolean;
      Reserved : UInt18;
   end record
     with Pack, Size => 32;

   for Interrupt_Register use record
      USI      at 0 range 0 .. 0;
      SSI      at 0 range 1 .. 1;
      HSI      at 0 range 2 .. 2;
      MSI      at 0 range 3 .. 3;
      UTI      at 0 range 4 .. 4;
      STI      at 0 range 5 .. 5;
      HTI      at 0 range 6 .. 6;
      MTI      at 0 range 7 .. 7;
      UEI      at 0 range 8 .. 8;
      SEI      at 0 range 9 .. 9;
      HEI      at 0 range 10 .. 10;
      MEI      at 0 range 11 .. 11;
      Reserved at 0 range 12 .. 31;
   end record;

   function Mip return Interrupt_Register
     with Inline_Always;

   procedure Write_Mip (Val : Interrupt_Register)
     with Inline_Always;

   function Mie return Interrupt_Register
     with Inline_Always;

   procedure Write_Mie (Val : Interrupt_Register)
     with Inline_Always;

end AGATE.Arch.RISCV;
