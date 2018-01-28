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

with HAL;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package AGATE_Arch_Parameters is

   subtype Word is HAL.UInt32;

   type Context_Index is range 0 .. 31;
   type Task_Context is array (Context_Index) of Word
     with Pack, Size => 32 * 32;
   --  Contains the stack pointer and PC of a task

   Ctx_PC_Index : constant Context_Index := 0;
   Ctx_SP_Index : constant Context_Index := 2;
   Ctx_TP_Index : constant Context_Index := 4;
   Ctx_A0_Index : constant Context_Index := 10;
   Ctx_A1_Index : constant Context_Index := 11;
   Ctx_A2_Index : constant Context_Index := 12;
   Ctx_A3_Index : constant Context_Index := 13;

   type Interrupt_ID is range -24 .. -1;
   type Interrupt_Priority is range 0 .. 0;

   CLINT_Addr            : constant := 16#02000000#;
   CLINT_Mtime_Offset    : constant := 16#BFF8#;
   CLINT_Mtimecmp_Offset : constant := 16#4000#;

   Mtime_Lo_Addr : Address := To_Address (CLINT_Addr + CLINT_Mtime_Offset);
   Mtime_Hi_Addr : Address := To_Address (CLINT_Addr + CLINT_Mtime_Offset + 4);

   Mtimecmp_Lo_Addr : Address := To_Address (CLINT_Addr + CLINT_Mtimecmp_Offset);
   Mtimecmp_Hi_Addr : Address := To_Address (CLINT_Addr + CLINT_Mtimecmp_Offset + 4);

   Timer_Frequency : constant := 32768;
end AGATE_Arch_Parameters;
