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

with System.Machine_Code;   use System.Machine_Code;
with AGATE_Arch_Parameters; use AGATE_Arch_Parameters;

package body AGATE.Arch.RISCV is

   CPU_Frequency_Calc : Word := 0;

   generic
      Reg_Name : String;
      type Reg_Type is private;
   function Read_CSR return Reg_Type
     with Inline_Always;

   generic
      Reg_Name : String;
      type Reg_Type is private;
   procedure Write_CSR (Val : Reg_Type)
     with Inline_Always;

   -------------------
   -- CPU_Frequency --
   -------------------

   function CPU_Frequency return Word is

      function Measure_Freq (Count : Word) return Word;

      ------------------
      -- Measure_Freq --
      ------------------

      function Measure_Freq (Count : Word) return Word is
         Start_Mtime  : Word;
         Delta_Mtime  : Word;
         Mtime_Freq   : constant Word := Timer_Frequency;
         Start_Mcycle : Word;
         Delta_Mcycle : Word;
         Tmp          : Word;

         Mtime_Lo : Word with Volatile_Full_Access, Address => Mtime_Lo_Addr;
         Mtime_Hi : Word with Volatile_Full_Access, Address => Mtime_Hi_Addr;
      begin

         Tmp := Mtime_Lo;
         loop
            Start_Mtime := Mtime_Lo;
            exit when Start_Mtime /= Tmp;
         end loop;

         Start_Mcycle := Mcycle_Low;

         loop
            Delta_Mtime := Mtime_Lo - Start_Mtime;
            exit when Delta_Mtime > Count;
         end loop;

         Delta_Mcycle := Mcycle_Low - Start_Mcycle;

         return (Delta_Mcycle / Delta_Mtime) * Mtime_Freq
           + ((Delta_Mcycle mod Delta_Mtime) * Mtime_Freq) / Delta_Mtime;
      end Measure_Freq;

   begin
      if CPU_Frequency_Calc = 0 then
         --  Warm up
         CPU_Frequency_Calc := Measure_Freq (1);

         --  measure for real
         CPU_Frequency_Calc := Measure_Freq (10);
      end if;

      return CPU_Frequency_Calc;
   end CPU_Frequency;

   --------------
   -- Read_CSR --
   --------------

   function Read_CSR return Reg_Type is
      Ret : Reg_Type;
   begin
      Asm ("csrr %0, " & Reg_Name,
           Outputs  => Reg_Type'Asm_Output ("=r", Ret),
           Volatile => True);
      return Ret;
   end Read_CSR;

   ---------------
   -- Write_CSR --
   ---------------

   procedure Write_CSR (Val : Reg_Type) is
   begin
      Asm ("csrw " & Reg_Name & ", %0",
           Inputs   => Reg_Type'Asm_Input ("r", Val),
           Volatile => True);
   end Write_CSR;

   function I_Stvec is new Read_CSR ("stvec", Word);
   function Stvec return Word renames I_Stvec;

   procedure I_Write_Stvec is new Write_CSR ("stvec", Word);
   procedure Write_Stvec (Val : Word) renames I_Write_Stvec;

   function I_Mtvec is new Read_CSR ("mtvec", Word);
   function Mtvec return Word renames I_Mtvec;

   procedure I_Write_Mtvec is new Write_CSR ("mtvec", Word);
   procedure Write_Mtvec (Val : Word) renames I_Write_Mtvec;

   function I_Mscratch is new Read_CSR ("mscratch", Word);
   function Mscratch return Word renames I_Mscratch;

   procedure I_Write_Mscratch is new Write_CSR ("mscratch", Word);
   procedure Write_Mcratch (Val : Word) renames I_Write_Mscratch;

   function I_Mbadaddr is new Read_CSR ("mbadaddr", Word);
   function Mbadaddr return Word renames I_Mbadaddr;

   function I_Mcycle_Low is new Read_CSR ("mcycle", Word);
   function Mcycle_Low return Word renames I_Mcycle_Low;

   function I_Mcycle_Hi is new Read_CSR ("mcycleh", Word);
   function Mcycle_Hi return Word renames I_Mcycle_Hi;

   function I_Mstatus is new Read_CSR ("mstatus", Mstatus_Reg);
   function Mstatus return Mstatus_Reg renames I_Mstatus;

   procedure I_Write_Mstatus is new Write_CSR ("mstatus", Mstatus_Reg);
   procedure Write_Mstatus (Val : Mstatus_Reg) renames I_Write_Mstatus;

   function I_Mip is new Read_CSR ("mip", Interrupt_Register);
   function Mip return Interrupt_Register renames I_Mip;

   procedure I_Write_Mip is new Write_CSR ("mip", Interrupt_Register);
   procedure Write_Mip (Val : Interrupt_Register) renames I_Write_Mip;

   function I_Mie is new Read_CSR ("mie", Interrupt_Register);
   function Mie return Interrupt_Register renames I_Mie;

   procedure I_Write_Mie is new Write_CSR ("mie", Interrupt_Register);
   procedure Write_Mie (Val : Interrupt_Register) renames I_Write_Mie;

end AGATE.Arch.RISCV;
