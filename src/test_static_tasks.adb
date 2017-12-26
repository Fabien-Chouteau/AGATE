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
with AGATE.SysCalls;
with AGATE;              use AGATE;

package body Test_Static_Tasks is

   -------------
   -- T1_Proc --
   -------------

   procedure T1_Proc is
      Now : Time;
   begin

      Ada.Text_IO.Put_Line ("---> Static T1 Signal Static_Semaphore");
      AGATE.SysCalls.Signal (Static_Semaphore.ID);

      loop
         Ada.Text_IO.Put_Line ("---> Static T1 Wait_Lock on Static_Mutex");
         SysCalls.Wait_Lock (Static_Mutex.ID);
         Ada.Text_IO.Put_Line ("---> Static T1 Got the mutex");

         Now := Time (AGATE.SysCalls.Clock);

         Ada.Text_IO.Put_Line ("---> Static T1 Clock:" & Now'Img);
         AGATE.SysCalls.Delay_Until (Now + 5_000_000);

         Ada.Text_IO.Put_Line ("---> Static T1 Release Static_Mutex");
         SysCalls.Release (Static_Mutex.ID);
      end loop;
   end T1_Proc;

   -------------
   -- T2_Proc --
   -------------

   procedure T2_Proc is
      Now : Time;
      Cnt : Natural := 0;
   begin

      Ada.Text_IO.Put_Line ("---> Static T2 Wait_For_Signal on Static_Semaphore");
      AGATE.SysCalls.Wait_For_Signal (Static_Semaphore.ID);
      Ada.Text_IO.Put_Line ("---> Static T2 released");

      loop
         Now := Time (AGATE.SysCalls.Clock);
         Ada.Text_IO.Put_Line ("---> Static T2 Clock:" & Now'Img);

         Cnt := Cnt + 1;

         if Cnt = 2 then
            Ada.Text_IO.Put_Line ("---> Static T2 Wait_Lock on Static_Mutex");
            SysCalls.Wait_Lock (Static_Mutex.ID);
            Ada.Text_IO.Put_Line ("---> Static T2 Got the mutex");
         elsif Cnt = 3 then
            Ada.Text_IO.Put_Line ("---> Static T2 Release Static_Mutex");
            SysCalls.Release (Static_Mutex.ID);
         elsif Cnt = 4 then
            Ada.Text_IO.Put_Line ("---> Static T2 Shuting down the system");
            AGATE.SysCalls.Shutdown_System;
         end if;

         AGATE.SysCalls.Delay_Until (Now + 10_000_000);

      end loop;
   end T2_Proc;

end Test_Static_Tasks;
