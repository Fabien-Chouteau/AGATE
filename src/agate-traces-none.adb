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


package body AGATE.Traces is

   --------------
   -- Register --
   --------------

   procedure Register
     (ID   : Task_ID;
      Name : String)
   is
   begin
      null;
   end Register;

   ------------
   -- Resume --
   ------------

   procedure Resume (ID : Task_ID) is
   begin
      null;
   end Resume;

   -------------
   -- Suspend --
   -------------

   procedure Suspend (ID : Task_ID) is
   begin
      null;
   end Suspend;

   -------------
   -- Running --
   -------------

   procedure Running (ID : Task_ID) is
   begin
      null;
   end Running;

   ---------------------
   -- Change_Priority --
   ---------------------

   procedure Change_Priority
     (ID       : Task_ID;
      New_Prio : Internal_Task_Priority)
   is
   begin
      null;
   end Change_Priority;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch
     (Old, Next : Task_ID)
   is
   begin
      null;
   end Context_Switch;

   --------------
   -- Register --
   --------------

   procedure Register
     (ID   : Semaphore_ID;
      Name : String)
   is
   begin
      null;
   end Register;

   -------------------
   -- Value_Changed --
   -------------------

   procedure Value_Changed
     (ID    : Semaphore_ID;
      Count : Semaphore_Count;
      By    : Task_ID)
   is
   begin
      null;
   end Value_Changed;

   --------------
   -- Register --
   --------------

   procedure Register
     (ID   : Mutex_ID;
      Name : String)
   is
   begin
      null;
   end Register;

   ----------
   -- Lock --
   ----------

   procedure Lock
     (ID : Mutex_ID;
      By : Task_ID)
   is
   begin
      null;
   end Lock;

   -------------
   -- Release --
   -------------

   procedure Release
     (ID : Mutex_ID;
      By : Task_ID)
   is
   begin
      null;
   end Release;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
   is
   begin
      null;
   end Shutdown;
end AGATE.Traces;
