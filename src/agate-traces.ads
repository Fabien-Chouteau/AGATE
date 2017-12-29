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

with AGATE.Semaphores; use AGATE.Semaphores;
with AGATE.Mutexes;    use AGATE.Mutexes;

package AGATE.Traces is

   -- Tasks --
   procedure Register (ID   : Task_ID;
                       Name : String);


   procedure Resume (ID : Task_ID);
   procedure Suspend (ID : Task_ID);
   procedure Running (ID : Task_ID);
   procedure Change_Priority (ID       : Task_ID;
                              New_Prio : Internal_Task_Priority);

   procedure Context_Switch (Old, Next : Task_ID);

   -- Semaphores --

   procedure Register (ID   : Semaphore_ID;
                       Name : String);

   procedure Value_Changed (ID    : Semaphore_ID;
                            Count : Semaphore_Count;
                            By    : Task_ID);

   -- Mutexes --

   procedure Register (ID   : Mutex_ID;
                       Name : String);

   procedure Lock (ID : Mutex_ID;
                   By : Task_ID);

   procedure Release (ID : Mutex_ID;
                      By : Task_ID);

   -- System --

   procedure Shutdown;

end AGATE.Traces;
