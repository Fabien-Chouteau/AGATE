------------------------------------------------------------------------------
--                                                                          --
--                Copyright (C) 2017-2020, Fabien Chouteau                  --
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

private package AGATE.Scheduler is

   procedure Register (ID   : Task_ID;
                       Name : String)
     with Pre => Name'Length <= Task_Name'Length;

   procedure Start
     with No_Return;

   procedure Print_Ready_Tasks;

   -- Scheduler --

   function Current_Task return Task_ID;
   function Task_To_Run return Task_ID;
   procedure Yield;
   procedure Resume (ID : Task_ID);

   type Suspend_Reason is (Alarm, Semaphore, Mutex);
   procedure Suspend (Reason : Suspend_Reason);

   procedure Fault;
   --  Signal a fault from the running task

   procedure Fault (ID : Task_ID);
   --  Signal a fault from the given task

   procedure Change_Priority (New_Prio : Task_Priority);

   procedure Delay_Until (Wake_Up_Time : Time);

   procedure Wakeup_Expired_Alarms;

   function Context_Switch_Needed return Boolean;
   procedure Do_Context_Switch;

   function Current_Task_Context return System.Address;
   pragma Export (C, Current_Task_Context, "current_task_context");

private

   Running_Task : Task_Object_Access := null;
   Ready_Tasks  : Task_Object_Access := null;

end AGATE.Scheduler;
