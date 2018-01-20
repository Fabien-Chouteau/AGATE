------------------------------------------------------------------------------
--                                                                          --
--                Copyright (C) 2017-2018, Fabien Chouteau                  --
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

with System.Machine_Code; use System.Machine_Code;

with Cortex_M_SVD.SCB;    use Cortex_M_SVD.SCB;

with AGATE.Traces;
with AGATE.Arch.ArmvX_m;  use AGATE.Arch.ArmvX_m;

package body AGATE.Tasking.Context_Switch is

   procedure Context_Switch_Handler;
   pragma Machine_Attribute (Context_Switch_Handler, "naked");
   pragma Export (C, Context_Switch_Handler, "PendSV_Handler");

   ------------
   -- Switch --
   ------------

   procedure Switch is
   begin
      --  Trigger PendSV
      SCB_Periph.ICSR.PENDSVSET := True;
   end Switch;

   ----------------------------
   -- Context_Switch_Handler --
   ----------------------------

   procedure Context_Switch_Handler is
   begin

      Asm (Template =>
             "push {lr}" & ASCII.LF &
             "bl current_task_context" & ASCII.LF &
             "stm  r0, {r4-r12}", -- Save extra context
           Volatile => True);

      SCB_Periph.ICSR.PENDSVCLR := True;

      Running_Task.Stack_Pointer := PSP;

      Set_PSP (Ready_Tasks.Stack_Pointer);

      Traces.Context_Switch (Task_ID (Running_Task),
                             Task_ID (Ready_Tasks));

      Running_Task := Ready_Tasks;
      Running_Task.Status := Running;

      Traces.Running (Current_Task);

      Asm (Template =>
             "bl current_task_context" & ASCII.LF &
             "ldm  r0, {r4-r12}"       & ASCII.LF & -- Load extra context
             "pop {pc}",
           Volatile => True);
   end Context_Switch_Handler;

end AGATE.Tasking.Context_Switch;
