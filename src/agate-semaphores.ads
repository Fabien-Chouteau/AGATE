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

with HAL; use HAL;

package AGATE.Semaphores is

   type Semaphore_Count is new Natural;

   type Semaphore_ID is limited private;

   function Count (Sem : Semaphore_ID)
                   return Semaphore_Count;

   procedure Signal (Sem : Semaphore_ID);

   procedure Wait_For_Signal (Sem : Semaphore_ID);

   function To_UInt32 (ID : Semaphore_ID) return UInt32;
   function To_ID (ID : UInt32) return Semaphore_ID;

private

   type Semaphore (Initial_Count : Semaphore_Count := 0)
   is limited record
      Count        : Semaphore_Count := Initial_Count;
      Waiting_List : Task_Object_Access := null;
   end record;

   type Semaphore_Access is access all Semaphore;
   type Semaphore_ID is new not null Semaphore_Access;

   function Count (Sem : Semaphore)
                   return Semaphore_Count;

   procedure Signal (Sem : in out Semaphore);

   procedure Wait_For_Signal (Sem : in out Semaphore);


   procedure Insert_Task (Sem : in out Semaphore;
                          T : Task_Object_Access);

   procedure Resume_One_Task (Sem : in out Semaphore)
     with Pre => Sem.Waiting_List /= null;

end AGATE.Semaphores;
