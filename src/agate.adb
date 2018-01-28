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

with Ada.Unchecked_Conversion;

package body AGATE is

   function To_Word_Internal is new Ada.Unchecked_Conversion
     (Mutex_ID, Word);

   function To_ID_Internal is new Ada.Unchecked_Conversion
     (Word, Mutex_ID);

   function To_Word_Internal is new Ada.Unchecked_Conversion
     (Semaphore_ID, Word);

   function To_ID_Internal is new Ada.Unchecked_Conversion
     (Word, Semaphore_ID);

   -------------
   -- To_Word --
   -------------

   function To_Word (ID : Mutex_ID) return Word
   is (To_Word_Internal (ID));

   -----------
   -- To_ID --
   -----------

   function To_ID (ID : Word) return Mutex_ID
   is (To_ID_Internal (ID));

   -------------
   -- To_Word --
   -------------

   function To_Word
     (ID : Semaphore_ID)
      return Word
   is (To_Word_Internal (ID));

   -----------
   -- To_ID --
   -----------

   function To_ID
     (ID : Word)
      return Semaphore_ID
   is (To_ID_Internal (ID));

   -----------
   -- Image --
   -----------

   function Image
     (Status : Task_Status)
      return String
   is (case Status is
          when Created             => "Created",
          when Ready               => "Ready",
          when Running             => "Running",
          when Suspended_Alarm     => "Waiting for alarm",
          when Suspended_Semaphore => "Waiting on semaphore",
          when Suspended_Mutex     => "Waiting on mutex");

   ----------
   -- Name --
   ----------

   function Name
     (ID : Task_ID)
      return String
   is (Task_Object_Access (ID).Name);

   -----------
   -- Image --
   -----------

   function Image (ID : Task_ID) return String
   is
      T : constant Task_Object_Access := Task_Object_Access (ID);
   begin
      return "" & T.Name &
        " - Prio:" & T.Base_Prio'Img &
        " - ID:" & Hex (UInt32 (To_Integer (T.all'Address))) &
        " - PSP:" & Image (T.Stack_Pointer) &
        " - " & Image (T.Status);
   end Image;

end AGATE;
