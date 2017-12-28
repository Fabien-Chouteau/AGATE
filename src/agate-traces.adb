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

with GNAT.OS_Lib; use GNAT.OS_Lib;

with AGATE.Timing;

package body AGATE.Traces is

   Next_Token : Character := '!';

   type Task_Info is record
      ID    : Task_ID := null;
      Token : Character := ' ';
   end record;

   Task_Array : array (0 .. 9) of Task_Info;

   type Sem_Info is record
      Used  : Boolean := False;
      ID    : Semaphore_ID;
      Token : Character := ' ';
   end record;

   Sem_Array : array (0 .. 9) of Sem_Info;

   type Mutex_Info is record
      Used  : Boolean := False;
      ID    : Mutex_ID;
      Token : Character := ' ';
   end record;

   Mutex_Array : array (0 .. 9) of Mutex_Info;

   Registration_Done : Boolean := False;

   FD : File_Descriptor;

   procedure Initialize;
   procedure Put_Line (Str : String);
   procedure End_Of_Registration;
   function Timestamp (Now : Time := 0) return String;
   procedure Put_State_Change (Token : Character;
                               Value : Integer;
                               Now   : Time := 0);

   function Create_Token return Character;
   function Token (ID : Task_ID) return Character;
   function Token (ID : Semaphore_ID) return Character;
   function Token (ID : Mutex_ID) return Character;
   function Clean (Name : String) return String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      FD := Create_File ("agate_traces.vcd", Text);

      if FD = Invalid_FD then
         raise Program_Error with "Cannot create trace file";
      end if;

      Put_Line ("$timescale 1 us $end");
      Put_Line ("$scope module AGATE $end");
   end Initialize;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Str : String)
   is
      To_Put : constant String := Str & ASCII.LF;
   begin
      if Write (FD, To_Put (To_Put'First)'Address, To_Put'Length) /= To_Put'Length then
         raise Program_Error;
      end if;
   end Put_Line;

   -------------------------
   -- End_Of_Registration --
   -------------------------

   procedure End_Of_Registration is
   begin
      Put_Line ("$upscope $end");
      Put_Line ("$enddefinitions $end");
      for Elt of Task_Array loop
         if Elt.Id /= null then
            Put_Line ("#0 0" & Elt.Token);
         end if;
      end loop;

      for Elt of Sem_Array loop
         if Elt.Id /= Invalid_Semaphore then
            Put_Line ("#0 0" & Elt.Token);
         end if;
      end loop;

      for Elt of Mutex_Array loop
         if Elt.Id /= Invalid_Mutex then
            Put_Line ("#0 0" & Elt.Token);
         end if;
      end loop;

      Registration_Done := True;
   end End_Of_Registration;

   ---------------
   -- Timestamp --
   ---------------

   function Timestamp
     (Now : Time := 0)
     return String
   is
      T   : Constant Time := (if Now /= 0 then Now else Timing.Clock);
      Img : constant String := T'Img;
   begin
      return "#" & Img (Img'First + 1 .. Img'Last);
   end Timestamp;

   ----------------------
   -- Put_State_Change --
   ----------------------

   procedure Put_State_Change
     (Token : Character;
      Value : Integer;
      Now   : Time := 0)
   is
   begin
      if not Registration_Done then
         End_Of_Registration;
      end if;

      Put_Line (Timestamp (Now) & Value'Img & Token);
   end Put_State_Change;

   -----------
   -- Token --
   -----------

   function Token
     (ID : Task_ID)
      return Character
   is
   begin
      for Elt of Task_Array loop
         if Elt.ID = ID then
            return Elt.Token;
         end if;
      end loop;

      raise Program_Error with "Unknown Task_ID";

      return '$';
   end Token;

   -----------
   -- Token --
   -----------

   function Token
     (ID : Semaphore_ID)
      return Character
   is
   begin
      for Elt of Sem_Array loop
         if Elt.ID = ID then
            return Elt.Token;
         end if;
      end loop;

      raise Program_Error with "Unknown Semaphore_ID";

      return '$';
   end Token;

   -----------
   -- Token --
   -----------

   function Token
     (ID : Mutex_ID)
      return Character
   is
   begin
      for Elt of Mutex_Array loop
         if Elt.ID = ID then
            return Elt.Token;
         end if;
      end loop;

      raise Program_Error with "Unknown Mutex_ID";

      return '$';
   end Token;

   -----------
   -- Clean --
   -----------

   function Clean
     (Name : String)
      return String
   is
      Ret : String := Name;
   begin
      for C of Ret loop
         if C in ASCII.NUL .. ' ' then
            C := '_';
         end if;
      end loop;
      return Ret;
   end Clean;

   ------------------
   -- Create_Token --
   ------------------

   function Create_Token
     return Character
   is
   begin
      Next_Token := Character'Succ (Next_Token);
      return Character'Pred (Next_Token);
   end Create_Token;

   --------------
   -- Register --
   --------------

   procedure Register
     (ID   : Task_ID;
      Name : String)
   is
      Index : Natural := Task_Array'First;
   begin
      while Index in Task_Array'Range
        and then
          Task_Array (Index).ID /= null
      loop
         Index := Index + 1;
      end loop;

      if Index not in Task_Array'Range then
         raise Program_Error with "Not enough space to log task traces";
      end if;

      Task_Array (Index).ID := ID;
      Task_Array (Index).Token := Create_Token;

      Put_Line ("$var wire 1 " & Task_Array (Index).Token &
                  " " & Clean (Name) & " $end");
   end Register;

   ------------
   -- Resume --
   ------------

   procedure Resume (ID : Task_ID) is
   begin
      --  Put_State_Change (Token (ID), 1);
      null;
   end Resume;

   -------------
   -- Suspend --
   -------------

   procedure Suspend (ID : Task_ID) is
   begin
      --  Put_State_Change (Token (ID), 0);
      null;
   end Suspend;

   -------------
   -- Running --
   -------------

   procedure Running (ID : Task_ID) is
   begin
      null;
   end Running;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch
     (Old, Next : Task_ID)
   is
      Now : constant Time := Timing.Clock;
   begin
      Put_State_Change (Token (Old), 0, Now);
      Put_State_Change (Token (Next), 1, Now);
   end Context_Switch;

   --------------
   -- Register --
   --------------

   procedure Register
     (ID   : Semaphore_ID;
      Name : String)
   is
      Index : Natural := Sem_Array'First;
   begin
      while Index in Sem_Array'Range
        and then
          Sem_Array (Index).ID /= Invalid_Semaphore
      loop
         Index := Index + 1;
      end loop;

      if Index not in Sem_Array'Range then
         raise Program_Error with "Not enough space to log semaphore traces";
      end if;

      Sem_Array (Index).ID := ID;
      Sem_Array (Index).Token := Create_Token;

      Put_Line ("$var wire 1 " & Sem_Array (Index).Token &
                  " " & Clean (Name) & " $end");
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
      Put_State_Change (Token (ID), Integer (Count));
   end Value_Changed;

   --------------
   -- Register --
   --------------

   procedure Register
     (ID   : Mutex_ID;
      Name : String)
   is
      Index : Natural := Mutex_Array'First;
   begin
      while Index in Mutex_Array'Range
        and then
          Mutex_Array (Index).ID /= Invalid_Mutex
      loop
         Index := Index + 1;
      end loop;

      if Index not in Mutex_Array'Range then
         raise Program_Error with "Not enough space to log mutex traces";
      end if;

      Mutex_Array (Index).ID := ID;
      Mutex_Array (Index).Token := Create_Token;

      Put_Line ("$var wire 1 " & Mutex_Array (Index).Token &
                  " " & Clean (Name) & " $end");
   end;

   ----------
   -- Lock --
   ----------

   procedure Lock
     (ID : Mutex_ID;
      By : Task_ID)
   is
   begin
      Put_State_Change (Token (ID), 1);
   end;

   -------------
   -- Release --
   -------------

   procedure Release
     (ID : Mutex_ID;
      By : Task_ID)
   is
   begin
      Put_State_Change (Token (ID), 0);
   end;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
   is
   begin
      Close (FD);
   end Shutdown;

begin
   Initialize;
end AGATE.Traces;
