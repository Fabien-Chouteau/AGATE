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

with HAL;                     use HAL;
with Semihosting;             use Semihosting;
with AGATE.Timing;
with AGATE_Types_Data.Traces; use AGATE_Types_Data.Traces;

package body AGATE.Traces is

   Next_String_Token : String_Token := 0;
   Next_Wire_Token   : Wire_Token := 0;
   Next_Reg_Token    : Reg_Token := 0;
   Next_Event_Token  : Event_Token := 0;

   Running_Task_Token : String_Token;
   Context_Switch_Token : Event_Token;

   Registration_Done : Boolean := False;

   FD : SH_Word;

   procedure Initialize;
   procedure Put_Line (Str : String);
   procedure End_Of_Registration;
   function Timestamp (Now : Time := 0) return String;
   procedure Put_State_Change (Token : Reg_Token;
                               Value : UInt32;
                               Now   : Time := 0);
   procedure Put_State_Change (Token : Wire_Token;
                               Value : Boolean;
                               Now   : Time := 0);
   procedure Put_State_Change (Token : String_Token;
                               Value : String;
                               Now   : Time := 0);
   procedure Put_State_Change (Token : Event_Token;
                               Now   : Time := 0);

   function Create return String_Token;
   function Create return Wire_Token;
   function Create return Reg_Token;
   function Create return Event_Token;

   function Image (Val : Natural) return String;
   function Image (Tok : String_Token) return String;
   function Image (Tok : Wire_Token) return String;
   function Image (Tok : Reg_Token) return String;
   function Image (Tok : Event_Token) return String;

   procedure Declare_Var (Tok : String_Token; Name : String);
   procedure Declare_Var (Tok : Wire_Token; Name : String);
   procedure Declare_Var (Tok : Reg_Token; Name : String);
   procedure Declare_Var (Tok : Event_Token; Name : String);

   function Clean (Name : String) return String;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      FD := Open ("agate_traces.vcd", OPEN_FLAG_W);

      if FD = SH_Word'Last or else FD = 0 then
         raise Program_Error with "Cannot create trace file";
      end if;

      Put_Line ("$timescale 1 us $end");
      Put_Line ("$scope module AGATE $end");

      Running_Task_Token := Create;
      Declare_Var (Running_Task_Token, "Running_Task");

      Context_Switch_Token := Create;
      Declare_Var (Context_Switch_Token, "Context_Switch");
   end Initialize;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Str : String)
   is
      To_Put : constant String := Str & ASCII.LF;
   begin
      if Write (FD, To_Put (To_Put'First)'Address, To_Put'Length) /= 0 then
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

      --  Print initial values

      for X in Wire_Token'First .. Next_Wire_Token - 1 loop
         Put_Line ("#0 0" & Image (X));
      end loop;

      for X in Reg_Token'First .. Next_Reg_Token - 1 loop
         Put_Line ("#0 b0 " & Image (X));
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
     (Token : Reg_Token;
      Value : UInt32;
      Now   : Time := 0)
   is
      Bin : String (1 .. 32);
      Val : UInt32 := Value;
   begin
      if not Registration_Done then
         End_Of_Registration;
      end if;

      for C of reverse Bin loop
         C := (if (Val and 1) = 0 then '0' else '1');
         Val := Shift_Right (Val, 1);
      end loop;

      --  TODO: print the boolean value of the integer (e.g. b100101)
      Put_Line (Timestamp (Now) & " b" & bin & " " & Image (Token));
   end Put_State_Change;

   ----------------------
   -- Put_State_Change --
   ----------------------

   procedure Put_State_Change
     (Token : Wire_Token;
      Value : Boolean;
      Now   : Time := 0)
   is
   begin
      if not Registration_Done then
         End_Of_Registration;
      end if;

      Put_Line (Timestamp (Now) & (if Value then " 1" else " 0") &
                  Image(Token));
   end Put_State_Change;

   ----------------------
   -- Put_State_Change --
   ----------------------

   procedure Put_State_Change
     (Token : String_Token;
      Value : String;
      Now   : Time := 0)
   is
   begin
      if not Registration_Done then
         End_Of_Registration;
      end if;

      Put_Line (Timestamp (Now) & " s" & Value & " " & Image (Token));
   end Put_State_Change;

   ----------------------
   -- Put_State_Change --
   ----------------------

   procedure Put_State_Change
     (Token : Event_Token;
      Now   : Time := 0)
   is
   begin
      if not Registration_Done then
         End_Of_Registration;
      end if;

      Put_Line (Timestamp (Now) & " x" & Image (Token));
   end Put_State_Change;

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

   ------------
   -- Create --
   ------------

   function Create
     return String_Token
   is
      Ret : String_Token := Next_String_Token;
   begin
      Next_String_Token := Next_String_Token + 1;
      return Ret;
   end Create;


   ------------
   -- Create --
   ------------

   function Create
     return Wire_Token
   is
      Ret : Wire_Token := Next_Wire_Token;
   begin
      Next_Wire_Token := Next_Wire_Token + 1;
      return Ret;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     return Reg_Token
   is
      Ret : Reg_Token := Next_Reg_Token;
   begin
      Next_Reg_Token := Next_Reg_Token + 1;
      return Ret;
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     return Event_Token
   is
      Ret : Event_Token := Next_Event_Token;
   begin
      Next_Event_Token := Next_Event_Token + 1;
      return Ret;
   end Create;

   -----------
   -- Image --
   -----------

   function Image
     (Val : Natural)
      return String
   is
      Ret : constant String := Val'Img;
   begin
      return Ret (Ret'First + 1 .. Ret'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Tok : String_Token)
      return String
   is ("s" & Image (Natural (Tok)));

   -----------
   -- Image --
   -----------

   function Image
     (Tok : Wire_Token)
      return String
   is ("w" & Image (Natural (Tok)));

   -----------
   -- Image --
   -----------

   function Image
     (Tok : Reg_Token)
      return String
   is ("r" & Image (Natural (Tok)));

   -----------
   -- Image --
   -----------

   function Image
     (Tok : Event_Token)
      return String
   is ("e" & Image (Natural (Tok)));

   -----------------
   -- Declare_Var --
   -----------------

   procedure Declare_Var
     (Tok  : String_Token;
      Name : String)
   is
   begin
      Put_Line ("$var string 1 " & Image (Tok) & " " & Name & " $end");
   end Declare_Var;

   -----------------
   -- Declare_Var --
   -----------------

   procedure Declare_Var
     (Tok  : Wire_Token;
      Name : String)
   is
   begin
      Put_Line ("$var wire 1 " & Image (Tok) & " " & Name & " $end");
   end Declare_Var;

   -----------------
   -- Declare_Var --
   -----------------

   procedure Declare_Var
     (Tok  : Reg_Token;
      Name : String)
   is
   begin
      Put_Line ("$var reg 32 " & Image (Tok) & " " & Name & " $end");
   end Declare_Var;

   -----------------
   -- Declare_Var --
   -----------------

   procedure Declare_Var
     (Tok  : Event_Token;
      Name : String)
   is
   begin
      Put_Line ("$var event 1 " & Image (Tok) & " " & Name & " $end");
   end Declare_Var;

   --------------
   -- Register --
   --------------

   procedure Register
     (ID   : Task_ID;
      Name : String)
   is
   begin
      ID.Trace_Data.Running := Create;
      ID.Trace_Data.Status := Create;
      ID.Trace_Data.Prio := Create;

      Declare_Var (ID.Trace_Data.Running, Clean (Name));
      Declare_Var (ID.Trace_Data.Status, Clean (Name) & "_Status");
      Declare_Var (ID.Trace_Data.Prio, Clean (Name) & "_Priority");
   end Register;

   ------------
   -- Resume --
   ------------

   procedure Resume (ID : Task_ID) is
   begin
      Put_State_Change (ID.Trace_Data.Status, Clean (Image (ID.Status)));
      Put_State_Change (ID.Trace_Data.Prio, UInt32 (ID.Current_Prio));
   end Resume;

   -------------
   -- Suspend --
   -------------

   procedure Suspend (ID : Task_ID) is
   begin
      Put_State_Change (ID.Trace_Data.Status, Clean (Image (ID.Status)));
   end Suspend;

   -------------
   -- Running --
   -------------

   procedure Running (ID : Task_ID) is
   begin
      Put_State_Change (ID.Trace_Data.Status, Clean (Image (ID.Status)));
   end Running;

   ---------------------
   -- Change_Priority --
   ---------------------

   procedure Change_Priority
     (ID       : Task_ID;
      New_Prio : Task_Priority)
   is
   begin
      Put_State_Change (ID.Trace_Data.Prio, UInt32 (New_Prio));
   end Change_Priority;

   --------------------
   -- Context_Switch --
   --------------------

   procedure Context_Switch
     (Old, Next : Task_ID)
   is
      Now : constant Time := Timing.Clock;
   begin
      Put_State_Change (Old.Trace_Data.Running, False, Now);
      Put_State_Change (Next.Trace_Data.Running, True, Now);
      Put_State_Change (Running_Task_Token, Clean (Next.Name), Now);
      Put_State_Change (Context_Switch_Token, Now);
   end Context_Switch;

   --------------
   -- Register --
   --------------

   procedure Register
     (ID   : Semaphore_ID;
      Name : String)
   is
   begin
      ID.Trace_Data.Token := Create;

      Declare_Var (ID.Trace_Data.Token, Clean (Name));
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
      Put_State_Change (ID.Trace_Data.Token, UInt32 (Count));
   end Value_Changed;

   --------------
   -- Register --
   --------------

   procedure Register
     (ID   : Mutex_ID;
      Name : String)
   is
   begin
      ID.Trace_Data.Token := Create;

      Declare_Var (ID.Trace_Data.Token, Clean (Name) & "_Owner");
   end Register;

   ----------
   -- Lock --
   ----------

   procedure Lock
     (ID : Mutex_ID;
      By : Task_ID)
   is
   begin
      Put_State_Change (ID.Trace_Data.Token, Clean (By.Name));
   end Lock;

   -------------
   -- Release --
   -------------

   procedure Release
     (ID : Mutex_ID;
      By : Task_ID)
   is
   begin
      Put_State_Change (ID.Trace_Data.Token, "unlocked");
   end Release;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
   is
      Unref : SH_Word with Unreferenced;
   begin
      Unref := Close (FD);
   end Shutdown;

begin
   Initialize;
end AGATE.Traces;
