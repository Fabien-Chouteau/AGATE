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

with Semihosting; use Semihosting;

package body AGATE.Traces_Output is

   Init_Done : Boolean := False;
   FD : SH_Word;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Filename : String)
   is
   begin
      FD := Open (Filename, OPEN_FLAG_W);

      if FD = SH_Word'Last or else FD = 0 then
         raise Program_Error with "Cannot create trace file";
      end if;

      Init_Done := True;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
   is
      Unref : SH_Word with Unreferenced;
   begin
      Unref := Close (FD);
   end Finalize;

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean is
   begin
      return Init_Done;
   end Initialized;

   -----------
   -- Write --
   -----------

   function Write
     (Buffer_Address  : System.Address;
      Buffer_Size     : Natural)
      return Natural
   is
      Ret : SH_Word;
   begin
      Ret := Write (FD, Buffer_Address, SH_Word (Buffer_Size));
      return Buffer_Size - Natural (Ret);
   end Write;

end AGATE.Traces_Output;
