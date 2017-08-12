--  BSD 3-Clause License
--
--  Copyright (c) 2017, Maxim Reznik
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  * Redistributions of source code must retain the above copyright notice,
--    this list of conditions and the following disclaimer.
--
--  * Redistributions in binary form must reproduce the above copyright notice,
--    this list of conditions and the following disclaimer in the documentation
--    and/or other materials provided with the distribution.
--
--  * Neither the name of the copyright holder nor the names of its
--    contributors may be used to endorse or promote products derived from
--    this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
--  THE POSSIBILITY OF SUCH DAMAGE.

with Ada.Wide_Wide_Text_IO;

with AWS.URL;
with AWS.Response;

package body CvsWeb.HTTP is

   ---------
   -- Get --
   ---------

   not overriding procedure Get
     (Self    : in out Connection;
      URL     : League.Strings.Universal_String;
      Result  : out League.Stream_Element_Vectors.Stream_Element_Vector;
      Success : out Boolean)
   is
      Request  : constant AWS.URL.Object :=
        AWS.URL.Parse (URL.To_UTF_8_String);
      Response : AWS.Response.Data;
   begin
      if not Self.Ready then
         AWS.Client.Create
           (Connection  => Self.Server,
            Host        => AWS.URL.URL (Request));
         Self.Ready := True;
      end if;

      Ada.Wide_Wide_Text_IO.Put_Line (URL.To_Wide_Wide_String);

      AWS.Client.Get
        (Connection => Self.Server,
         Result     => Response,
         URI        => AWS.URL.Pathname_And_Parameters (Request));

      Result.Clear;
      Result.Append (AWS.Response.Message_Body (Response));
      Success := True;
   exception
      when others =>
         Success := False;
   end Get;

end CvsWeb.HTTP;
