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

with GNAT.OS_Lib;

with League.Calendars;
with League.Strings;
with League.String_Vectors;

with CvsWeb.Loaders;

package CvsWeb.Pushers is

   type Pusher is tagged limited private;

   procedure Initialize
     (Self : in out Pusher;
      Path : League.Strings.Universal_String);

   procedure Push
     (Self   : in out Pusher;
      Loader : in out CvsWeb.Loaders.Loader;
      Skip   : League.Calendars.Date_Time);

private
   type Pusher is tagged limited record
      Path : League.Strings.Universal_String;
      Git  : GNAT.OS_Lib.String_Access;
   end record;

   not overriding procedure Git_Add
     (Self : Pusher;
      File : League.Strings.Universal_String);

   not overriding procedure Git_Commit
     (Self    : Pusher;
      Date    : League.Calendars.Date_Time;
      Message : League.Strings.Universal_String;
      List    : League.String_Vectors.Universal_String_Vector);

   not overriding procedure Exec
     (Self : Pusher;
      List : in out GNAT.OS_Lib.Argument_List);

end CvsWeb.Pushers;
