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

with League.Application;
with League.Calendars.ISO_8601;
with League.String_Vectors;
with League.Strings;

with CvsWeb.Loaders;
with CvsWeb.Pushers;

procedure CvsWeb2git is
   function To_Date return League.Calendars.Date_Time;

   Args   : constant League.String_Vectors.Universal_String_Vector :=
     League.Application.Arguments;

   function To_Date return League.Calendars.Date_Time is
      Result : League.Calendars.Date_Time :=
        League.Calendars.ISO_8601.Create
          (Year           => 1999,
           Month          => 1,
           Day            => 1,
           Hour           => 0,
           Minute         => 0,
           Second         => 0,
           Nanosecond_100 => 0);
   begin
      if Args.Length > 2 then
         declare
            use League.Calendars.ISO_8601;
            List : constant League.String_Vectors.Universal_String_Vector :=
              Args.Element (3).Split ('.');
         begin
            Result := League.Calendars.ISO_8601.Create
              (Year           => Year_Number'Wide_Wide_Value
                 (List (1).To_Wide_Wide_String),
               Month          => Month_Number'Wide_Wide_Value
                 (List (2).To_Wide_Wide_String),
               Day            => Day_Number'Wide_Wide_Value
                 (List (3).To_Wide_Wide_String),
               Hour           => Hour_Number'Wide_Wide_Value
                 (List (4).To_Wide_Wide_String),
               Minute         => Minute_Number'Wide_Wide_Value
                 (List (5).To_Wide_Wide_String),
               Second         => Second_Number'Wide_Wide_Value
                 (List (6).To_Wide_Wide_String),
               Nanosecond_100 => 0);
         end;
      end if;

      return Result;
   end To_Date;

   URL    : League.Strings.Universal_String;
   Root   : League.Strings.Universal_String;
   Loader : CvsWeb.Loaders.Loader;
   Pusher : CvsWeb.Pushers.Pusher;
begin
   URL := Args.Element (1);
   Root := Args.Element (2);

   Loader.Initialize (URL);
   Pusher.Initialize (Root);
   Pusher.Push (Loader, Skip => To_Date);
end CvsWeb2git;
