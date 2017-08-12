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

with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with League.Calendars.ISO_8601;
with League.Stream_Element_Vectors;

package body CvsWeb.Pushers is

   Author : constant String := "Randy Brukardt <randy@rrsoftware.com>";
   Format : constant League.Strings.Universal_String :=
     League.Strings.To_Universal_String ("yyyy-MM-ddTHH:mm:ss");

   procedure Make_Dirs (List : League.String_Vectors.Universal_String_Vector);

   ----------
   -- Exec --
   ----------

   procedure Exec
     (Self : Pusher;
      List : in out GNAT.OS_Lib.Argument_List)
   is
      Ok   : Boolean;
   begin
      Ada.Text_IO.Put ("Exec: " & Self.Git.all);
      for J of List loop
         Ada.Text_IO.Put (' ');
         Ada.Text_IO.Put (J.all);
      end loop;
      Ada.Text_IO.New_Line;

      GNAT.OS_Lib.Spawn (Self.Git.all, List, Ok);

      if not Ok then
         raise Program_Error;
      end if;

      for J of List loop
         GNAT.OS_Lib.Free (J);
      end loop;
   end Exec;

   -------------
   -- Git_Add --
   -------------

   not overriding procedure Git_Add
     (Self : Pusher;
      File : League.Strings.Universal_String)
   is
      Args : GNAT.OS_Lib.Argument_List (1 .. 2);
   begin
      Args (1) := new String'("add");
      Args (2) := new String'(File.To_UTF_8_String);
      Self.Exec (Args);
   end Git_Add;

   ----------------
   -- Git_Commit --
   ----------------

   not overriding procedure Git_Commit
     (Self    : Pusher;
      Date    : League.Calendars.Date_Time;
      Message : League.Strings.Universal_String;
      List    : League.String_Vectors.Universal_String_Vector)
   is
      Args : GNAT.OS_Lib.Argument_List (1 .. 6 + List.Length);
      Img  : constant League.Strings.Universal_String :=
        League.Calendars.ISO_8601.Image (Format, Date);
   begin
      Args (1) := new String'("commit");
      Args (2) := new String'("-m");
      Args (3) := new String'(Message.To_UTF_8_String);
      Args (4) := new String'("--date");
      Args (5) := new String'(Img.To_UTF_8_String);
      Args (6) := new String'("--author=" & Author);

      for J in 1 .. List.Length loop
         Args (6 + J) := new String'(List.Element (J).To_UTF_8_String);
      end loop;

      Self.Exec (Args);
   end Git_Commit;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Pusher;
      Path : League.Strings.Universal_String)
   is
   begin
      Self.Path := Path;
      Ada.Directories.Set_Directory (Path.To_UTF_8_String);
      Self.Git := GNAT.OS_Lib.Locate_Exec_On_Path ("git");
   end Initialize;

   ---------------
   -- Make_Dirs --
   ---------------

   procedure Make_Dirs
     (List : League.String_Vectors.Universal_String_Vector)
   is
      Path : constant League.Strings.Universal_String := List.Join ("/");
   begin
      Ada.Directories.Create_Path (Path.To_UTF_8_String);
   end Make_Dirs;

   ----------
   -- Push --
   ----------

   procedure Push
     (Self   : in out Pusher;
      Loader : in out CvsWeb.Loaders.Loader;
      Skip   : League.Calendars.Date_Time)
   is
      use CvsWeb.Loaders.Commit_Maps;
      use type League.Calendars.Date_Time;

      procedure Fetch_File
        (File : League.Strings.Universal_String;
         Date : League.Calendars.Date_Time);

      ----------------
      -- Fetch_File --
      ----------------

      procedure Fetch_File
        (File : League.Strings.Universal_String;
         Date : League.Calendars.Date_Time)
      is
         function Get_Rev return League.Strings.Universal_String;

         -------------
         -- Get_Rev --
         -------------

         function Get_Rev return League.Strings.Universal_String is
            Changes : constant access CvsWeb.Loaders.File_Changes.Set :=
              Loader.Changes (File);
         begin
            for Change of Changes.all loop
               if Change.Date = Date then
                  return Change.Rev;
               end if;
            end loop;

            raise Constraint_Error;
         end Get_Rev;

         use type League.Strings.Universal_String;
         Output : Ada.Streams.Stream_IO.File_Type;
         Path : constant League.Strings.Universal_String := Self.Path & File;
         Rev  : constant League.Strings.Universal_String :=  Get_Rev;
         Data : constant League.Stream_Element_Vectors.Stream_Element_Vector :=
           Loader.Fetch_File (File, Rev);
      begin
         if Ada.Directories.Exists (Path.To_UTF_8_String) then
            Ada.Streams.Stream_IO.Open
              (Output,
               Ada.Streams.Stream_IO.Out_File,
               Path.To_UTF_8_String);

            Ada.Streams.Stream_IO.Write (Output, Data.To_Stream_Element_Array);
         else
            declare
               Dirs : constant League.String_Vectors.Universal_String_Vector :=
                 Path.Split ('/');
            begin
               Make_Dirs (Dirs.Slice (1, Dirs.Length - 1));

               Ada.Streams.Stream_IO.Create
                 (Output,
                  Ada.Streams.Stream_IO.Out_File,
                  Path.To_UTF_8_String);

               Ada.Streams.Stream_IO.Write
                 (Output, Data.To_Stream_Element_Array);

               Self.Git_Add (File);
            end;
         end if;
         Ada.Streams.Stream_IO.Close (Output);
      end Fetch_File;

      use type League.Strings.Universal_String;

      Commits : constant access Map := Loader.Commits;
      Date     : League.Calendars.Date_Time;
      Comment : League.Strings.Universal_String;
      Prev    : League.Strings.Universal_String;
      Cursor  : CvsWeb.Loaders.Commit_Maps.Cursor := Commits.First;
      Vector  : League.String_Vectors.Universal_String_Vector;
      Files   : League.String_Vectors.Universal_String_Vector;
   begin
      while Has_Element (Cursor) loop
         Date := Key (Cursor);

         if Date > Skip then
            Comment := Loader.Commit_Message (Date);

            if Comment /= Prev then
               if not Files.Is_Empty then
                  Self.Git_Commit (Date, Prev, Files);
                  Files.Clear;
               end if;

               Prev := Comment;
            end if;

            Vector := Element (Cursor);
            Files.Append (Vector);

            for J in 1 .. Vector.Length loop
               Fetch_File (Vector.Element (J), Date);
            end loop;
         end if;

         Next (Cursor);
      end loop;

      if not Files.Is_Empty then
         Self.Git_Commit (Date, Prev, Files);
      end if;
   end Push;

end CvsWeb.Pushers;
