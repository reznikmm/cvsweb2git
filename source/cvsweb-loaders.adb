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

with Ada.Characters.Wide_Wide_Latin_1;
with League.Calendars.ISO_8601;
with XML.SAX.Attributes;
with XML.SAX.Content_Handlers;
with XML.SAX.Entity_Resolvers;
with XML.SAX.Input_Sources.Strings;
with XML.SAX.Simple_Readers;
--  with League.Text_Codecs;
--  with Ada.Wide_Wide_Text_IO;

with CvsWeb.Html2Xml;

package body CvsWeb.Loaders is

   function "+" (Text : Wide_Wide_String)
     return League.Strings.Universal_String
       renames League.Strings.To_Universal_String;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : File_Change) return Boolean is

      function "<"
        (Left, Right : League.Strings.Universal_String) return Boolean;

      ---------
      -- "<" --
      ---------

      function "<"
        (Left, Right : League.Strings.Universal_String) return Boolean
      is
         Left_Value : constant Positive :=
           Positive'Wide_Wide_Value (Left.To_Wide_Wide_String);
         Right_Value : constant Positive :=
           Positive'Wide_Wide_Value (Right.To_Wide_Wide_String);
      begin
         return Left_Value < Right_Value;
      end "<";

      Left_Rev : constant League.String_Vectors.Universal_String_Vector :=
        Left.Rev.Split ('.');

      Right_Rev : constant League.String_Vectors.Universal_String_Vector :=
        Right.Rev.Split ('.');
   begin
      for J in 1 .. Positive'Min (Left_Rev.Length, Right_Rev.Length) loop
         if Left_Rev (J) < Right_Rev (J) then
            return True;
         elsif Right_Rev (J) < Left_Rev (J) then
            return False;
         end if;
      end loop;

      return Left_Rev.Length < Right_Rev.Length;
   end "<";

   package Constants is
      subtype Text is League.Strings.Universal_String;
      A        : constant Text := +"a";
      Div      : constant Text := +"div";
      Dir_GIF  : constant Text := +"/icons/dir.gif";
      HR       : constant Text := +"hr";
      HRef     : constant Text := +"href";
      I        : constant Text := +"i";
      Img      : constant Text := +"img";
      CR       : constant Text := +(1 => Ada.Characters.Wide_Wide_Latin_1.CR);
      LF       : constant Text := +(1 => Ada.Characters.Wide_Wide_Latin_1.LF);
      Src      : constant Text := +"src";
      Text_GIF : constant Text := +"/icons/text.gif";
   end Constants;

   package Parse_Directory is

      type Content_Handler (Loader : access Loaders.Loader) is limited
        new XML.SAX.Content_Handlers.SAX_Content_Handler
            and XML.SAX.Entity_Resolvers.SAX_Entity_Resolver
          with record
             Prefix  : League.Strings.Universal_String;
             Dirs    : League.String_Vectors.Universal_String_Vector;
             Files   : League.String_Vectors.Universal_String_Vector;
             Is_Dir  : Boolean := False;
             Is_File : Boolean := False;
          end record;

      overriding procedure Start_Element
        (Self           : in out Content_Handler;
         Namespace_URI  : League.Strings.Universal_String;
         Local_Name     : League.Strings.Universal_String;
         Qualified_Name : League.Strings.Universal_String;
         Attributes     : XML.SAX.Attributes.SAX_Attributes;
         Success        : in out Boolean);

      overriding function Error_String
        (Self : Content_Handler)
         return League.Strings.Universal_String is
           (League.Strings.Empty_Universal_String);

      overriding procedure Resolve_Entity
        (Self      : in out Content_Handler;
         Name      : League.Strings.Universal_String;
         Public_Id : League.Strings.Universal_String;
         Base_URI  : League.Strings.Universal_String;
         System_Id : League.Strings.Universal_String;
         Source    : out XML.SAX.Input_Sources.SAX_Input_Source_Access;
         Success   : in out Boolean);

   end Parse_Directory;

   package Parse_Commit_List is

      type State_Kinds is
        (Start_State, Rev_State, Date_State, Before_Comment_State,
         Comment_State, Done_State);

      type Content_Handler
        (Loader : access Loaders.Loader;
         Set     : not null access File_Changes.Set) is limited
        new XML.SAX.Content_Handlers.SAX_Content_Handler
            and XML.SAX.Entity_Resolvers.SAX_Entity_Resolver
          with record
             Prefix  : League.Strings.Universal_String;
             Text    : League.Strings.Universal_String;
             Change  : File_Change;
             State   : State_Kinds := Start_State;
          end record;

      overriding procedure Characters
        (Self    : in out Content_Handler;
         Text    : League.Strings.Universal_String;
         Success : in out Boolean);

      overriding procedure End_Element
        (Self           : in out Content_Handler;
         Namespace_URI  : League.Strings.Universal_String;
         Local_Name     : League.Strings.Universal_String;
         Qualified_Name : League.Strings.Universal_String;
         Success        : in out Boolean);

      overriding procedure Start_Element
        (Self           : in out Content_Handler;
         Namespace_URI  : League.Strings.Universal_String;
         Local_Name     : League.Strings.Universal_String;
         Qualified_Name : League.Strings.Universal_String;
         Attributes     : XML.SAX.Attributes.SAX_Attributes;
         Success        : in out Boolean);

      overriding function Error_String
        (Self : Content_Handler)
         return League.Strings.Universal_String is
           (League.Strings.Empty_Universal_String);

      overriding procedure Resolve_Entity
        (Self      : in out Content_Handler;
         Name      : League.Strings.Universal_String;
         Public_Id : League.Strings.Universal_String;
         Base_URI  : League.Strings.Universal_String;
         System_Id : League.Strings.Universal_String;
         Source    : out XML.SAX.Input_Sources.SAX_Input_Source_Access;
         Success   : in out Boolean);

   end Parse_Commit_List;

   package body Parse_Directory is

      --------------------
      -- Resolve_Entity --
      --------------------

      overriding procedure Resolve_Entity
        (Self      : in out Content_Handler;
         Name      : League.Strings.Universal_String;
         Public_Id : League.Strings.Universal_String;
         Base_URI  : League.Strings.Universal_String;
         System_Id : League.Strings.Universal_String;
         Source    : out XML.SAX.Input_Sources.SAX_Input_Source_Access;
         Success   : in out Boolean)
      is
         pragma Unreferenced (Self);
         pragma Unreferenced (Name, Public_Id, Base_URI, System_Id, Source);
      begin
         Success := True;
         Source := new XML.SAX.Input_Sources.Strings.String_Input_Source;
      end Resolve_Entity;

      -------------------
      -- Start_Element --
      -------------------

      overriding procedure Start_Element
        (Self           : in out Content_Handler;
         Namespace_URI  : League.Strings.Universal_String;
         Local_Name     : League.Strings.Universal_String;
         Qualified_Name : League.Strings.Universal_String;
         Attributes     : XML.SAX.Attributes.SAX_Attributes;
         Success        : in out Boolean)
      is
         pragma Unreferenced (Success, Qualified_Name);

         use type League.Strings.Universal_String;
         Src  : League.Strings.Universal_String;
         HRef : League.Strings.Universal_String;
      begin
         if Local_Name = Constants.Img then
            Src := Attributes.Value (Namespace_URI, Constants.Src);

            if Src = Constants.Text_GIF then
               Self.Is_File := True;
            elsif Src = Constants.Dir_GIF then
               Self.Is_Dir := True;
            else
               Self.Is_File := False;
               Self.Is_Dir := False;
            end if;
         elsif Local_Name = Constants.A then
            HRef := Attributes.Value (Namespace_URI, Constants.HRef);
            HRef := HRef.Tail_From (Self.Prefix.Length + 1);

            if Self.Is_Dir then
               --  HRef.Slice (1, HRef.Length - 1);
               Self.Dirs.Append (HRef);
               Self.Is_Dir := False;
            elsif Self.Is_File then
               Self.Files.Append (HRef);
               Self.Is_File := False;
            end if;
         end if;
      end Start_Element;

   end Parse_Directory;

   package body Parse_Commit_List is

      ----------------
      -- Characters --
      ----------------

      overriding procedure Characters
        (Self    : in out Content_Handler;
         Text    : League.Strings.Universal_String;
         Success : in out Boolean)
      is
         pragma Unreferenced (Success);
      begin
         if Self.State in Date_State | Comment_State then
            Self.Text.Append (Text);
         end if;
      end Characters;

      -----------------
      -- End_Element --
      -----------------

      overriding procedure End_Element
        (Self           : in out Content_Handler;
         Namespace_URI  : League.Strings.Universal_String;
         Local_Name     : League.Strings.Universal_String;
         Qualified_Name : League.Strings.Universal_String;
         Success        : in out Boolean)
      is
         pragma Unreferenced (Namespace_URI, Qualified_Name, Success);

         use type League.Strings.Universal_String;
      begin
         if Local_Name = Constants.I and Self.State = Date_State then
            declare
               use League.Calendars.ISO_8601;

               Part : constant League.String_Vectors.Universal_String_Vector :=
                 Self.Text.Split (' ');
               Date : constant League.String_Vectors.Universal_String_Vector :=
                 Part (1).Split ('/');
               Time : constant League.String_Vectors.Universal_String_Vector :=
                 Part (2).Split (':');
            begin
               Self.Change.Date := League.Calendars.ISO_8601.Create
                 (Year           => Year_Number'Wide_Wide_Value
                                      (Date (1).To_Wide_Wide_String),
                  Month          => Month_Number'Wide_Wide_Value
                                      (Date (2).To_Wide_Wide_String),
                  Day            => Day_Number'Wide_Wide_Value
                                      (Date (3).To_Wide_Wide_String),
                  Hour           => Hour_Number'Wide_Wide_Value
                                      (Time (1).To_Wide_Wide_String),
                  Minute         => Minute_Number'Wide_Wide_Value
                                      (Time (2).To_Wide_Wide_String),
                  Second         => Second_Number'Wide_Wide_Value
                                      (Time (3).To_Wide_Wide_String),
                  Nanosecond_100 => 0);

               Self.State := Before_Comment_State;
            end;
         elsif Local_Name = Constants.Div and Self.State = Comment_State then

            while Self.Text.Starts_With (Constants.LF) or
               Self.Text.Starts_With (Constants.CR)
            loop
               Self.Text.Slice (2, Self.Text.Length);
            end loop;

            while Self.Text.Ends_With (Constants.LF) or
              Self.Text.Ends_With (Constants.CR)
            loop
               Self.Text.Slice (1, Self.Text.Length - 1);
            end loop;

            Self.Change.Comment := Self.Text;
            Self.State := Done_State;
         end if;
      end End_Element;

      --------------------
      -- Resolve_Entity --
      --------------------

      overriding procedure Resolve_Entity
        (Self      : in out Content_Handler;
         Name      : League.Strings.Universal_String;
         Public_Id : League.Strings.Universal_String;
         Base_URI  : League.Strings.Universal_String;
         System_Id : League.Strings.Universal_String;
         Source    : out XML.SAX.Input_Sources.SAX_Input_Source_Access;
         Success   : in out Boolean)
      is
         pragma Unreferenced (Self);
         pragma Unreferenced (Name, Public_Id, Base_URI, System_Id, Source);
      begin
         Success := True;
         Source := new XML.SAX.Input_Sources.Strings.String_Input_Source;
      end Resolve_Entity;

      -------------------
      -- Start_Element --
      -------------------

      overriding procedure Start_Element
        (Self           : in out Content_Handler;
         Namespace_URI  : League.Strings.Universal_String;
         Local_Name     : League.Strings.Universal_String;
         Qualified_Name : League.Strings.Universal_String;
         Attributes     : XML.SAX.Attributes.SAX_Attributes;
         Success        : in out Boolean)
      is
         pragma Unreferenced (Success, Qualified_Name);

         use type League.Strings.Universal_String;
         HRef : League.Strings.Universal_String;
      begin
         if Local_Name = Constants.HR then
            if Self.State = Done_State then
               if Self.Change.Rev.Count ('.') = 1 then
                  Self.Set.Insert (Self.Change);
               end if;

               Self.Change.Rev.Clear;
               Self.Change.Comment.Clear;
               Self.State := Rev_State;

            elsif Self.State = Start_State then
               Self.State := Rev_State;

            end if;

         elsif Local_Name = Constants.A and Self.State = Rev_State then
            HRef := Attributes.Value (Namespace_URI, Constants.HRef);
            Self.Change.Rev := HRef.Tail_From (HRef.Index ("=") + 1);

         elsif Local_Name = Constants.I and Self.State = Rev_State then
            Self.Text.Clear;
            Self.State := Date_State;

         elsif Local_Name = Constants.Div
           and Self.State = Before_Comment_State
         then
            Self.Text.Clear;
            Self.State := Comment_State;

         end if;
      end Start_Element;

   end Parse_Commit_List;

   -------------
   -- Changes --
   -------------

   not overriding function Changes
     (Self : in out Loader;
      Path : League.Strings.Universal_String)
        return not null access File_Changes.Set is
   begin
      return Self.Changes.Reference (Path).Element;
   end Changes;

   --------------------
   -- Commit_Message --
   --------------------

   not overriding function Commit_Message
     (Self : in out Loader;
      Id   : League.Calendars.Date_Time)
        return League.Strings.Universal_String is
   begin
      return Self.Messages.Element (Id);
   end Commit_Message;

   -------------
   -- Commits --
   -------------

   not overriding function Commits
     (Self : in out Loader) return not null access Commit_Maps.Map is
   begin
      return Self.Commits'Unchecked_Access;
   end Commits;

   -----------------
   -- Directories --
   -----------------

   function Directories
     (Self : in out Loader;
      Path : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
      return League.String_Vectors.Universal_String_Vector is
   begin
      return Self.Dirs.Element (Path);
   end Directories;

   ----------------
   -- Fetch_File --
   ----------------

   not overriding function Fetch_File
     (Self : in out Loader;
      Path : League.Strings.Universal_String;
      Rev  : League.Strings.Universal_String)
      return League.Stream_Element_Vectors.Stream_Element_Vector
   is
      use type League.Strings.Universal_String;

      Result : League.Stream_Element_Vectors.Stream_Element_Vector;
      Ok     : Boolean;
   begin
      Self.Connect.Get
        (URL     => Self.URL & Path & "?rev=" & Rev,
         Result  => Result,
         Success => Ok);

      if not Ok then
         raise Program_Error;
      end if;

      return Result;
   end Fetch_File;

   -----------
   -- Files --
   -----------

   not overriding function Files
     (Self : in out Loader;
      Path : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
           return League.String_Vectors.Universal_String_Vector is
   begin
      return Self.Dirs.Element (Path);
   end Files;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : in out Loader;
      URL  : League.Strings.Universal_String)
   is
      procedure Parse_Dir (Path : League.Strings.Universal_String);
      procedure Parse_Changes
        (Path : League.Strings.Universal_String;
         Set  : not null access File_Changes.Set);

      procedure Parse_Changes
        (Path : League.Strings.Universal_String;
         Set  : not null access File_Changes.Set)
      is
         use type League.Strings.Universal_String;

         Data : League.Stream_Element_Vectors.Stream_Element_Vector;
         Ok   : Boolean;
      begin
         Self.Connect.Get
           (URL     => URL & Path,
            Result  => Data,
            Success => Ok);

         if not Ok then
            raise Program_Error;
         end if;

         declare
            Text   : League.Strings.Universal_String;
            Input  : aliased XML.SAX.Input_Sources.Strings.String_Input_Source;
            Parser : aliased XML.SAX.Simple_Readers.Simple_Reader;

            Handler : aliased Parse_Commit_List.Content_Handler
              (Self'Unchecked_Access, Set);
--              Format : constant League.Strings.Universal_String :=
--               +"yyyy-MM-dd HH:mm:ss";
         begin
            Parser.Set_Content_Handler (Handler'Unchecked_Access);
            Parser.Set_Entity_Resolver (Handler'Unchecked_Access);
            Parser.Set_Input_Source (Input'Unchecked_Access);
            Text := CvsWeb.Html2Xml.Convert
              (Data => Data,
               URL  => URL & Path);
            Input.Set_String (Text);
--            Ada.Wide_Wide_Text_IO.Put_Line (Text.To_Wide_Wide_String);

            Handler.Prefix := "/cgi-bin/cvsweb.cgi/arm/" & Path;

            Parser.Parse;

            for J of Handler.Set.all loop
               if not Self.Messages.Contains (J.Date) then
                  Self.Messages.Insert (J.Date, J.Comment);

                  Self.Commits.Insert
                    (J.Date,
                     League.String_Vectors.Empty_Universal_String_Vector);

               elsif Self.Messages (J.Date) /= J.Comment then
                  raise Program_Error;

               end if;

               Self.Commits (J.Date).Append (Path);
--                 Ada.Wide_Wide_Text_IO.Put (J.Rev.To_Wide_Wide_String);
--                 Ada.Wide_Wide_Text_IO.Put (' ');
--                 Ada.Wide_Wide_Text_IO.Put
--                   (League.Calendars.ISO_8601.Image
--                      (Format, J.Date).To_Wide_Wide_String);
--                 Ada.Wide_Wide_Text_IO.Put (' ');
--              Ada.Wide_Wide_Text_IO.Put_Line (J.Comment.To_Wide_Wide_String);
            end loop;
         end;
      end Parse_Changes;

      procedure Parse_Dir (Path : League.Strings.Universal_String) is
         use type League.Strings.Universal_String;

         Data : League.Stream_Element_Vectors.Stream_Element_Vector;
         Ok   : Boolean;
      begin
         Self.Connect.Get
           (URL     => URL & Path,
            Result  => Data,
            Success => Ok);

         if not Ok then
            raise Program_Error;
         end if;

         declare
            Text   : League.Strings.Universal_String;
            Input  : aliased XML.SAX.Input_Sources.Strings.String_Input_Source;
            Parser : aliased XML.SAX.Simple_Readers.Simple_Reader;

            Handler : aliased Parse_Directory.Content_Handler
              (Self'Unchecked_Access);
         begin
            Parser.Set_Content_Handler (Handler'Unchecked_Access);
            Parser.Set_Entity_Resolver (Handler'Unchecked_Access);
            Parser.Set_Input_Source (Input'Unchecked_Access);
            Text := CvsWeb.Html2Xml.Convert
              (Data => Data,
               URL  => URL & Path);
            Input.Set_String (Text);
--            Ada.Wide_Wide_Text_IO.Put_Line (Text.To_Wide_Wide_String);

            Handler.Prefix := "/cgi-bin/cvsweb.cgi/arm/" & Path;

            Parser.Parse;

            Self.Dirs.Insert (Path, Handler.Dirs);
            Self.Files.Insert (Path, Handler.Files);

            for J in 1 .. Handler.Dirs.Length loop
               Parse_Dir (Path & Handler.Dirs.Element (J));
            end loop;

            for J in 1 .. Handler.Files.Length loop
               declare
                  File : constant League.Strings.Universal_String :=
                    Path & Handler.Files.Element (J);
               begin
                  Self.Changes.Insert (File, File_Changes.Empty_Set);
                  Parse_Changes (File, Self.Changes.Reference (File).Element);
               end;
            end loop;
         end;
      end Parse_Dir;
   begin
      Self.URL := URL;
      Parse_Dir (League.Strings.Empty_Universal_String);
   end Initialize;

end CvsWeb.Loaders;
