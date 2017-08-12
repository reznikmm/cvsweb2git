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

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;

with League.Calendars;
with League.Stream_Element_Vectors;
with League.String_Vectors;
with League.Strings.Hash;

with CvsWeb.HTTP;

package CvsWeb.Loaders is

   type Loader is tagged limited private;

   not overriding procedure Initialize
     (Self : in out Loader;
      URL  : League.Strings.Universal_String);

   not overriding function Directories
     (Self : in out Loader;
      Path : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
           return League.String_Vectors.Universal_String_Vector;

   not overriding function Files
     (Self : in out Loader;
      Path : League.Strings.Universal_String :=
        League.Strings.Empty_Universal_String)
           return League.String_Vectors.Universal_String_Vector;

   type File_Change is record
      Rev     : League.Strings.Universal_String;
      Date    : League.Calendars.Date_Time;
      Comment : League.Strings.Universal_String;
   end record;

   function "<" (Left, Right : File_Change) return Boolean;

   package File_Changes is new Ada.Containers.Ordered_Sets
     (Element_Type => File_Change,
      "<"          => "<",
      "="          => "=");

   not overriding function Changes
     (Self : in out Loader;
      Path : League.Strings.Universal_String)
        return not null access File_Changes.Set;

   package Commit_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => League.Calendars.Date_Time,
      Element_Type => League.String_Vectors.Universal_String_Vector,
      "<"          => League.Calendars."<",
      "="          => League.String_Vectors."=");

   not overriding function Commits
     (Self : in out Loader) return not null access Commit_Maps.Map;

   not overriding function Commit_Message
     (Self : in out Loader;
      Id   : League.Calendars.Date_Time)
        return League.Strings.Universal_String;

   not overriding function Fetch_File
     (Self : in out Loader;
      Path : League.Strings.Universal_String;
      Rev  : League.Strings.Universal_String)
        return League.Stream_Element_Vectors.Stream_Element_Vector;

private

   package Vector_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => League.String_Vectors.Universal_String_Vector,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=",
      "="             => League.String_Vectors."=");

   package Changes_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => League.Strings.Universal_String,
      Element_Type    => File_Changes.Set,
      Hash            => League.Strings.Hash,
      Equivalent_Keys => League.Strings."=",
      "="             => File_Changes."=");

   package Message_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => League.Calendars.Date_Time,
      Element_Type => League.Strings.Universal_String,
      "<"          => League.Calendars."<",
      "="          => League.Strings."=");

   type Loader is tagged limited record
      URL      : League.Strings.Universal_String;
      Connect  : CvsWeb.HTTP.Connection;
      Dirs     : Vector_Maps.Map;
      Files    : Vector_Maps.Map;
      Changes  : Changes_Maps.Map;
      Messages : Message_Maps.Map;
      Commits  : aliased Commit_Maps.Map;
   end record;

end CvsWeb.Loaders;
