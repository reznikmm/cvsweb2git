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

with Interfaces.C.Strings;
with Ada.Streams;
with System;

package body CvsWeb.Html2Xml is

   use Interfaces;

   type Doc_Ptr is access all C.int;

   procedure xmlDocDumpMemory
     (Doc    : Doc_Ptr;
      Buffer : out C.Strings.chars_ptr;
      Size   : out C.int);
   pragma Import (C, xmlDocDumpMemory, "xmlDocDumpMemory");

   function htmlReadMemory
     (Buffer   : System.Address;
      Size     : C.int;
      URL      : C.Strings.chars_ptr;
      Encoding : C.Strings.chars_ptr;
      Options  : C.int) return Doc_Ptr;
   pragma Import (C, htmlReadMemory, "htmlReadMemory");

   HTML_PARSE_RECOVER : constant := 1;  -- Relaxed parsing
--     HTML_PARSE_NODEFDTD : constant := 4;
--     -- do not default a doctype if not found
   HTML_PARSE_NOERROR : constant := 32;  -- suppress error reports
   HTML_PARSE_NOWARNING : constant := 64;  -- suppress warning reports
--      HTML_PARSE_PEDANTIC : constant := 128;  -- pedantic error reporting
--      HTML_PARSE_NOBLANKS : constant := 256;  -- remove blank nodes
--      HTML_PARSE_NONET : constant := 2048;  -- Forbid network access
--     HTML_PARSE_NOIMPLIED : constant := 8192;
--     -- Do not add implied html/body... elements
--      HTML_PARSE_COMPACT : constant := 65536;  -- compact small text nodes
--     HTML_PARSE_IGNORE_ENC : constant := 2097152;
--     -- ignore internal document encoding hint

   procedure free (Address : C.Strings.chars_ptr);
   pragma Import (C, free, "free");

   procedure xmlFreeDoc (Cur : Doc_Ptr);
   pragma Import (C, xmlFreeDoc, "xmlFreeDoc");

   pragma Linker_Options ("-lxml2");

   -------------
   -- Convert --
   -------------

   function Convert
     (Data : League.Stream_Element_Vectors.Stream_Element_Vector;
      URL  : League.Strings.Universal_String)
      return League.Strings.Universal_String
   is
      use type C.int;

      Doc    : Doc_Ptr;
      C_URL  : C.Strings.chars_ptr :=
        C.Strings.New_String (URL.To_UTF_8_String);
      Buffer : aliased Ada.Streams.Stream_Element_Array :=
        Data.To_Stream_Element_Array;
      Result : C.Strings.chars_ptr;
      Size   : C.int;
   begin
      Doc := htmlReadMemory
        (Buffer   => Buffer (Buffer'First)'Address,
         Size     => Buffer'Length,
         URL      => C_URL,
         Encoding => C.Strings.Null_Ptr,
         Options  => HTML_PARSE_RECOVER
                   + HTML_PARSE_NOERROR
                   + HTML_PARSE_NOWARNING);

      xmlDocDumpMemory (Doc, Result, Size);

      return Value : constant League.Strings.Universal_String :=
        League.Strings.From_UTF_8_String
          (C.Strings.Value (Result, C.size_t (Size)))
      do
         free (Result);
         xmlFreeDoc (Doc);
         C.Strings.Free (C_URL);
      end return;
   end Convert;

end CvsWeb.Html2Xml;
