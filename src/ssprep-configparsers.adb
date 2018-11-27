--  ---------------------------------------------------------------------------
--  Copyright 2008 Per Sandberg  <per.s.sandberg@bahnhof.se>
--  ---------------------------------------------------------------------------

--  $Id$

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--  ---------------------------------------------------------------------------
with Ada.Calendar;
with Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Calendar.Time_IO;
with GNAT.Case_Util;
with GNAT.Regpat;
with GNAT.Spitbol.Table_Boolean;
with Ada.Strings.Unbounded;

package body Ssprep.ConfigParsers is
   use Ada.Strings.Unbounded;
   ---------------
   -- To_String --
   ---------------
   Boolean_Map : GNAT.Spitbol.Table_Boolean.Table (10);

   procedure Initialize is
      use GNAT.Spitbol.Table_Boolean;
   begin
      Set (Boolean_Map, "0", False);
      Set (Boolean_Map, "no", False);
      Set (Boolean_Map, "false", False);
      Set (Boolean_Map, "off", False);
      Set (Boolean_Map, "1", True);
      Set (Boolean_Map, "yes", True);
      Set (Boolean_Map, "true", True);
      Set (Boolean_Map, "on", True);
   end Initialize;

   function To_String (Item : VString_Table_Access) return String is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return To_String (Item);
   end To_String;

   --------------
   -- Sections --
   --------------

   function Sections
     (This : RawConfigParser)
      return Table_Table_VString.Table
   is
   begin
      return This.Data;
   end Sections;

   --------------
   -- Sections --
   --------------

   function Sections
     (This : RawConfigParser)
      return Table_Table_VString.Table_Array
   is
   begin
      return Table_Table_VString.Convert_To_Array (This.Sections);
   end Sections;

   --------------
   -- Sections --
   --------------

   function Sections (This : RawConfigParser) return String_Vectors.Vector is
      Ret  : String_Vectors.Vector;
      Temp : constant Table_Table_VString.Table_Array := This.Sections;
   begin
      for I in Temp'Range loop
         Ret.Append (S (Temp (I).Name));
      end loop;
      return Ret;
   end Sections;

   -----------------
   -- Add_Section --
   -----------------

   procedure Add_Section (This : in out RawConfigParser; Section : String) is
      Null_Data : constant VString_Table_Access := new Table_VString.Table (32);

   begin
      Table_Table_VString.Set (This.Data, Section, Null_Data);
   end Add_Section;

   -----------------
   -- Has_Section --
   -----------------

   function Has_Section
     (This    : RawConfigParser;
      Section : String)
      return Boolean
   is
   begin
      return Table_Table_VString.Present (This.Data, Section);
   end Has_Section;

   -------------
   -- Options --
   -------------

   function Options
     (This    : RawConfigParser;
      Section : String)
      return String_Vectors.Vector
   is
      Temp : constant Table_VString.Table_Array := This.Options (Section);
      Ret  : String_Vectors.Vector;
   begin
      for I in Temp'Range loop
         Ret.Append (S (Temp (I).Name));
      end loop;
      return Ret;
   end Options;

   function Options
     (This    : RawConfigParser;
      Section : String)
      return Table_VString.Table_Array
   is
   begin
      return Table_VString.Convert_To_Array (This.Options (Section));
   end Options;
   -------------
   -- Options --
   -------------

   function Options
     (This    : RawConfigParser;
      Section : String)
      return Table_VString.Table
   is
   begin
      if Section = "" then
         return This.Default;
      else
         return Table_Table_VString.Get (This.Data, Section).all;
      end if;
   end Options;

   -----------------
   -- Has_Section --
   -----------------

   function Has_Option
     (This    : RawConfigParser;
      Section : String;
      Option  : String)
      return Boolean
   is
   begin
      if Table_Table_VString.Present (This.Data, Section) then
         return Table_VString.Present (Table_Table_VString.Get (This.Data, Section).all, Option);
      end if;
      return False;
   end Has_Option;

   ----------
   -- Read --
   ----------

   procedure Read (This : in out RawConfigParser; Filename : String; Ignore_Nonexisting : Boolean := True) is
      F : Ada.Text_IO.File_Type;
   begin
      if Ignore_Nonexisting and
        not Ada.Directories.Exists (Filename)
      then
         return;
      end if;

      Open (F, In_File, Filename);
      This.Read (F);
      Close (F);

   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (This      : in out RawConfigParser;
      Filenames : String_Vectors.Vector)
   is
      procedure Read (Position : String_Vectors.Cursor) is
      begin
         This.Read (String_Vectors.Element (Position));
      end Read;
   begin
      Filenames.Iterate (read'Access);
   end Read;

   ----------
   -- Read --
   ----------

   procedure Read
     (This      : in out RawConfigParser;
      File      : Ada.Text_IO.File_Type)
   is
      use GNAT.Regpat;
      Matcher      : constant Pattern_Matcher :=
                       Compile ("^\b*((.*)[:=](.*)|\[(.*)\])");
      Matches      :  Match_Array (1 .. Paren_Count (Matcher));
      Continue     : Boolean := False;
      Section      : VString;
      Long_Line    : VString;

      procedure Parse (Line : String) is
         L : constant String := Ada.Strings.Fixed.Trim (Line, Ada.Strings.Both);
      begin
         if L'Length <= 1 then
            return;
         elsif L (L'First) = '#' or L (L'First) = ';' then
            return;
         elsif L'Length > 1 and L (L'First .. L'First + 1) = "--" then
            return;
         end if;

         if L (L'Last) = '\' then
            Continue := True;
            Append (Long_Line, L (L'First .. L'Last - 1));
            Append (Long_Line, " ");
         else
            Continue := False;
            Append (Long_Line, L (L'First .. L'Last));
         end if;

         if not Continue then
            declare
               L : constant String := S (Long_Line);
            begin
               Match (Matcher, L, Matches);
               Long_Line := V ("");
               if Matches (2) /= No_Match then
                  This.Set (S (Section),
                            Trim (L (Matches (2).First .. Matches (2).Last), Both),
                            Trim (L (Matches (3).First .. Matches (3).Last), Both));
               elsif Matches (4) /= No_Match then
                  Section := V (L (Matches (4).First .. Matches (4).Last));
                  This.Add_Section (S (Section));
               end if;
            end;
         end if;
      end Parse;
   begin
      while not Ada.Text_IO.End_Of_File (File) loop
         Parse (Ada.Text_IO.Get_Line (File));
      end loop;
   end Read;

   ---------
   -- Get --
   ---------

   Variable_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
                        GNAT.Regpat.Compile ("^.*([\$%]\((\w+)\)).*");

   function Get
     (This    : RawConfigParser;
      Section : String;
      Option  : String)
      return String
   is
      function Expand (S : String) return String is
         Matches : GNAT.Regpat.Match_Array (1 .. GNAT.Regpat.Paren_Count (Variable_Matcher));
         use GNAT.Regpat;
      begin
         Match (Variable_Matcher, S, Matches);
         if Matches (1) /= No_Match then
            return Expand (S (S'First .. Matches (1).First - 1) &
                           String'(This.Get (Section, S (Matches (2).First .. Matches (2).Last))) &
                           S (Matches (1).Last + 1 .. S'Last));
         else
            return S;
         end if;
      end Expand;
   begin
      if Section = "" then
         return Expand (S (Table_VString.Get (This.Default, Option)));
      else
         return Expand (S (Table_VString.Get (Table_Table_VString.Get (This.Data, Section).all, Option)));
      end if;
   exception
      when others =>
         raise NoOptionError with  Section & "." & Option & " Not Found";
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This    : RawConfigParser;
      Section : String;
      Option  : String)
      return Integer
   is
      Temp : constant String := This.Get (Section, Option);
   begin
      return Integer'Value (Temp);
   exception
      when others =>
         raise ValueError with "[" & Section & "]." & Option & "='" & Temp & "' Does not resolve to a Integer";
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This    : RawConfigParser;
      Section : String;
      Option  : String)
      return Float
   is
      Temp : constant String := This.Get (Section, Option);
   begin
      return Float'Value (Temp);
   exception
      when others =>
         raise ValueError with "[" & Section & "]." & Option & "='" & Temp & "' Does not resolve to a Float";
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This    : RawConfigParser;
      Section : String;
      Option  : String)
      return Long_Float
   is
      Temp : constant String := This.Get (Section, Option);
   begin
      return Long_Float'Value (Temp);
   exception
      when others =>
         raise ValueError with "[" & Section & "]." & Option & "='" & Temp & "' Does not resolve to a Long_Float";
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (This    : RawConfigParser;
      Section : String;
      Option  : String)
      return Boolean
   is
      Temp : String := This.Get (Section, Option);
      use GNAT.Spitbol.Table_Boolean;
   begin
      GNAT.Case_Util.To_Lower (Temp);
      if Present (Boolean_Map, Temp) then
         return Get (Boolean_Map, Temp);
      else
         raise ValueError with "[" & Section & "]." & Option & "='" & Temp & "' Does not resolve to a boolean";
      end if;
   end Get;

   -----------
   -- Items --
   -----------

   function Items
     (This    : RawConfigParser;
      Section : String)
      return Table_VString.Table_Array
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return Items (This, Section);
   end Items;

   -----------
   -- Items --
   -----------

   function Items
     (This    : RawConfigParser;
      Section : String)
      return Table_VString.Table
   is
   begin
      --  Generated stub: replace with real body!
      raise Program_Error;
      return Items (This, Section);
   end Items;

   ---------
   -- Set --
   ---------

   procedure Set
     (This    : in out RawConfigParser;
      Section : String;
      Option  : String;
      Value   : String)
   is
   begin
      if Section = "" then
         Table_VString.Set (This.Default, Option, V (Value));
      else
         if not This.Has_Section (Section) then
            raise NoSectionError with Section & " does not exists.";
         end if;
         Table_VString.Set (Table_Table_VString.Get (This.Data, Section).all, Option, V (Value));
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (This    : in out RawConfigParser;
      Section : String;
      Option  : String;
      Value   : Integer)
   is
   begin
      This.Set (Section, Option, Value'Img);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (This    : in out RawConfigParser;
      Section : String;
      Option  : String;
      Value   : Boolean)
   is
   begin
      This.Set (Section, Option, Value'Img);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (This    : in out RawConfigParser;
      Section : String;
      Option  : String;
      Value   : Float)
   is
   begin
      This.Set (Section, Option, Value'Img);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (This    : in out RawConfigParser;
      Section : String;
      Option  : String;
      Value   : Long_Float)
   is
   begin
      This.Set (Section, Option, Value'Img);
   end Set;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (This                             : in RawConfigParser;
      Handle_Section                   : not null access procedure
        (This           : in RawConfigParser;
         Section : String) := Null_Handle_Section'Access;
      Handle_Value                     : not null access procedure
        (This           : in RawConfigParser;
         Section, Option, Value : String) := Null_Handle_Value'Access)
   is
      procedure Internal_Handle_Value (Section, Option, Value : String) is
      begin
         Handle_Value (This, Section, Option, Value);
      end Internal_Handle_Value;

      procedure Internal_Handle_Section (Section : VString;
                                         Data    : access Table_VString.Table) is
         Temp_Data : constant Table_VString.Table_Array := Table_VString.Convert_To_Array (Data.all);

      begin
         Handle_Section (This, S (Section));
         for I in Temp_Data'Range loop
            Internal_Handle_Value (S (Section), S (Temp_Data (I).Name), S (Temp_Data (I).Value));
         end loop;
      end Internal_Handle_Section;

      Temp_Sections : constant Table_Table_VString.Table_Array := This.Sections;
      Default : constant Table_VString.Table_Array := Table_VString.Convert_To_Array (This.Default);
   begin
      for I in Default'Range loop
         Handle_Value (This, "", S (Default (I).Name), S (Default (I).Value));
      end loop;
      for I in Temp_Sections'Range loop
         Internal_Handle_Section (Temp_Sections (I).Name,
                                  Temp_Sections (I).Value);
      end loop;
   end Iterate;

   -----------
   -- Write --
   -----------

   procedure Write
     (This       : in RawConfigParser;
      File       : in Ada.Text_IO.File_Type;
      Header     : Boolean := False)
   is
      procedure Handle_Section (This : in RawConfigParser; Section : String) is
         pragma Unreferenced (This);
      begin
         New_Line (File);
         Put_Line (File, "[" &  Section & "]");
      end Handle_Section;
      procedure Handle_Value (This : in RawConfigParser; Section, Option, Value : String) is
         pragma Unreferenced (Section, This);
      begin
         Put_Line (File, Option & ": " & Value);
      end Handle_Value;

   begin
      if Header then
         Put_Line (File, "# Auto Saved Time : " &
                   GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%Y-%m-%d %T"));
         Put_Line (File, "#            Path : '" & Name (File) & "'");
         New_Line (File);
      end if;

      This.Iterate (Handle_Section'Access,
                    Handle_Value'Access);
   end Write;

   -----------
   -- Write --
   -----------

   procedure Write
     (This       : in RawConfigParser;
      File       : in String)
   is
      F : Ada.Text_IO.File_Type;
   begin
      Create (F, Out_File, File);
      This.Write (F);
      Close (F);
   end Write;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (This    : in out RawConfigParser;
      Section : String;
      Option  : String)
   is
   begin
      Table_VString.Delete (Table_Table_VString.Get (This.Data, Section).all, Option);
   end Remove;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (This    : in out RawConfigParser;
      Section : String)
   is
   begin
      Table_Table_VString.Delete (This.Data, Section);
   end Remove;
begin
   Initialize;
end Ssprep.ConfigParsers;
