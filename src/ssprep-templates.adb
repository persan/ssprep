--  ---------------------------------------------------------------------------
--  Copyright 2008 Per Sandberg  <per.sandberg@bredband.net>
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



with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers;  use Ada.Containers;
with Ada.Directories;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with DOM.Core.Nodes; use DOM.Core; use DOM.Core.Nodes;
with DOM.Readers;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Regpat;
with GNAT.Spitbol; use GNAT.Spitbol;
with GNAT.String_Split;
with GNAT.Strings;

with Input_Sources.File;
with Input_Sources.Strings;

with Sax.Readers;
with Ssprep.Utilities; use Ssprep.Utilities;
with Templates_Parser.XML;

with Unicode.CES.Basic_8bit;
with Unicode.CES.Utf8;

package body Ssprep.Templates is


   function Get_Errors_Found (This : Templates) return Boolean is
   begin
      return This.Errors_Found;
   end Get_Errors_Found;

   procedure Set_Break_On_Error (This : in out Templates; breake :  Boolean) is
   begin
      This.Abort_On_Error := breake;
   end Set_Break_On_Error;


   procedure Error (Node : in DOM.Core.Node; Message : in String) is
      Name : constant String := Local_Name (Node);
   begin
      raise Constraint_Error with Name & " - " & Message;
   end Error;


   function Containing_Directory (S : String) return String is
   begin
      for I in reverse S'Range loop
         if S (I) = GNAT.OS_Lib.Directory_Separator then
            return S (S'First .. I);
         end if;
      end loop;
      return "";
   end Containing_Directory;
   procedure Add_Template
     (This : in out Templates;
      Name : String;
      Path : Template'Class) is
   begin
      if not This.Sources.Contains (Name) then
         This.Sources.Include (Name, Path);
      end if;
   end Add_Template;

   procedure Add_Template
     (This : in out Templates;
      Name : String; Path : String) is
      T : Template;
   begin
      T.Valid := True;
      T.Locations.Include (Path);
      This.Add_Template (Name, T);
   end Add_Template;

   ---------------
   -- Get_Value --
   ---------------
   function Get_Value (N : in DOM.Core.Node) return String is
      use Unicode.CES;
   begin
      if N = null then
         return "";
      else
         return Basic_8bit.From_Utf32 (Utf8.To_Utf32 (Node_Value (N)));
      end if;
   end Get_Value;


   function Get_Symbols (This  : in Templates) return not null access constant
     Templates_Parser.Translate_Set is
   begin
      return This.Symbols'Unrestricted_Access;
   end Get_Symbols;

   procedure Read (this : access Boolean_Variable; from : DOM.Core.Node) is
   --      C    : DOM.Core.Node := First_Child (N);
   begin
      null;
   end Read;

   procedure Read (this : access File_Selection_Variable; from : DOM.Core.Node) is
   begin
      null;
   end Read;

   procedure Read (this : access Selection_Variable; from : DOM.Core.Node) is
   begin
      null;
   end Read;

   procedure Parse_Variable (This      : in out Template;
                             N         : DOM.Core.Node;
                             V_Name    : String;
                             V_Type    : String;
                             V_Prompt  : String;
                             V_ToolTip : String) is
      V  : Any_Variable;
   begin
      if V_Type = "boolean" then
         V := new Boolean_Variable;
      elsif V_Type = "string" then
         V := new String_Variable;
      elsif V_Type = "file" then
         V := new File_Selection_Variable;
      elsif V_Type = "direcory" then
         V := new Dir_Selection_Variable;
      elsif V_Type = "selection" then
         V := new Selection_Variable;
      elsif V_Type = "project" then
         V := new Project_Variable;
      else
         null;
      end if;
      V.Name := new String'(V_Name);
      V.Prompt := new String'(V_Prompt);
      V.ToolTip := new String'(V_ToolTip);
      V.Read (N);
      This.Variables.Append (V);
   end Parse_Variable;

   procedure Parse_Variables (This : in out Template;
                              N    : in DOM.Core.Node) is
      C    : DOM.Core.Node := First_Child (N);
   begin
      while C /= null loop
         declare
            N_Name         : constant String := Local_Name (C);
            N_Attributes   : constant Named_Node_Map := DOM.Core.Nodes.Attributes (C);
            V_Name         : constant String := To_Lower (Get_Value (DOM.Core.Nodes.Get_Named_Item (N_Attributes, "name")));
            V_Type         : constant String := To_Lower (Get_Value (DOM.Core.Nodes.Get_Named_Item (N_Attributes, "type")));
            V_Prompt       : constant String := To_Lower (Get_Value (DOM.Core.Nodes.Get_Named_Item (N_Attributes, "prompt")));
            V_Hint         : constant String := To_Lower (Get_Value (DOM.Core.Nodes.Get_Named_Item (N_Attributes, "tooltip")));
         begin
            if N_Name = "variable" then
               This.Parse_Variable (C, V_Name, V_Type, V_Prompt, V_Hint);
            elsif N_Name = "" then
               null;
            else
               Error (C, "variable expected, found " & N_Name);
            end if;
         end;
         C := Next_Sibling (C);
      end loop;
   end Parse_Variables;

   procedure Parse_Template (This        : in out Templates;
                             N           : in DOM.Core.Node) is
      C               : DOM.Core.Node := First_Child (N);
      Name            : Ada.Strings.Unbounded.Unbounded_String;
      T               : Template;
      Locations_Found : Boolean := False;
   begin
      while C /= null loop
         declare
            N_Name  : constant String := To_Lower (Local_Name (C));
            N_Value : constant String := Get_Value (DOM.Core.Nodes.First_Child (C));
         begin
            if N_Name = "name" then
               Name := V (N_Value);
            elsif N_Name = "location" then
               T.Locations.Include (Normalize_Pathname (N_Value));
               Locations_Found := True;

            elsif N_Name = "alias" then
               T.Alias := new String'(N_Value);

            elsif N_Name = "prompt" then
               T.Prompt := new String'(N_Value);

            elsif N_Name = "simple" then
               T.Simple := Boolean'Value (N_Value);

            elsif N_Name = "helper" then
               T.Simple := Boolean'Value (N_Value);

            elsif N_Name = "prompt" then
               T.Prompt := new String'(N_Value);

            elsif N_Name = "hint" then
               T.Hint := new String'(N_Value);

            elsif N_Name = "tooltip" then
               T.ToolTip := new String'(N_Value);

            elsif N_Name = "class" then
               T.Class := new String'(N_Value);

            elsif N_Name = "path" then
               T.Path := new String'(N_Value);

            elsif N_Name = "output" then
               T.Output := new String'(N_Value);

            elsif N_Name = "load" then
               T.Load := new String'(N_Value);

            elsif N_Name = "variables" then
               T.Parse_Variables (C);
            elsif N_Name = "" then
               null;
            else
               Error (C, "Unable to parse: " & N_Name);
            end if;
         end;
         C := Next_Sibling (C);
      end loop;
      if Length (Name) /= 0 and Locations_Found then
         T.Valid := True;
         This.Add_Template (S (Name), T);
      end if;
   end Parse_Template;

   procedure add_Variables (This        : in out Templates;
                            N           : in DOM.Core.Node) is
      pragma Unreferenced (This);
      C : DOM.Core.Node;
   begin
      C := First_Child (N);
      while C /= null loop
         declare
            Name    : constant String := To_Lower (Local_Name (C));
         begin
            if Name = "variable" then
               null;
            elsif Name = "" then
               null;
            end if;
         end;
         C := Next_Sibling (C);
      end loop;
   end add_Variables;

   procedure Parse_Templates (This        : in out Templates;
                              N           : in DOM.Core.Node) is
      C : DOM.Core.Node;
   begin
      C := First_Child (N);
      while C /= null loop
         declare
            Name    : constant String := To_Lower (Local_Name (C));
            N_Value : constant String := Get_Value (DOM.Core.Nodes.First_Child (C));
         begin
            if Name = "template" then
               This.Parse_Template (C);
            elsif Name = "link" then
               This.Add_Directory (Expand_Path (N_Value, Both));
            elsif Name = "link_file" then
               This.Add_Config_File (Expand_Path (N_Value, Both));
            elsif Name = "symbols" then
               declare
                  T : Templates_Parser.Translate_Set;
               begin
                  T := Templates_Parser.XML.Load (Expand_Path (N_Value, Both));
                  Templates_Parser.Insert (This.Symbols, T);
               end;
            elsif Name = "variables" then
               This.add_Variables (C);
            elsif Name = "" then
               null;
            end if;
         end;
         C := Next_Sibling (C);
      end loop;
   end Parse_Templates;

   procedure Parse_Document
     (This        : in out Templates;
      Doc         : in DOM.Core.Node) is
      C : DOM.Core.Node;
   begin
      C := First_Child (Doc);
      while C /= null loop
         declare
            Name : constant String := To_Lower (Local_Name (C));
         begin
            if Name = "ssprep" then
               This.Parse_Ssprep (C);
            end if;
         end;
         C := Next_Sibling (C);
      end loop;
   end Parse_Document;

   procedure Parse_Ssprep
     (This        : in out Templates;
      Doc         : in DOM.Core.Node) is
      C : DOM.Core.Node;
   begin
      C := First_Child (Doc);
      while C /= null loop
         declare
            Name : constant String := To_Lower (Local_Name (C));
         begin
            if Name = "templates" then
               This.Parse_Templates (C);
            elsif Name = "" then
               null;
            else
               Error (C, "templates  expected, found" & Name);
            end if;
         end;
         C := Next_Sibling (C);
      end loop;
   end Parse_Ssprep;


   procedure Add_Config_File
     (This        : in out Templates;
      Config_File : String)
   is
      use DOM.Readers;
      Reader    : Tree_Reader;
      Input     : Input_Sources.File.File_Input;
      Doc       : DOM.Core.Document;
      Temp_Dir  : constant String := Get_Current_Dir;
   begin
      if not GNAT.Spitbol.Table_Boolean.Present (This.Config_Files, Config_File) then
         GNAT.Spitbol.Table_Boolean.Set (This.Config_Files, Config_File, True);
         if This.Verbose then
            Ada.Text_IO.Put_Line ("Add_Config_File :" & Config_File);
         end if;
         if not Is_Readable_File (Config_File) then
            Ada.Text_IO.Put_Line (Config_File & ":0:0 Does not exist");
            This.Errors_Found := True;
            if This.Abort_On_Error then
               raise Ada.IO_Exceptions.Name_Error with "'" & Config_File & "' does not exist.";
            end if;
         end if;
         This.PATH.Append (Containing_Directory (Normalize_Pathname (Config_File)));
         Change_Dir
           (Containing_Directory (Normalize_Pathname (Config_File)));
         declare
         begin
            Input_Sources.File.Open (Config_File, Input);
            Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
            Set_Feature (Reader, Sax.Readers.Namespace_Feature, True);
            Parse (Reader, Input);
            Input_Sources.File.Close (Input);
            Doc := Get_Tree (Reader);
            This.Parse_Document (Doc);
            Free (Doc);
         exception
            when e : Sax.Readers.XML_Fatal_Error  =>
               Put_Line (Ada.Text_IO.Standard_Error,
                         Config_File & First_Line (Ada.Exceptions.Exception_Message (e)));
               This.Errors_Found := True;
               if This.Abort_On_Error then
                  raise;
               end if;

         end;

         if Temp_Dir (Temp_Dir'Last) = ':' then
            Change_Dir (Temp_Dir & Directory_Separator);
         else
            Change_Dir (Temp_Dir);
         end if;
      end if;
   end Add_Config_File;

   procedure Add_String (This : in out Templates;
                         Data : String) is
      use DOM.Readers;
      Reader    : Tree_Reader;
      Input     : Input_Sources.Strings.String_Input;
      Doc       : DOM.Core.Document;
   begin
      Input_Sources.Strings.Open (Str      => Data,
                                  Encoding => Unicode.CES.Utf8.Utf8_Encoding,
                                  Input    => Input);
      Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Set_Feature (Reader, Sax.Readers.Namespace_Feature, True);
      Parse (Reader, Input);
      Input_Sources.Strings.Close (Input);
      Doc := Get_Tree (Reader);
      This.Parse_Document (Doc);
      Free (Doc);
   end Add_String;

   procedure Add_Directory (This      : in out Templates;
                            Directory : String) is
      Dir        : Dir_Type;
      Buffer     : String (1 .. 256);
      Last       : Natural;
   begin
      if Directory'Length > 0 and then Is_Directory (Directory) then
         Open (Dir, Directory);
         while True loop
            Read (Dir, Buffer, Last);
            exit when Last = 0;
            declare
               Name : constant String := Buffer (Buffer'First .. Last);
            begin
               if File_Extension (Name) = ".xml" then
                  This.Add_Config_File (Directory & GNAT.OS_Lib.Directory_Separator & Name);
               end if;
            end;
         end loop;
         Close (Dir);
      end if;
   end Add_Directory;


   procedure Add_Path (This : in out Templates; Path : String) is
      Dirs : GNAT.String_Split.Slice_Set;
      use GNAT.String_Split;
   begin
      Create (Dirs, Path, GNAT.OS_Lib.Path_Separator & "");
      for I in 1 .. GNAT.String_Split.Slice_Count (Dirs) loop
         This.Add_Directory (Slice (Dirs, I));
      end loop;
   end Add_Path;

   --------------------------------------------------------------------------------
   --------------------------------------------------------------------------------
   procedure Put_Element (Tag    : String;
                          Value  : access String;
                          Indent : String := "") is
   begin
      if Value /= null and then Value'Length > 0 then
         Put_Line (Indent & "<" & Tag & ">" & Value.all & "</" & Tag & ">");
      end if;
   end Put_Element;

   procedure Put_Element (Tag    : String;
                          Value  : String;
                          Indent : String := "") is
   begin
      if Value /= "" then
         Put_Line (Indent & "<" & Tag & ">" & Value & "</" & Tag & ">");
      end if;
   end Put_Element;

   procedure Put_Element (This   : Variable;
                          Tag    : String;
                          Value  : access String;
                          indent : String := "") is
      pragma Unreferenced (This);
   begin
      Put_Element (Tag, Value, indent);
   end Put_Element;

   procedure Put_Element (This : Variable; Tag : String; Value : String; indent : String := "") is
      pragma Unreferenced (This);
   begin
      Put_Element (Tag, Value, indent);
   end Put_Element;

   procedure Put_Variable_Heading (This : Variable; V_Type : String; indent : String := "") is
   begin
      Put_Line (indent & "<variable name=""" & This.Name.all & """ type=""" & V_Type & """>");
      Put_Element ("tooltip", This.ToolTip, indent & "  ");
      Put_Element ("prompt", This.Prompt, indent & "  ");
   end Put_Variable_Heading;

   procedure Put_Variable_Footing (This : Variable; Indent : String := "") is
      pragma Unreferenced (This);
   begin
      Put_Line (Indent & "</variable>");
   end Put_Variable_Footing;

   procedure Dump_XML (This : String_Variable; indent : String := "") is
      L_Indent : constant String := indent & "  ";
   begin
      This.Put_Variable_Heading ("string", L_Indent);
      This.Put_Variable_Footing (L_Indent);
   end Dump_XML;
   procedure Dump_XML (This : Project_Variable; indent : String := "") is
      L_Indent : constant String := indent & "  ";
   begin
      This.Put_Variable_Heading ("project", L_Indent);
      This.Put_Variable_Footing (L_Indent);
   end Dump_XML;

   procedure Dump_XML (This : File_Selection_Variable; indent : String := "") is
      L_Indent : constant String := indent & "  ";
   begin
      This.Put_Variable_Heading ("file", L_Indent);
      This.Put_Variable_Footing (L_Indent);
   end Dump_XML;

   procedure Dump_XML (This : Dir_Selection_Variable; indent : String := "") is
      L_Indent : constant String := indent & "  ";
   begin
      This.Put_Variable_Heading ("dir", L_Indent);
      This.Put_Element ("must_exist", This.Must_exist'Img, L_Indent);
      This.Put_Variable_Footing (L_Indent);
   end Dump_XML;

   procedure Dump_XML (This : Boolean_Variable; indent : String := "") is
   begin
      This.Put_Variable_Heading ("boolean", indent & "  ");
      This.Put_Variable_Footing (indent & "  ");
   end Dump_XML;

   procedure dump_xml (This : Selection_Variable; indent : String := "") is
   begin
      This.Put_Variable_Heading ("selection", indent & "  ");
      This.Put_Variable_Footing (indent & "  ");
   end dump_xml;

   procedure Dump (This     : in Templates;
                   Put_Line : not null access procedure (Item : String);
                   XML      : Boolean := False) is
      procedure Process_I (Key     : String;
                           Element : Template'Class);

      procedure Process_I (Key     : String;
                           Element : Template'Class) is
         Prefix : String := Key & " => ";
         procedure Process_II (Cursor : String_Sets.Cursor);

         procedure Process_II (Cursor : String_Sets.Cursor) is

         begin
            Put_Line (Prefix & String_Sets.Element (Cursor));
            Prefix := (others => ' ');
         end Process_II;
      begin
         Element.Locations.Iterate (Process_II'Access);
      end Process_I;

      procedure Process (Position : Template_Storages.Cursor);
      procedure Process (Position : Template_Storages.Cursor) is
      begin
         Template_Storages.Query_Element (Position, Process_I'Access);
      end Process;

      procedure DumpXML_Variable (Cursor : Variable_Vectors.Cursor);
      procedure DumpXML_Variable (Cursor : Variable_Vectors.Cursor) is

      begin
         Variable_Vectors.Element (Cursor).Dump_XML ("       ");
      end DumpXML_Variable;

      procedure DumpXML_Location (Cursor : String_Sets.Cursor);
      procedure DumpXML_Location (Cursor : String_Sets.Cursor) is

      begin
         Put_Line ("       <location>" & String_Sets.Element (Cursor) & "</location>");
      end DumpXML_Location;

      procedure DumpXML_Template_2 (Key     : String;
                                    Element : Template'Class);

      procedure DumpXML_Template_2 (Key     : String;
                                    Element : Template'Class) is
      begin
         Put_Line ("    <template>");
         Put_Element ("name",   Key, "       ");
         Put_Element ("simple", Element.Simple'Img, "       ");
         Put_Element ("alias",  Element.Alias, "       ");
         Put_Element ("prompt", Element.Prompt, "       ");
         Put_Element ("hint",   Element.Hint, "       ");
         Put_Element ("class",  Element.Class, "       ");
         Put_Element ("path",   Element.Path, "       ");
         Put_Element ("tooltip", Element.ToolTip, "       ");
         Put_Element ("load",   Element.Load, "       ");
         Put_Element ("output", Element.Output, "       ");
         Put_Element ("helper", Element.Helper'Img, "       ");
         Element.Locations.Iterate (DumpXML_Location'Access);
         if Element.Variables.Length > 0 then
            Put_Line ("       <variables>");
            Element.Variables.Iterate (DumpXML_Variable'Access);
            Put_Line ("       </variables>");
         end if;
         Put_Line ("    </template>");
         New_Line;
      end DumpXML_Template_2;

      procedure DumpXML_Template (Position : Template_Storages.Cursor);
      procedure DumpXML_Template (Position : Template_Storages.Cursor) is

      begin
         Template_Storages.Query_Element (Position, DumpXML_Template_2'Access);
      end DumpXML_Template;

   begin
      if XML then
         Put_Line ("<?xml version=""1.0"" encoding=""UTF-8"" ?>");
         Put_Line ("<ssprep>");
         Put_Line ("  <templates>");
         This.Sources.Iterate (DumpXML_Template'Access);
         Put_Line ("  </templates>");
         Put_Line ("</ssprep>");
      else
         This.Sources.Iterate (Process'Access);
      end if;
   end Dump;

   function New_Template (Location  : String) return Template is
      Ret : Template;
   begin
      if Location /= "" then
         Ret.Valid := True;
         Ret.Locations.Include (Location);
      else
         Ret.Valid := False;
      end if;
      return Ret;
   end New_Template;


   function Get (This : in Templates; Name : String) return Template'Class is
   begin
      if This.Sources.Contains (Name) then
         return This.Sources.Element (Name);
      elsif Is_Directory (Name)  or Is_Readable_File (Name) then
         return New_Template (Normalize_Pathname (Name));
      else
         return New_Template ("");
      end if;
   end Get;

   function Executable_Prefix_Path return String is
      Exec_Name : constant String := Ada.Command_Line.Command_Name;
      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceeded by the absolute or relative
      --  path, e.g. "c:\usr\bin\gcc.exe". Returns the absolute directory
      --  where "bin" lies (in the example "C:\usr").
      --  If the executable is not in a "bin" directory, return "".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         use GNAT.Regpat;

         Exec      : constant String  := GNAT.Directory_Operations.Format_Pathname (Normalize_Pathname (S), GNAT.Directory_Operations.UNIX);
         Matcher   : constant Pattern_Matcher :=
                       Compile ("(.*)/bin/.*" & Ada.Directories.Base_Name (Exec));
         Matches   :  Match_Array (1 .. Paren_Count (Matcher));
      begin
         Match (Matcher, Exec, Matches);
         if Matches (1) /= No_Match then
            return Exec (Matches (1).First .. Matches (1).Last);
         else
            return "";
         end if;
      end Get_Install_Dir;

      --  Beginning of Executable_Prefix_Path

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      declare
         Ret : GNAT.Strings.String_Access := Locate_Exec_On_Path (Exec_Name);
      begin
         if Ret /= null and then Ret.all /= "" then
            declare
               R : constant String := Get_Install_Dir (Ret.all);
            begin
               Free (Ret);
               return R;
            end;
         end if;
      end;
      for J in reverse Exec_Name'Range loop
         if Exec_Name (J) = Directory_Separator then
            return Get_Install_Dir (Exec_Name);
         end if;
      end loop;
      return "";
   end Executable_Prefix_Path;



   function Get_System_Templates_Dir return String is

   begin
      return Executable_Prefix_Path & Directory_Separator &
      "share" & Directory_Separator &
      "ssprep" & Directory_Separator &
      "templates";
   end Get_System_Templates_Dir;

   function Get_User_Templates_Dir return String is
      HOME_REF : GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Getenv ("HOME");
      HOME     : constant String := HOME_REF.all;
   begin
      Free (HOME_REF);

      if HOME'Length /= 0 and then
        Is_Absolute_Path (HOME) and then
        Is_Directory (Ada.Directories.Compose (HOME, ".ssprep"))
      then
         return Ada.Directories.Compose (HOME, ".ssprep");
      else
         return "";
      end if;
   end Get_User_Templates_Dir;


   procedure Set_Verbose (This       : in out Templates;
                          Verbose    : Boolean) is
   begin
      This.Verbose := Verbose;
   end Set_Verbose;

   procedure Set_Quiet (This       : in out Templates;
                        Quiet      : Boolean) is
   begin
      This.Quiet := Quiet;
   end Set_Quiet;

   function Get_Path (This  : Templates) return String is
      Ret : GNAT.Spitbol.VString;
      procedure Process (C : String_Vectors.Cursor) is
      begin
         if Length (Ret) /= 0 then
            Append (Ret, GNAT.OS_Lib.Path_Separator);
         end if;
         Append (Ret, String_Vectors.Element (C));
      end Process;
   begin
      This.PATH.Iterate (Process'Access);
      return S (Ret);
   end Get_Path;

   ----------------
   --  Get_Path  --
   ----------------
   function Get_Paths (This : Template) return Any_String_Set is
   begin
      if This.Valid then
         return This.Locations'Unrestricted_Access;
      else
         return null;
      end if;
   end Get_Paths;
   ----------------
   --  Is_Valid  --
   ----------------
   function Is_Valid (This : Template) return Boolean is
   begin
      return This.Valid;
   end Is_Valid;

   function getHint (this : Template) return access String is
   begin
      return this.Hint;
   end getHint;

end Ssprep.Templates;

