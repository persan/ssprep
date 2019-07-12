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
with Ada.Environment_Variables;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Case_Util;
with GNAT.String_Split;
with GNAT.Spitbol; use GNAT.Spitbol;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;


with Templates_Parser.XML;
with Ssprep.Java_Utils;
with Ssprep.ConfigParsers;
with Ssprep.String_Sets;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line;

package body Ssprep.Translators is
   use Templates_Parser;
   use Ada;
   use type Ssprep.Templates.Any_Template;

   Ada2File   : constant Ada.Strings.Maps.Character_Mapping :=
                  Ada.Strings.Maps.To_Mapping (".ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ",
                                               "-abcdefghijklmnopqrstuvwxyzåäö");
   Lc         : constant Ada.Strings.Maps.Character_Mapping :=
                  Ada.Strings.Maps.To_Mapping ("ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ",
                                               "abcdefghijklmnopqrstuvwxyzåäö");
   Ada2Dir    : constant Ada.Strings.Maps.Character_Mapping :=
                  Ada.Strings.Maps.To_Mapping (".",
                                               "-");
   Ada2File2  : constant Ada.Strings.Maps.Character_Mapping :=
                  Ada.Strings.Maps.To_Mapping (".ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ",
                                               "_abcdefghijklmnopqrstuvwxyzåäö");

   Ada2File3   : constant Ada.Strings.Maps.Character_Mapping :=
                   Ada.Strings.Maps.To_Mapping (".-ABCDEFGHIJKLMNOPQRSTUVWXYZÅÄÖ",
                                                "__abcdefghijklmnopqrstuvwxyzåäö");
   Name2File3  : constant Ada.Strings.Maps.Character_Mapping :=
                   Ada.Strings.Maps.To_Mapping (".-",
                                                "__");

   function Is_Windows return Boolean is
   begin
      if Ada.Environment_Variables.Exists ("OS") then
         return Ada.Environment_Variables.Value ("OS") = "Windows_NT";
      else
         return False;
      end if;
   end Is_Windows;

   procedure Create_Path (Name : String) is
      Parent : constant String := Dir_Name (Name);
   begin
      if not Is_Directory (Parent) then
         Create_Path (Parent);
      else
         Make_Dir (Name);
      end if;
   end Create_Path;

   procedure Execute_Children (This   : Translator) is
      use String_Vectors;


      procedure Try_Exec_Command (Cmd : String; Src : String; Line : Ada.Text_IO.Count) is
         Tgtcmd  : VString;
         procedure Exec_Command (Args : VString) is
            Argv    : Argument_List_Access := new Argument_List (1 .. 3);
            Success : Boolean;
         begin
            Argv (1) := new String'("cmd");
            Argv (2) := new String'("/C");
            Argv (3) := new String'(Cmd);

            if This.Verbose then
               Put_Line ("   " & Get_Current_Dir & ">" & Cmd);
            end if;
            if not This.Effort_Only then
               Spawn (Argv.all (1).all,
                      Argv.all (2 .. 3), Success);
               Free (Argv);
               if not Success then
                  Put_Line (Standard_Error, Src & ":" & Line'Img & ": Faild to execute '" & S (Args) & "'");
                  Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
               end if;
            end if;
         end Exec_Command;
      begin
         if Cmd'Length = 0 then
            return;
         elsif Cmd'Length > 0 and then (Cmd (Cmd'First) = '#' or Cmd (Cmd'First) = ';') then
            return;
         elsif Cmd'Length > 1 and then (Cmd (Cmd'First .. Cmd'First + 1) = "--" or
                                          Cmd (Cmd'First .. Cmd'First + 1) = "//")
         then
            return;
         end if;
         Append (Tgtcmd, Cmd);
         GNAT.OS_Lib.Setenv ("root_tmplt", S (This.Root) & ".");
         GNAT.OS_Lib.Setenv ("droot_tmplt_parent", Dir_Name (S (This.Root)) & ".");
         Exec_Command (Tgtcmd);
      end Try_Exec_Command;

      procedure Process (Position : Cursor) is
         F      : Ada.Text_IO.File_Type;
         Src    : constant String := Element (Position);
      begin
         if This.Verbose then
            Put_Line ("X  " & Src);
         end if;
         Change_Dir (Dir_Name (Src));
         if Is_Windows then
            Open (F, In_File, Src);
            while not End_Of_File (F) loop
               Try_Exec_Command (Ada.Strings.Fixed.Trim (Get_Line (F), Ada.Strings.Both), Src, Line (F));
            end loop;
            if This.Verbose then
               Close (F);
            else
               Delete (F);
            end if;
         else -- Target has real execution capabilities
            declare
               Success : Boolean;
               Argv    : Argument_List (1 .. 0);
            begin
               GNAT.OS_Lib.Setenv ("root_tmplt", S (This.Root) & ".");
               GNAT.OS_Lib.Setenv ("droot_tmplt_parent", Dir_Name (S (This.Root)) & ".");
               GNAT.OS_Lib.Set_Executable (Src);
               Spawn (Src, Argv, Success);
               if not Success then
                  Put_Line (Standard_Error, Src);
                  Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
               end if;
               if not This.Verbose then
                  GNAT.OS_Lib.Delete_File (Src, Success);
                  if not Success then
                     Put_Line (Standard_Error, "Failed to delete:" & Src);
                     Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
                  end if;
               end if;
            end;
         end if;
      end Process;
   begin
      declare

      begin
         This.Execution_List.Iterate (Process'Access);
      end;
   end Execute_Children;

   procedure Set_Log_Exceptions (This              : in out Translator;
                                 Log_Exceptions    : Boolean) is
   begin
      This.Log_Exceptions := Log_Exceptions;
   end Set_Log_Exceptions;

   ---------------
   -- Translate --
   ---------------
   procedure Translate (This         : in out Translator;
                        Source       : Templates.Any_Template;
                        Translations : Templates_Parser.Translate_Set;
                        Target_Dir   : String) is
      procedure Process (Cursor : Ssprep.String_Sets.Cursor);
      procedure Process (Cursor : String_Sets.Cursor) is
      begin
         This.Translate (String_Sets.Element (Cursor), Translations, Target_Dir);
      end Process;
   begin
      This.Template := Source;
      Source.Get_Paths.Iterate (Process'Access);
   end Translate;


   procedure Translate
     (This         : in out Translator;
      Source       : String;
      Translations : Templates_Parser.Translate_Set;
      Target_Dir   : String)
   is

   begin
      if Source = "" then
         This.Root := V (Normalize_Pathname ("./"));
      elsif Is_Directory (Source) and (Source (Source'Last) /= Directory_Separator) then
         This.Root := V (Source & Directory_Separator);
      else
         This.Root := V (Source);
      end if;
      if Target_Dir'Length = 2 and then Target_Dir (Target_Dir'First + 1) = ':' then
         This.Translate_Inner (Source, Translations, Target_Dir);
      else
         if not This.Echo and not Is_Directory (Target_Dir) then
            Create_Path (Target_Dir);
         end if;
         This.Translate_Inner (Source, Translations, Normalize_Pathname (Target_Dir));
      end if;
      if This.Hint and then This.Template /= null and then This.Template.GetHint /= null then
         Ada.Text_IO.Put_Line ("hint:" & Translate_String (Translations, This.Template.GetHint.all));
      end if;
   end Translate;


   ---------------------
   -- Translate_Inner --
   ---------------------
   procedure Translate_Inner
     (This          : in out Translator;
      Source        : String;
      Translations  : Templates_Parser.Translate_Set;
      Target_Root   : String)
   is
      Simple_Src : constant String := File_Name (Source);
      Flag       : String := "?f ";
   begin
      if GNAT.Regexp.Match (Simple_Src, This.Ignore) then
         return;
      end if;

      if This.Verbose then
         if Is_Directory (Source) then
            Flag (Flag'First + 1) := 'd';
         end if;
         if This.Echo then
            Ada.Text_IO.Put (This.Log_File.all, "--  ");
         end if;
         Ada.Text_IO.Put_Line (This.Log_File.all, Flag & Source);
      end if;

      if Is_Directory (Source) then
         This.Translate_Dir
           (Source,
            Translations,
            Target_Root & GNAT.OS_Lib.Directory_Separator & Translate_String (Translations, Simple_Src));
      elsif Is_Readable_File (Source) then
         This.Translate_File
           (Source, Translations,
            Target_Root  & GNAT.OS_Lib.Directory_Separator & Translate_String (Translations, Simple_Src));
      else
         raise Ada.IO_Exceptions.Name_Error with "Unable to read : " & Source;
      end if;

   end Translate_Inner;

   --------------------
   -- Translate_File --
   --------------------
   procedure Translate_File (This         : in out Translator;
                             Source       : String;
                             Translations : Templates_Parser.Translate_Set;
                             Target       : String) is
      Exists         : constant Boolean := Is_Regular_File (Target);
      Success        : Boolean;
      L_Translations : Templates_Parser.Translate_Set := Translations;
   begin


      if This.Verbose or This.Effort_Only then
         if This.Echo then
            Ada.Text_IO.Put (This.Log_File.all, "--  ");
         end if;
         if Exists and not This.Replace then
            Ada.Text_IO.Put (This.Log_File.all, "-  ");
         else
            Ada.Text_IO.Put (This.Log_File.all, "+  ");
         end if;
         Ada.Text_IO.Put_Line (This.Log_File.all, Target);
      end if;
      if This.Effort_Only then
         return;
      end if;
      if not This.Replace and Exists and not This.Echo then
         return;
      end if;

      Change_Dir (Dir_Name (Source));
      Insert (L_Translations, Assoc ("this_source_file", Source));
      Insert (L_Translations, Assoc ("this_source_dir", Dir_Name (Source) & "."));
      Insert (L_Translations, Assoc ("this_source_parent", Dir_Name (Dir_Name (Source)) & "."));

      if This.Echo then
         Ada.Text_IO.Put (This.Log_File.all,
                          Templates_Parser.Parse (Source, L_Translations, Keep_Unknown_Tags => True));
      elsif GNAT.Regexp.Match (Source, This.Dont_Expand) then
         Copy_File (Source, Target, Success, Preserve => None);

      else
         declare
            SourceImage : constant String := Templates_Parser.Parse (Source, L_Translations, Keep_Unknown_Tags => True);
         begin
            if This.Expand_Java and File_Extension (Target) = Java_Suffix then
               Java_Utils.Write (Dir_Name (Target), SourceImage);
            else
               declare
                  Fc : GNAT.OS_Lib.File_Descriptor;
                  Nb : Integer;
               begin
                  Fc := GNAT.OS_Lib.Create_File (Target, GNAT.OS_Lib.Binary);
                  Nb := GNAT.OS_Lib.Write (Fc, SourceImage'Address, SourceImage'Length);
                  GNAT.OS_Lib.Close (Fc);
                  if Nb /= SourceImage'Length then
                     Put_Line (Standard_Error, Target & ": is not writable");
                     raise Ada.IO_Exceptions.Data_Error with "Unable to write " & Target;
                  end if;
               end;
            end if;
         end;
      end if;

      if File_Extension (Target) = Ssprep_Suffix then
         This.Execution_List.Append (Target);
      end if;

   end Translate_File;

   -------------------
   -- Translate_Dir --
   -------------------

   procedure Translate_Dir (This         : in out Translator;
                            Source       : String;
                            Translations : Templates_Parser.Translate_Set;
                            Target       : String) is
      procedure Process (Name  :   String);
      procedure Process (Name  :   String) is
      begin
         if Name /= ""  and Name /= "." and Name /= ".." then
            This.Translate_Inner (Normalize_Pathname (Source & Directory_Separator & Name),
                                  Translations,
                                  Normalize_Pathname (Target));
         end if;
      end Process;


   begin

      if not (Is_Regular_File (Target) or Is_Directory (Target)) then
         if not This.Effort_Only then
            if This.Verbose then
               if This.Echo then
                  Ada.Text_IO.Put_Line (This.Log_File.all, "-- ");
               end if;
               Ada.Text_IO.Put_Line (This.Log_File.all, "+  " & Target & "/");
            end if;
            if not This.Echo then
               Create_Path (Target);
            end if;
         end if;
      end if;

      declare
         Dir  : Dir_Type;
         Name : String (1 .. 1024);
         Last : Natural;
      begin
         Open (Dir, Source);
         loop
            Read (Dir, Name, Last);
            exit when Last = 0;
            Process (Name (Name'First .. Last));
         end loop;
         Close (Dir);
      end;
   end Translate_Dir;

   ----------------------
   -- Translate_String --
   ----------------------
   function Translate_String (Translations : Templates_Parser.Translate_Set;
                              Source       : String) return String is
   begin
      return Templates_Parser.Translate (Source, Translations);
   end Translate_String;


   function  Get_Parent (P : String) return String is
   begin
      for I in reverse  P'Range loop
         if P (I) = '.' or P (I) = '-' then
            return P (P'First .. I - 1);
         end if;
      end loop;
      return "";
   end Get_Parent;

   ----------------
   -- Set_Symbol --
   ----------------

   procedure Set_Symbol
     (This        : in out Templates_Parser.Translate_Set;
      Name        : String;
      Value       : String;
      Delimiter   : Character := ',';
      Define_File : Boolean := False)
   is
      LC_Name : String := Name;


   begin
      if Value = "" then
         return;
      end if;

      GNAT.Case_Util.To_Lower (LC_Name);
      if (Value (Value'First) = '(' and Value (Value'Last) = ')') or
        (Value (Value'First) = '{' and Value (Value'Last) = '}') or
        (Value (Value'First) = '[' and Value (Value'Last) = ']')
      then
         declare
            use GNAT.String_Split;
            Slices       : GNAT.String_Split.Slice_Set;
            Values       : Templates_Parser.Tag;
            Found_Value  : Boolean := False;
         begin
            GNAT.String_Split.Create (Slices, Value (Value'First + 1 .. Value'Last - 1), "" & Delimiter);
            for I in 1 .. Slice_Count (Slices) loop
               declare
                  V : constant String := Trim (Slice (Slices, I), Ada.Strings.Both);
               begin
                  if V'Length > 0 then
                     Append (Values, V);
                     Found_Value := True;
                  end if;
               end;
            end loop;
            if Found_Value then
               Templates_Parser.Insert
                 (Set  => This,
                  Item => Templates_Parser.Assoc (Name, Values));
               if Define_File then
                  declare
                     Values_Ada2File       : Templates_Parser.Tag;
                     Values_Ada2Dir        : Templates_Parser.Tag;
                     Values_Lc             : Templates_Parser.Tag;
                     Values_File2          : Templates_Parser.Tag;
                  begin
                     for I in 1 .. Slice_Count (Slices) loop
                        declare
                           V_Ada2File : constant String := Translate (Trim (Slice (Slices, I), Ada.Strings.Both), Ada2File);
                           V_Ada2Dir  : constant String := Translate (Trim (Slice (Slices, I), Ada.Strings.Both), Ada2Dir);
                           V_Lc       : constant String := Translate (Trim (Slice (Slices, I), Ada.Strings.Both), Lc);
                           V_File2    : constant String := Translate (Trim (Slice (Slices, I), Ada.Strings.Both), Ada2File2);
                        begin
                           if V_Lc'Length > 0 then
                              Append (Values_Ada2File, V_Ada2File);
                              Append (Values_Ada2Dir, V_Ada2Dir);
                              Append (Values_Lc, V_Lc);
                              Append (Values_File2, V_File2);
                              Found_Value := True;
                           end if;
                        end;
                     end loop;
                     Templates_Parser.Insert
                       (Set  => This,
                        Item => Templates_Parser.Assoc (Name & "_dir",  Values_Ada2Dir));
                     Templates_Parser.Insert
                       (Set  => This,
                        Item => Templates_Parser.Assoc ("lc_" & Name,  Values_Lc));
                     Templates_Parser.Insert
                       (Set  => This,
                        Item => Templates_Parser.Assoc (Name & "_file", Values_Ada2File));
                     Templates_Parser.Insert
                       (Set  => This,
                        Item => Templates_Parser.Assoc (Name & "_file2", Values_File2));
                  end;


               end if;
            end if;
         end;
      else
         Templates_Parser.Insert
           (Set  => This,
            Item => Templates_Parser.Assoc (Name, Value));

         if Name = "project" then
            Set_Symbol (This, "parent", Get_Parent (Value), Delimiter, Define_File);
         end if;

         if Define_File then
            Templates_Parser.Insert
              (Set  => This,
               Item => Templates_Parser.Assoc (LC_Name & "_file", Translate (Value, Ada2File)));
            Templates_Parser.Insert
              (Set  => This,
               Item => Templates_Parser.Assoc (LC_Name & "_dir",  Translate (Value, Ada2Dir)));
            Templates_Parser.Insert
              (Set  => This,
               Item => Templates_Parser.Assoc (LC_Name & "_file2",  Translate (Value, Ada2File2)));
            Templates_Parser.Insert
              (Set  => This,
               Item => Templates_Parser.Assoc (LC_Name & "_file3",  Translate (Value, Ada2File3)));
            Templates_Parser.Insert
              (Set  => This,
               Item => Templates_Parser.Assoc (LC_Name & "_name",  Translate (Value, Name2File3)));
            Templates_Parser.Insert
              (Set  => This,
               Item => Templates_Parser.Assoc ("lc_" & LC_Name, Translate (Value, Lc)));
         end if;
      end if;
   end Set_Symbol;


   ------------------
   -- Set_Symbol   --
   ------------------
   procedure Set_Symbol (This           : in out Templates_Parser.Translate_Set;
                         Value_And_Name : String;
                         Delimiter      : Character := ',';
                         Define_File    : Boolean := False) is

      I : constant Natural := Ada.Strings.Fixed.Index (Value_And_Name, "=");
   begin
      if I /= 0 then
         Set_Symbol (This,
                     Value_And_Name (Value_And_Name'First ..  I - 1),
                     Value_And_Name (I + 1 .. Value_And_Name'Last),
                     Delimiter,
                     Define_File);
      end if;
   end Set_Symbol;

   ------------------
   -- Read_Symbols --
   ------------------

   procedure Read_Symbols
     (This       : in out Templates_Parser.Translate_Set;
      From_File  : String)
   is
   begin
      Templates_Parser.Insert (This, Templates_Parser.XML.Load (From_File));
   end Read_Symbols;

   procedure Read_Symbols_From_Manifest (This           : in out Templates_Parser.Translate_Set;
                                         Source         : String;
                                         Delimiter      : Character := ',';
                                         Define_File    : Boolean) is
      P       : ConfigParsers.RawConfigParser;
      I       : constant Natural := Ada.Strings.Fixed.Index (Source, "=");
      procedure IRead_Symbols (Prefix, File : String) is
         procedure Handle_Value
           (P : in ConfigParsers.RawConfigParser; Section, Option, Value : String) is
            pragma Unreferenced (P);
            Name    : VString;
            Data    : VString;
         begin
            if Prefix /= "" then
               Append (Name, Prefix);
               Append (Name, ".");
            end if;
            if Section /= "" then
               Append (Name, Section);
               Append (Name, ".");
            end if;
            Append (Name, Option);
            if Ada.Strings.Fixed.Index (Value, "" & Delimiter) /= 0 then
               Append (Data, "(");
               Append (Data, Value);
               Append (Data, ")");
            else
               Append (Data, Value);
            end if;
            Set_Symbol (This, S (Name), S (Data), Delimiter, Define_File);
         end Handle_Value;
      begin
         if Is_Readable_File (File) then
            P.Read (File);
            P.Iterate (Handle_Value => Handle_Value'Access);
         else
            Put_Line (Standard_Error, Normalize_Pathname (File) & ": Does not exist.");
            raise IO_Exceptions.Name_Error with File  & "not found.";
         end if;
      end IRead_Symbols;
   begin
      if I > 0 then
         IRead_Symbols (Source (Source'First .. I - 1), Source (I + 1 .. Source'Last));
      else
         IRead_Symbols ("", Source);
      end if;
   end Read_Symbols_From_Manifest;


   --------------------
   -- Append_Symbols --
   --------------------

   procedure Append_Symbols (This    : in out Templates_Parser.Translate_Set;
                             Symbols : Templates_Parser.Translate_Set) is
   begin
      Insert (This, Symbols);
   end Append_Symbols;


   -------------------
   -- Write_Symbols --
   -------------------

   procedure Write_Symbols
     (This       : Templates_Parser.Translate_Set;
      To_File    : String)
   is
   begin
      if To_File = "" then
         Text_IO.Put (S (Templates_Parser.XML.Image (This)));
      else
         Templates_Parser.XML.Save (To_File, This);
      end if;
   end Write_Symbols;

   -----------------
   -- Set_Replace --
   -----------------

   procedure Set_Replace
     (This       : in out Translator;
      Replace    : Boolean)
   is
   begin
      This.Replace := Replace;
   end Set_Replace;

   -----------------
   -- Set_Replace --
   -----------------

   procedure Set_Verbose
     (This       : in out Translator;
      Verbose    : Boolean)
   is
   begin
      This.Verbose := Verbose;
   end Set_Verbose;

   ---------------------
   -- Set_Effort_Only --
   ---------------------

   procedure Set_Effort_Only
     (This           : in out Translator;
      Effort_Only    : Boolean)
   is
   begin
      This.Effort_Only := Effort_Only;
   end Set_Effort_Only;

   --------------
   -- Set_Hint --
   --------------
   procedure Set_Hint (This       : in out Translator;
                       Hint       : Boolean)
   is
   begin
      This.Hint := Hint;
   end Set_Hint;
   --------------
   -- Set_Echo --
   --------------

   procedure Set_Echo (This       : in out Translator;
                       Echo       : Boolean) is
   begin
      This.Echo := Echo;
   end Set_Echo;
   procedure Set_Java_Expand (This       : in out Translator;
                              Expand     : Boolean) is
   begin
      This.Expand_Java := Expand;
   end Set_Java_Expand;

   procedure Add_Environment (This : in out Templates_Parser.Translate_Set) is

      procedure Process (Name, Value : String);
      procedure Process (Name, Value : String) is
      begin
         Set_Symbol (This, "env." & Name, Value);
      end Process;
   begin
      Ada.Environment_Variables.Iterate (Process'Access);
   end Add_Environment;

   procedure Set_Dont_Expand (This         : in out Translator;
                              Regexp       : String) is
   begin
      This.Dont_Expand := GNAT.Regexp.Compile (Regexp);
   end Set_Dont_Expand;


   procedure Set_Ignore (This         : in out Translator;
                         Regexp       : String) is
   begin
      This.Ignore := GNAT.Regexp.Compile (Regexp);
   end Set_Ignore;

   procedure Set_Delimiter (This         : in out Translator;
                            Delimiter    : Character) is
   begin
      This.Delimiter := Delimiter;
   end Set_Delimiter;

   function Get_Delimiter (This : Translator) return Character is
   begin
      return This.Delimiter;
   end Get_Delimiter;

end Ssprep.Translators;
