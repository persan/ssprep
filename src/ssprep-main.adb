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
with Ada.Command_Line;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.Exception_Traces;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

with Ssprep.Templates;
with Ssprep.Translators;
with Ada.Command_Line.Environment;
with GNAT.String_Split; use GNAT.String_Split;
with Ssprep.Version;
with Ssprep.Utilities; use Ssprep.Translators;
with Templates_Parser;
with Ada; use Ada;
with Ada.IO_Exceptions;
with Ssprep.TempDir;
with GNAT.Strings;

procedure Ssprep.Main is

   Command_Name : constant String := Ada.Directories.Base_Name
     (Ada.Command_Line.Command_Name);


   Trans                : Translators.Translator;
   DB                   : Templates.Templates;
   Dump_Db              : Boolean := False;
   Symbols              : GNAT.Strings.String_Access;
   Include_System_Paths : Boolean := True;
   Output_Dir           : GNAT.Strings.String_Access;
   DumpFilePath         : GNAT.Strings.String_Access;
   Execute_Children     : Boolean := True;
   Verbose              : Boolean := False with Unreferenced;
   Environment_Loaded   : Boolean := False;
   Dump_Db_XML          : Boolean := False;

   Translations : Templates_Parser.Translate_Set;

   procedure Print_Help;
   procedure Print_Help is
      use ASCII;
   begin
      Put_Line
        (Command_Name & " ("  & Ssprep.Version.Rev & ")" & LF & LF &
           "Syntax :" & LF &
           " " & Command_Name & " [Options] Templates [Options]" & LF & LF &

           "Templates:" & LF &
           "  Is ether an existing path or a name from the templates database." & LF &

           LF & "Options:" & LF &
           LF & "  Symbol defenition:" & LF &
           "    -x=file|--xml=file       Read symbols from xml-file file in Templates_Parser format." & LF &
           "    -DSymbol=Value           Define a symbol value and coresponding support symbols" & LF &
           "                              {Symbol}_file the value with '.' replaced with '-' and in lower case" & LF &
           "                              {Symbol}_dir the value with '.' replaced with '-'." & LF &
           "                              {Symbol}_file2 the value in lowercase with '.' replaced with '_'." & LF &
           "                              {Symbol}_file3 the value in lowercase with '.' and '-' replaced with '_' ." & LF &
           "                              {Symbol}_name t he value  with '.' and '-' replaced with '_' ." & LF &
           "                              lc_{Symbol} the value in lower case." & LF &
           "    -dSymbol=Value           Define a symbol value without filename." & LF &
           "                              If the value is enclosed in parantheses then" & LF &
           "                              the value is treated as a vector (not empty values will be ignored)." & LF &
           "    -m{prefix=}file          Read symbols from a manifest or property file " & LF &
           "                               and if prefix is present the values will get that prefix." & LF &
           "                               If the full value is a {delimiter} separated string it " & LF &
           "                               will be treated as a vector entry" & LF &
           "                               Values are treated as multiline values if the last " & LF &
           "                               nonblank character on the line is '\'." & LF &
           "    -M{prefix=}file          Read symbols from a manifest or property file" & LF &
           "                               and also get decorated symbols in the same way as '-D'." & LF &
           "    -E|--environment         Include Environment variables as symbols," & LF &
           "                               environment symbols are decoraded with 'env.' as prefix." & LF &
           "" & LF &
         --           "    --exec-{mode}={Command}  Execute to and parse output output values in acordance with mode." & LF &
         --           "                               Where mode may be:" & LF &
         --           "                                 D         Parse output as 'D' Symbol Value pairs." & LF &
         --           "                                 d         Parse output as 'd' Symbol Value pairs." & LF &
         --           "                                 m{prefix} Parse output as Manifest or Property file." & LF &
         --           "                                 M{prefix} Parse output as Manifest or Property file with decoration." & LF &
         --           "                                 x|xml     Parse output as Templates_Parser symbols." & LF & LF &
           "    --delimiter={char}       Define delimiter for vector values default '" & Default_Delimiter & "'." & LF &
           "      NOTE !" & LF &
           "        Decorated single symbolnames are transformed to lowercase due to filenaming ambiguity on" & LF &
           "        the windows operating system." & LF &

           LF & "  Database defenition:" & LF &
           "    -I=dir               Add templates from dbfiles 'dir/*.xml' to database." & LF &
           "    -If=file             Add templates from dbfile file to database (the file must exist)." & LF &
           "    -I-                  Dont use system templates." & LF &
           "            Note !" & LF &
           "                         The $" & SSPREP_PATH & " active for the root ssprep given all extra includes " & LF &
           "                         will be the initial $" & SSPREP_PATH & " for children." & LF &

           LF & "  Output Control and source limitation:" & LF &
           "    --noexec             Dont execute *." & Ssprep_Suffix & " files after the translation." & LF &
           "    -v|--verbose         Be verbose (inherited in SSPREP.verbose)." & LF &
           "    -e|--effort          Effort only." & LF &
           "    -f|--force           Force update (inherited in SSPREP.force)." & LF &
           "    --java               Expand ." & Java_Suffix & " files to the proper directory (inherited in SSPREP.java)." & LF &
           "    -o=dir               Set target dir (default=current dir)." & LF &
           "    -O=dir               Set target dir and variable project."  & LF &
           "    --echo               Echo the translated object to standard output" & LF &
           "                           only meaningfull when as single file is used as template" & LF &
           "    --hint               If hint is avalible in the template thet then echo ""hint:${hint}"" to standard output" & LF &
         --  "    --echo-file          Echo generated filenames to standard output" & lf &
           "    --symbols{=file}      Dump the symbol database to file in Templates_Parser format." & LF &
           "                           If no file is given then standard output is used." & LF &
           "    --dontexpand=regexp  Do not expand file namaes matching regexp default: '" & Default_Dont_Expand  & "'." & LF &
           "    --ignorefiles=regexp Do not process simple simple filenames matching regexp: '" & Default_Ignore  & "'." & LF &
           LF & "  Misc:" & LF &

           "    --break-on-db-error Abort if ther is an errors in the database." & LF &
           "    --dump              Dump current templates database." & LF &
           "    --dump-xml{=file}   Dump current templates database in xml format." & LF &
           "    --exceptions        Log all exceptions to standard output (inherited in SSPREP.exception)." & LF &
           "    --version           Prints the current version." & LF &
           "    -h|-?|--help        Print this  text." & LF &
           LF & "Special Symbols:" & LF &
           "    project               will define a the symbol 'parent' that contains the projects name" & LF &
           "                          up to and excluding the last '.' or '-'" & LF &
           LF &
           "  Generated for each template used in the expansion." & LF &
           "    TEMPLATE              Contains the logical name for the current template" & LF &
           "    TEMPLATEPATH          Contains the full path current template" & LF &
           "    TEMPLATEPROJECTPATH   Contains the full path of the closest enclosing directory containing " & LF &
           "                            the file '.project' originating search in TEMPLATEPATH." & LF &
           "    TEMPLATEPROJECT       Contains the basename of 'TEMPLATEPROJECTPATH'." & LF &
           "    output_dir            Contains the output directory" & LF
        );

   end Print_Help;

   procedure Read_Environment is
      procedure Set (S : String) is
         Var : Slice_Set;
      begin
         Create (Var, S, "=");
         declare
            Name  : constant String := Slice (Var, 1);
            Value : constant String := Slice (Var, 2);
         begin
            if Name = "SSPREP.hint" then
               Trans.Set_Hint (True);
            elsif Name = "SSPREP.verbose" then
               Trans.Set_Verbose (True);
               DB.Set_Verbose (True);
            elsif  Name = "SSPREP.environmentloaded" then
               Environment_Loaded := True;
            elsif  Name = "SSPREP.force" then
               Trans.Set_Replace (True);
            elsif  Name = "SSPREP.java" then
               Trans.Set_Java_Expand (True);
            elsif  Name = "SSPREP.exceptions" then
               GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
               GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
            elsif Name = "root_tmplt" then
               Set_Symbol (Translations, Name, Value);
            elsif Name = "root_tmplt_parent" then
               Set_Symbol (Translations, Name, Value);
            elsif Name = "SSPREP.symbols" then
               Read_Symbols (Translations, Value);
            end if;
         end;
      end Set;
   begin
      for I in 1 .. Ada.Command_Line.Environment.Environment_Count loop
         Set (Ada.Command_Line.Environment.Environment_Value (I));
      end loop;
   end Read_Environment;
begin
   Read_Environment;
   loop
      case Getopt ("x= -xml= v O= o= -hint -echo -symbols? -verbose " &
                     "-delimiter= -dontexpand= -ignorefiles= e d: D: E M: m: -environment -effort -noexec -f I= I- If -java -force f " &
                     "-dump? -dump-xml? --dump-path -db= " &
                     "-exceptions h ? -help -version " &
                     "-exec-x= -exec-xml= -exec-d= -exec-D= -exec-m: -exec-m:") is
         when ASCII.NUL => exit;

            when '-' =>
            if Full_Switch = "-environment" then
               if not Environment_Loaded then
                  Add_Environment (Translations);
                  GNAT.OS_Lib.Setenv ("SSPREP.environmentloaded", "1");
               end if;
            elsif Full_Switch = "-delimiter" then
               declare
                  P : constant String := Parameter;
               begin
                  if P'Length = 0 then
                     Trans.Set_Delimiter (' ');
                  else
                     Trans.Set_Delimiter (P (P'First));
                  end if;
               end;

            elsif Full_Switch = "-xml" then
               Read_Symbols (Translations, Parameter);

            elsif Full_Switch = "-dontexpand" then
               Trans.Set_Dont_Expand (Parameter);


            elsif Full_Switch = "-ignorefiles" then
               Trans.Set_Ignore (Parameter);
            elsif Full_Switch = "-verbose" then
               Trans.Set_Verbose (True);
               DB.Set_Verbose (True);
               GNAT.OS_Lib.Setenv ("SSPREP.verbose", "1");
            elsif Full_Switch = "-effort" then
               Trans.Set_Effort_Only (True);

            elsif Full_Switch = "-force" then
               Trans.Set_Replace (True);
               GNAT.OS_Lib.Setenv ("SSPREP.verbose", "1");

            elsif Full_Switch = "-dump" then
               Dump_Db := True;
               declare
                  S : constant String := Parameter;
               begin
                  if S'Length > 0 and then S (S'First) = '=' then
                     DumpFilePath := new String'(S (S'First + 1 .. S'Last));
                  end if;
               end;

            elsif Full_Switch = "-dump-xml" then
               Dump_Db := True;
               Dump_Db_XML := True;
               declare
                  S : constant String := Parameter;
               begin
                  if S'Length > 0 and then S (S'First) = '=' then
                     DumpFilePath := new String'(S (S'First + 1 .. S'Last));
                  end if;
               end;

            elsif Full_Switch = "-hint" then
               Trans.Set_Hint (True);
               GNAT.OS_Lib.Setenv ("SSPREP.hint", "1");
            elsif Full_Switch = "-java" then
               Trans.Set_Java_Expand (True);
               GNAT.OS_Lib.Setenv ("SSPREP.java", "1");

            elsif Full_Switch = "-echo" then
               Trans.Set_Echo (True);
               DB.Set_Quiet (True);

            elsif Full_Switch = "-symbols" then
               declare
                  S : constant String := Parameter;
               begin
                  if S'Length > 0 and then S (S'First) = '=' then
                     Symbols := new String'(S (S'First + 1 .. S'Last));
                  else
                     Symbols := new String'(S);
                  end if;
               end;

            elsif Full_Switch = "-version" then
               Ada.Text_IO.Put_Line (Ssprep.Version.Rev);
               return;

            elsif Full_Switch = "-help" then
               Print_Help;
               return;

            elsif Full_Switch = "-noexec" then
               Execute_Children := False;

            elsif Full_Switch = "-break-on-db-error" then
               DB.Set_Break_On_Error (True);

            elsif Full_Switch = "-exceptions" then
               GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
               GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
               GNAT.OS_Lib.Setenv ("SSPREP.exceptions", "1");
            end if;


         when 'x' =>
            Read_Symbols (Translations, Parameter);
         when 'v' =>
            Trans.Set_Verbose (True);
            DB.Set_Verbose (True);
            Verbose := True;
         when 'e' =>
            Trans.Set_Effort_Only (True);
         when 'E' =>
            if not Environment_Loaded then
               Add_Environment (Translations);
               GNAT.OS_Lib.Setenv ("SSPREP.environmentloaded", "1");
            end if;
         when 'f' =>
            Trans.Set_Replace (True);
         when 'I' =>
            if Full_Switch = "I" then
               DB.Add_Directory (Parameter);
            elsif Full_Switch = "If" then
               DB.Add_Config_File (Parameter);
            elsif Full_Switch = "I-" then
               Include_System_Paths := False;
            end if;
         when 'D' =>
            Set_Symbol (Translations, Parameter, Trans.Get_Delimiter, True);
         when 'd' =>
            Set_Symbol (Translations, Parameter, Trans.Get_Delimiter, False);
         when 'M' =>
            Read_Symbols_From_Manifest (Translations, Parameter, Trans.Get_Delimiter, True);
         when 'm' =>
            Read_Symbols_From_Manifest (Translations, Parameter, Trans.Get_Delimiter, False);
         when 'o' =>
            Output_Dir := new String'(Parameter);
         when 'O' =>
            declare
               P : constant String := Directories.Full_Name (Parameter);
            begin
               Output_Dir := new String'(Directories.Containing_Directory (P));
               Set_Symbol (Translations, "project", Directories.Simple_Name (P), Trans.Get_Delimiter, True);
            end;
         when 'h' | '?' =>
            Print_Help;
            return;

         when others =>
            raise Program_Error with "Erronous configuration of options got '" & Full_Switch & "'";
      end case;
   end loop;

   if Output_Dir = null then
      Output_Dir := new String'(Ada.Directories.Current_Directory);
   end if;

   if Include_System_Paths then
      DB.Add_Directory (Templates.Get_User_Templates_Dir);
      DB.Add_Directory (Templates.Get_System_Templates_Dir);
      if Ada.Environment_Variables.Exists (SSPREP_PATH) then
         DB.Add_Path (Ada.Environment_Variables.Value (SSPREP_PATH));
      end if;
   end if;

   if Dump_Db then
      declare
         DumpFile : aliased Ada.Text_IO.File_Type;
      begin
         if DumpFilePath /= null then
            Ada.Text_IO.Create (DumpFile, Ada.Text_IO.Out_File, DumpFilePath.all);
            Ada.Text_IO.Set_Output (DumpFile);
         end if;

         if Dump_Db_XML then
            DB.Dump (Ada.Text_IO.Put_Line 'Access, True);
         else
            Put_Line ("Templates:");
            DB.Dump (Ada.Text_IO.Put_Line'Access);
         end if;

         if DumpFilePath /= null then
            Close (DumpFile);
         end if;
      end;
   end if;
   if DB.Get_Errors_Found then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
   if Symbols /= null then
      Write_Symbols (Translations, Symbols.all);
   end if;

   loop
      declare
         S : constant String := Get_Argument (False);
      begin
         exit when S'Length = 0;
         declare
            Src : aliased  Templates.Template'Class := DB.Get (S);
         begin
            if not Src.Is_Valid then
               Put_Line ("Unable to find template: '" & S & "'.");
               Ada.Text_IO.Put_Line ("Templates:");
               DB.Dump (Ada.Text_IO.Put_Line'Access);
               Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
            end if;

            Set_Symbol (Translations, "TEMPLATE", S);
            Append_Symbols (Translations, DB.Get_Symbols.all);
            Set_Symbol (Translations, "output_dir", GNAT.OS_Lib.Normalize_Pathname (Output_Dir.all));
            Trans.Translate (Src'Unchecked_Access,
                             Translations,
                             GNAT.OS_Lib.Normalize_Pathname (Output_Dir.all));
         end;
      end;
   end loop;

   if Execute_Children then
      Ada.Environment_Variables.Set (SSPREP_PATH, DB.Get_Path);
      declare
         TempFile : constant String := TempDir & "/" & "ssprep-sybols-" & Utilities.Get_Process_Id & ".xml";
         Success  : Boolean;
      begin
         if TempDir = "" or not Ada.Directories.Exists (TempDir) then
            raise Ada.IO_Exceptions.Name_Error with "Invalid value of $TEMP: """ & TempDir & """.";
         end if;
         Write_Symbols (Translations, TempFile);
         GNAT.OS_Lib.Setenv ("SSPREP.symbols", TempFile);
         Trans.Execute_Children;
         GNAT.OS_Lib.Delete_File (TempFile, Success);
      end;
   end if;
exception
   when Invalid_Switch    =>
      Put_Line ("Invalid Switch " & Full_Switch);
      Print_Help;
      Put ("Arguments:'");
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         Put (Ada.Command_Line.Argument (I) & " ");
      end loop;
      Put_Line ("'.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when Invalid_Parameter =>
      Put_Line ("No parameter for " & Full_Switch);
      Print_Help;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : others =>
      Put_Line ("");
      Put_Line (Ada.Exceptions.Exception_Information (E));
      Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Ssprep.Main;
