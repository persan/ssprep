

with Ada.Command_Line;
--  with Ada.Environment;
with Ada.Exceptions;
--  with Ada.IO_Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada; use Ada;

with GNAT.Command_Line; use GNAT.Command_Line;
--  with GNAT.Exception_Traces;
with GNAT.OS_Lib; use GNAT.OS_Lib;
--  with GNAT.String_Split; use GNAT.String_Split;
with GNAT.Traceback.Symbolic;

--  with Ssprep.TempDir;
--  with Ssprep.Utilities;
with Ssprep.Version;
with Ada.Directories; use Ada.Directories;
with Ssprep.String_Vectors;

with Ssprep.ssfetch.projects;
with GNAT.Strings;
with Ada.Containers;
with GNAT.Exception_Traces;

procedure Ssprep.ssfetch.Main is
   Command_Name : constant String := Ada.Directories.Base_Name
     (Ada.Command_Line.Command_Name);

   procedure Print_Help;
   procedure Print_Help is
      use ASCII;
   begin
      Put_Line
        (Command_Name & " ("  & Ssprep.Version.Rev & ")" & LF & LF &
         "Syntax :" & LF &
         " " & Command_Name & " [Options] Projects [Options]" & LF &


         LF & "Options:" &
         LF & "  Symbol definition:" & LF &
         "    -w=dir|--workspace=dir    Sets the workspace to be dir default is <current dir>." & LF &
         "" & LF &

         LF & "  Database definition:" & LF &
         "    -I-file=file         Add file to file-finder." & LF &
         --  "    -I-url=file          Add url  to url-finder." & LF &

         LF & "  Misc:" & LF &
         "    --svn-export        Use svn export instead of svn check out to get source." & LF &
         "    --verbose           Increase verbosity." & LF &
         "    --exceptions        Log all exceptions to standard output." & LF &
         "    -v|--version        Prints the current version." & LF &
         "    -h|-?|--help        Print this  text." & LF
        );

   end Print_Help;
   projects     : Ssprep.String_Vectors.Vector;
   file_finder  : aliased Ssprep.ssfetch.projects.file_finder;
   finders      : aliased Ssprep.ssfetch.projects.finders;
   HOME         : constant GNAT.Strings.String_Access := GNAT.OS_Lib.Getenv ("HOME");
   Workspace    : GNAT.Strings.String_Access;
   use type Ada.Containers.Count_Type;

begin
   finders.register (file_finder'Unrestricted_Access);
   file_finder.Add (Compose (Compose (HOME.all, Ssprep.SSPREP_PATH), "ssfetch-file.conf"));
   declare
      done : Boolean := False;
   begin
      loop
         case Getopt ("w= -workspace= v -verbose " &
                      "I-file= " &
                      "-svn-export " &
                      "-exceptions h ? -help -version") is
         when ASCII.NUL => exit;

         when '-' =>
            if Full_Switch = "-workspace" then
               Workspace := new String'(Parameter);

            elsif Full_Switch = "-verbose" then
               Verbosity := Verbosity + 1;

            elsif Full_Switch = "-version" then
               Put_Line (Ssprep.Version.Rev);
               done := True;

            elsif Full_Switch = "-help" then
               Print_Help;
               done := True;

            elsif Full_Switch = "-svn-export" then
               Svn_Export := True;

            elsif Full_Switch = "-I-file" then
               file_finder.Add (Ada.Directories.Full_Name (Parameter));

            elsif Full_Switch = "-exceptions" then
               GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback'Access);
               GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
            end if;

         when 'v' =>
            Put_Line (Ssprep.Version.Rev);
            done := True;

         when 'w' =>
            Workspace := new String'(Parameter);

         when 'h' | '?' =>
            Print_Help;
            done := True;

         when others =>
            raise Program_Error with "Erronous configuration of options got '" & Full_Switch & "'";
         end case;
      end loop;
      if done then
         return;
      end if;
   end;

   if Workspace = null then
      Workspace := new String'(Ada.Directories.Current_Directory);
   end if;

   Put_Line (1, "Workspace => " & Workspace.all);
   loop
      declare
         S : constant String := Get_Argument (False);
      begin
         exit when S'Length = 0;
         Put_Line (2, "Adding => " & S);
         projects.Append (S);
      end;
   end loop;

   if projects.Length > 0 then
      if not Ada.Directories.Exists (Workspace.all) then
         Ada.Directories.Create_Path (Workspace.all);
      end if;
   end if;



exception
   when Invalid_Switch    =>
      Put_Line (Standard_Error, "Invalid Switch " & Full_Switch);
      Print_Help;
      Put (Standard_Error, "Arguments:'");
      for i in 1 .. Ada.Command_Line.Argument_Count loop
         Put (Standard_Error, Ada.Command_Line.Argument (i) & " ");
      end loop;
      Put_Line (Standard_Error, "'.");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when Invalid_Parameter =>
      Put_Line (Standard_Error, "No parameter for " & Full_Switch);
      Print_Help;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : others =>
      Put_Line (Standard_Error, "");
      Put_Line (Standard_Error, Ada.Exceptions.Exception_Information (E));
      Put_Line (Standard_Error, GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Ssprep.ssfetch.main;
