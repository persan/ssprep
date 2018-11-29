-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------
with Ada.Command_Line;
with Ada.Directories;
with GNAT.Command_Line; use  GNAT.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ssprep.Version;
with GNAT.OS_Lib;

with Ssprep.Savepoints;
with Ssprep.GNATls;
with Ssprep.String_Vectors;
with GNAT.Regpat;
with Templates_Parser;
with GNAT.Traceback.Symbolic;
with GNATCOLL.Projects;
use GNATCOLL.Projects;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Ada.Strings.Fixed.Equal_Case_Insensitive;

procedure Ssprep.GetBuildOrder.Main is
   Command_Name    : constant String := Ada.Directories.Simple_Name
     (Ada.Command_Line.Command_Name);
   use GNAT.OS_Lib;
   Children_Failed : exception;
   Root            : GNATCOLL.Projects.Project_Tree;
   Env             : GNATCOLL.Projects.Project_Environment_Access;
   function "=" (L, R : String) return Boolean renames Ada.Strings.Fixed.Equal_Case_Insensitive;
   procedure Print_Help;

   procedure Print_Help is
      use ASCII;
   begin
      Put_Line
        (Command_Name & " Version: " & Version.Rev & "-" & Version.Date);
      Put_Line
        ("Syntax:" & LF &
           "  " & Command_Name & "[options] [GNAT-projectfiles] [options]");
      Put_Line
        ("Options:" & LF &
           "  -v|--verbose       Be verbose." & LF &
           "  -p|--project       Print Project files." & LF &
           "  -d|--dir           Print Project dirs (default)." & LF &
           "  -n|--name          Print Project names." & LF &
           "  -e=regexp |" & LF &
           "   --excludes=regexp Exclude project paths matching regexp from printout" & LF &
           "  --include-gnat     Include projects located in the GNAT installation" & LF &
           "  --externally-built Include projecs that are externally built." & LF &
           "  --echo command     Echo command where the strings @_dir_@ @_name_@ @_path_@ are replaced acordinly" & LF &
           "                     replaced with acordingly (must be last on line).:" & LF &
           "                      @_dir_@     : The directory name of the project" & LF &
           "                      @_name_@    : The simple name of the project file without "".gpr#""" & LF &
           "                      @_path_@    : The path of the project" & LF &
           "  --exec command     Execute command where the strings @_dir_@ @_name_@ @_path_@ are replaced (must be last on line)." & LF &
           "  --cwd              Change dir to projects enclosing dir before execute" & LF &
           "  --version          Print version and exit." & LF &
           "  --path=PATH        Prepend PATH to ADA_PROJECT_PATH during search." & LF &
           "  -h|-?|--help       Print this text" & LF);
      Put_Line
        ("Abstract:" & LF &
           "  Reads the gnat project files and sorts them in build order or" & LF &
           "  executs commands with the import information in order." & LF &
           "  It could be used to:" & LF &
           "    Generate build scripts that will build in correct order." & LF &
           "    Projects with dependencies for automated builders such as hudson." & LF &
           "    Anything that reqirers the import structure of a gnat project set.");

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end Print_Help;


   Echo                     : String_Vectors.Vector;
   Exec                     : String_Vectors.Vector;
   Executed                 : Boolean := False;
   NewPath                  : String_Vectors.Vector;
   Displayed_Dirs           : String_Vectors.Vector;
   Excludes                 : String_Vectors.Vector;
   ChangeDir                : Boolean :=  True;

   procedure Process (Line : String_Vectors.Vector; Proj : Project_Type; Execute : Boolean) is
      Args         : GNAT.OS_Lib.String_List_Access;
      Cursor       : String_Vectors.Cursor := Line.First;
      Ret          : Integer;
      I            : Integer;
      Translations : Templates_Parser.Translate_Set;

      function Expand (S : String) return String is
      begin
         return Templates_Parser.Translate (S, Translations);
      end Expand;
      use Templates_Parser;
      Cmd          : String_Access;
   begin
      if Line.Is_Empty then
         raise Program_Error with "No command Given";
      end if;

      Displayed_Dirs.Append (String (Filesystem_String'(Dir_Name (Proj.Project_Path))));
      Insert (Translations, Assoc ("path",  String (Filesystem_String'(Full_Name (Proj.Project_Path)))));
      Insert (Translations, Assoc ("dir", String (Filesystem_String'(Dir_Name (Proj.Project_Path)))));
      Insert (Translations, Assoc ("name", Proj.Name));

      if Execute then
         Args := new GNAT.OS_Lib.String_List (1 ..  Integer (Line.Length) - 1);
         I := 1;
         Cursor := String_Vectors.Next (Cursor);
         Cmd := Locate_Exec_On_Path (String_Vectors.Element (Line.First));
         if Cmd = null then
            raise Program_Error with "Command not found " & String_Vectors.Element (Line.First);
         end if;
         Put (Cmd.all);

         while String_Vectors.Has_Element (Cursor) loop
            Args (I) := new String'(Expand (String_Vectors.Element (Cursor)));
            Put (" " & Args (I).all);
            I := I + 1;
            Cursor := String_Vectors.Next (Cursor);
         end loop;
         New_Line;

         if ChangeDir then
            Change_Dir (Dir (Proj.Project_Path));
         end if;
         Ret := GNAT.OS_Lib.Spawn (Cmd.all, Args.all);
      else
         for I of Line loop
            Put (Expand (I) & " ");
         end loop;
         New_Line;
      end if;

      Free (Args);
      if Ret /= 0 then
         GNAT.OS_Lib.Set_Errno (Ret);
         raise Children_Failed;
      end if;
   end Process;


   function In_Exludes (Path : String) return Boolean is
      Cursor : String_Vectors.Cursor := Excludes.First;
   begin
      while String_Vectors.Has_Element (Cursor) loop
         if GNAT.Regpat.Match (String_Vectors.Element (Cursor), Path) then
            return True;
         end if;
         String_Vectors.Next (Cursor);
      end loop;
      return False;
   end In_Exludes;

   procedure Process (Proj : Project_Type) is
   begin
      if In_Exludes (Proj.Project_Path.Dir.Display_Dir_Name) then
         --  if (not Include_Externally_Built and Proj.Externally_Built) or In_Exludes (Proj.Display_Dir_Name) then
         return;
      end if;
      if Proj.Attribute_Value (Attribute => Build ("", "externally_built")) = "true" then
         return;
      end if;
      if Proj.Direct_Sources_Count = 0 then
         return;
      end if;

      if not Exec.Is_Empty  then
         Process (Exec, Proj, True);
      elsif not Echo.Is_Empty then
         Process (Echo, Proj, False);
      else
         case DumpFormat is
            when Format_Dir =>
               if Displayed_Dirs.Contains (Proj.Project_Path.Dir.Display_Dir_Name) then
                  return;
               end if;
               Put_Line (Proj.Project_Path.Dir.Display_Dir_Name);
               Displayed_Dirs.Append (Proj.Project_Path.Dir.Display_Dir_Name);
            when Format_Proj =>
               Put_Line (Proj.Project_Path.Display_Dir_Name);
            when Format_Proj_Name =>
               Put_Line (Proj.Name);
            when Format_Verbose =>
               Put_Line (Proj.Project_Path.Display_Dir_Name);
         end case;
      end if;
   end Process;

   Options                    : constant String :=
                                  "v -verbose " &
                                  "p -project " &
                                  "d -dir " &
                                  "n -name " &
                                  "-cwd " &
                                  "aP= " &

                                  "e= -exclude= " &
                                  "-externally-built " &
                                  "-path= " &
                                  "-dump " &
                                  "h ? -help -version";
   GNAT_Version               : GNAT.Strings.String_Access;
begin
   Initialize (Env);
   Env.Set_Path_From_Gnatls ("gnatls", GNAT_Version);

   Initialize_Option_Scan ('-', False, "-echo -exec");
   Param_Loop : loop
      case Getopt (Options) is
         when ASCII.NUL => exit Param_Loop;
         when '-'  =>
            if Full_Switch = "-help" then
               Print_Help;
               return;
            elsif Full_Switch = "-cwd" then
               ChangeDir  := True;
            elsif Full_Switch = "-exclude" then
               Excludes.Append (Parameter);

            elsif Full_Switch = "-verbose" then

               Verbosity_Level := Verbosity_Level + 1;
            elsif Full_Switch = "-project" then
               DumpFormat := Format_Proj;
            elsif Full_Switch = "-dir" then
               DumpFormat := Format_Dir;
            elsif Full_Switch = "-name" then
               DumpFormat := Format_Proj_Name;
            elsif Full_Switch = "-version" then
               Put_Line (Version.Rev);
               return;
            end if;


         when 'a'  =>
            if Full_Switch = "aP" then
               --                 NewPath.Append (GNATls.ADA_PROJECT_PATH);
               --
               --                 declare
               --                    p : GNATCOLL.VFS.File_Array := env.Predefined_Project_Path;
               --                 begin
               --
               --                    env.Predefined_Project_Path (Parameter));
               --                 end;
               null;
            else
               raise Invalid_Switch;
            end if;
         when 'e'  =>
            Excludes.Append (Parameter);
         when 'v'  =>
            Verbosity_Level := Verbosity_Level + 1;
         when 'p'  =>
            DumpFormat := Format_Proj;
         when 'd'  =>
            DumpFormat := Format_Dir;
         when 'n'  =>
            DumpFormat := Format_Proj_Name;
         when 'h' | '?' =>
            Print_Help;
            return;
         when others =>
            raise Program_Error;         -- cannot occur!
      end case;
   end loop Param_Loop;

   Goto_Section ("-echo");
   loop
      declare
         S : constant String := Get_Argument (False);
      begin
         exit when S'Length = 0;
         Echo.Append (S);
      end;
   end loop;
   Goto_Section ("-exec");
   loop
      declare
         S : constant String := Get_Argument (False);
      begin
         exit when S'Length = 0;
         Exec.Append (S);
      end;
   end loop;

   while Getopt (Options) /= ASCII.NUL loop
      null;
   end loop;

   --     if not Include_GNAT then
   --        declare
   --           P : String_Access := GNAT.OS_Lib.Locate_Exec_On_Path ("gnatls");
   --        begin
   --           if P /= null then
   --              declare
   --                 S0 : constant String := Dir_Name (P.all);
   --                 S1 : constant String := Dir_Name (S0 (S0'First .. S0'Last - 1)) & ".*";
   --              begin
   --                 Excludes.Append (S1);
   --
   --              end;
   --              Free (P);
   --           end if;
   --        end;
   --     end if;

   declare
      Saved_Ada_PATH : Ssprep.Savepoints.Environment_Saver (new String'("GPR_PROJECT_PATH"));
      pragma Unreferenced (Saved_Ada_PATH);
   begin
      GNAT.OS_Lib.Setenv ("GPR_PROJECT_PATH", GNATls.ToString (NewPath));
      Ada_Project_Path := new String'(GNATls.ToString (GNATls.ADA_PROJECT_PATH));
      if Verbosity_Level > 0 then
         Put_Line  ("GPR_PROJECT_PATH := '" & Ada_Project_Path.all & "'.");
      end if;
   end;

   Initialize_Option_Scan ('-', False, "-echo -exec");

   declare
      S : constant Filesystem_String := Filesystem_String (Get_Argument (False));
   begin
      if S'Length > 0 then
         Root.Load (GNATCOLL.VFS.Create (S), Env);
         Executed := True;
      end if;
   end;

   if Executed then
      declare
         Iter    : Project_Iterator := Start (Root.Root_Project);
         Project : Project_Type;
      begin
         loop
            Project := Current (Iter);
            exit when Project = No_Project;
            Process (Current (Iter));
            Next (Iter);
         end loop;
      end;
   else
      Put_Line ("Did not find any projects");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;

exception
   when Invalid_Switch    =>
      Put_Line ("Invalid Switch " & Full_Switch);
      Print_Help;
   when Invalid_Parameter =>
      Put_Line ("No parameter for " & Full_Switch);
      Print_Help;
   when Children_Failed =>
      null;
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Information (E) &
                  GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
end Ssprep.GetBuildOrder.Main;
