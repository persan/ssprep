with Ada.Calendar;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.Calendar.Time_IO;
with GNAT.Command_Line; use GNAT.Command_Line;

with @_project_@.Version;

procedure Checkrelease is
   Command_Name : constant String := Ada.Directories.Base_Name (Ada.Command_Line.Command_Name);
   procedure Help is
      use ASCII;
   begin
      Put_Line
        (Command_Name & " "  & @_project_@.Version.Full_Version & Lf &
         "Syntax:" & Lf &
         "  " & Command_Name & " [options]" & Lf &
         "Options:" & Lf &
         "  --dirtylist=file    Returns error if file isent empty." & Lf &
         "  --svnversions=file  Returns error if the current version is in file" & Lf &
         "                        where file is the output from 'svn ls ${tags}." & Lf &
         "  --releasenote=file  Verifies that the release note at least contins " & lf &
         "                        the release identifier and the current date " & Lf &
         "                        in the first 20 lines." & Lf &
         "  -v                  Print current version." & Lf &
         "  -?|-h|--help        Print this text");

   end;

   procedure Check_Dirty (Path : String) is
      F : Ada.Text_IO.File_Type;
   begin
      Open (F, In_File, Path);
      while not End_Of_File (F) loop
         declare
            S : constant String := Get_Line (F);
         begin
            if S /= "" then
               Put_Line ("Dirty files:" & S);
               Set_Exit_Status (Failure);
            end if;
         end;
      end loop;
      Close (F);
   end Check_Dirty;

   procedure Check_Release_Note (Path : String) is
      F                : Ada.Text_IO.File_Type;
      Version_Ok       : Boolean := False;
      Date_Ok          : Boolean := False;
      Now              : constant String := GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, GNAT.Calendar.Time_IO.ISO_Date);
   begin

      Open (F, In_File, Path);
      while not End_Of_File (F) loop
         declare
            S : constant String := Get_Line (F);
         begin
            if Index (S, Now) /= 0 and Line (F) < 20 then
               Date_Ok := True;
            elsif Index (S, @_project_@.Version.Version ) /= 0 and Line (F) < 20 then
               Version_Ok := True;
            end if;
         end;
      end loop;
      Close (F);
      if not Version_Ok then
         Put_Line ("Current version '" & @_project_@.Version.Version  & "' is not mentioned in the relese note");
         Set_Exit_Status (Failure);
      end if;

      if not Date_Ok then
         Put_Line ("Release Date '" & Now & "' is not mentioned in the relese note");
         Set_Exit_Status (Failure);
      end if;
   end Check_Release_Note;


   procedure Check_Versions (Path : String) is
      F            : Ada.Text_IO.File_Type;
   begin
      Open (F, In_File, Path);
      while not End_Of_File (F) loop
         declare
            S : constant String := Get_Line (F);
         begin
            if @_project_@.Version.Version = S (S'First .. S'Last - 1) then
               Put_Line ("Version " &  @_project_@.Version.Version & " does alredy exists.");
               Set_Exit_Status (Failure);
            end if;
         end;
      end loop;
      Close (F);
   end Check_Versions;

begin
   begin
      loop
         case Getopt ("-dirtylist= -svnversions= -releasenote= v ? h -help") is
            when ASCII.NUL => exit;

            when '-' =>
               if Full_Switch = "-dirtylist" then
                  Check_Dirty (Parameter);

               elsif Full_Switch = "-svnversions" then
                  Check_Versions (Parameter);

               elsif Full_Switch = "-releasenote" then
                  Check_Release_Note (Parameter);

               elsif Full_Switch = "-help" then
                  Help;
                  return;

               end if;

            when 'v' =>
               Put_Line (@_project_@.Version.Version);
            when '?' | 'h' =>
               Help;
               return;
            when others =>
               raise Program_Error;         -- cannot occur!
         end case;
      end loop;
   end;
end;
