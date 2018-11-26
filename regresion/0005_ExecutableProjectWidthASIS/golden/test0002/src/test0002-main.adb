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
with test0002.Version;
procedure test0002.Main is
   Command_Name : constant String := Ada.Directories.Simple_Name
     (Ada.Command_Line.Command_Name);
   --![output] output : access String;
   Verbosity : Natural := 0;
   procedure Print_Help;

   procedure Print_Help is
      use ASCII;
   begin
      pragma Compile_Time_Warning (True, "To be completed help.");
      Put_Line
        (Command_Name & " Version: " & Version.Full_Version);
      Put_Line
        ("Syntax:" & LF &
         "  " & Command_Name & "");
      Put_Line
        ("Options:" & LF &
         --![output]"  -o=dir          Set output dir to dir." & LF &
         "  -v | --verbose  Increase verbosity." & LF &
         "  --version       Print version and exit." & LF &
         "  -h|-?|--help    Print this text");
   end Print_Help;
begin
   loop
      case Getopt ("h ? -help -version " &
                   "v -verbosity ") is
         when ASCII.NUL => exit;
            pragma Compile_Time_Warning (True, "To be completed options.");
         when '-' =>
            if Full_Switch = "-help" then
               Print_Help;
               return;
            elsif Full_Switch = "-verbosity" then
               Verbosity := Verbosity + 1;
            elsif Full_Switch = "-version" then
               Put_Line (Version.Version);
               return;
            elsif Full_Switch = "-<Your Switch here>" then
               null;
            end if;


         when 'v' =>
            Verbosity := Verbosity + 1;
         when 'h' | '?' =>
            Print_Help;
            return;
            --![output] when 'o' =>
            --![output]    output := new Sring (Parameter);
         when others =>
            raise Program_Error;         -- cannot occur!
      end case;
   end loop;


   loop
      declare
         S : constant String := Get_Argument (True);
      begin
         exit when S'Length = 0;
         --# TODO Process the parameters
         null; pragma Compile_Time_Warning (True, "To be compleated arguments.");
      end;
   end loop;

exception
   when Invalid_Switch    =>
      Put_Line ("Invalid Switch " & Full_Switch);
      Print_Help;
   when Invalid_Parameter =>
      Put_Line ("No parameter for " & Full_Switch);
      Print_Help;
   when E : others =>
      Put_Line (Ada.Exceptions.Exception_Message (E));
      Print_Help;
end test0002.Main;