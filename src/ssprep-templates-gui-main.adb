with Ssprep.Version;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;

procedure Ssprep.Templates.GUI.Main is
   Command_Name : constant String := Ada.Directories.Base_Name
     (Ada.Command_Line.Command_Name);
   procedure Print_Help;
   procedure Print_Help is
      use ASCII;
   begin
      Put_Line
        (Command_Name & " ("  & Ssprep.Version & ")" & LF & LF &
         "Syntax :" & LF &
         " " & Command_Name & " [Options] Template [Options]" & LF & LF &
         "Template:" & LF &
         "  Template to provide GUI for." & LF &
         LF & "Options:" & LF);

   end Print_Help;

begin
   loop
      case Getopt ("-xml h ? -help -version") is
         when ASCII.NUL => exit;
         when '-' =>
            if Full_Switch = "-xml" then
               null;
               if Full_Switch = "-version" then
                  Put_Line (Ssprep.Version);
               elsif Full_Switch = "-help" then
                  Print_Help;
                  return;
               end if;
            end if;
         when 'h' | '?' =>
            Print_Help;
            return;

         when others =>
            raise Program_Error with "Erronous configuration of options got '" & Full_Switch & "'";
      end case;
   end loop;
end Ssprep.Templates.GUI.Main;
