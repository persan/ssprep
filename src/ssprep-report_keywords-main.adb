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
procedure Ssprep.Report_Keywords.Main is
   Command_Name : constant String := Ada.Directories.Simple_Name
     (Ada.Command_Line.Command_Name);
   --![output] output : access String;

   procedure Print_Help;

   procedure Print_Help is
      use ASCII;
   begin
      Put_Line
        (Command_Name & " Version: " & Version.Rev);
      Put_Line
        ("Syntax:" & LF &
         "  " & Command_Name & "");
      Put_Line
        ("Options:" & LF &
         --![output]"  -o=dir          Set output dir to dir." & LF &
         --!"  -v              Be verbose." & Lf &
         "  --version       Print version and exit." & LF &
         "  -h|-?|--help    Print this text");
      Put_Line
        ("Abstract:" & LF
         & "  Reads a GNAT project faile and echos the folowing symbols in"
         & "  templates_parser foramt to be read be read by programs like ssprep."
        );

   end Print_Help;
   The_Reporter  : reporter;
   report_in_xml : Boolean := False;
   Report_File   : Ada.Text_IO.File_Access := Ada.Text_IO.Standard_Output;
   Out_File      : aliased Ada.Text_IO.File_Type;
   verbosity     : Integer := 0;
begin
   loop
      case Getopt ("h ? -help " &
                   "-version " &
                   "o= " &
                   "-OK= -ok=" &
                   "-xml " &
                   "-verbose v " &
                   "-words=  w= " &
                   "-Words=  W= " &
                   "c= -config=") is
         when ASCII.NUL => exit;
         when '-' =>
            if Full_Switch = "-words" then
               The_Reporter.add_word (Parameter);
            elsif Full_Switch = "-words" then
               The_Reporter.add_words (Parameter);
            elsif Full_Switch = "-help" then
               Print_Help;
               return;
            elsif Full_Switch = "-xml" then
               report_in_xml := True;
            elsif Full_Switch = "-config" then
               null; --#TODO config
            elsif Full_Switch = "-verbose" then
               verbosity := verbosity + 1;
            elsif Full_Switch = "-version" then
               Put_Line (Version.Rev);
               return;
            elsif Full_Switch = "-OK=" then
               declare
                  f : Ada.Text_IO.File_Type;
               begin
                  Open (f, In_File, Parameter);
                  while not End_Of_File (f) loop
                     The_Reporter.Add_OK (Get_Line (f));
                  end loop;
                  Close (f);
               end;
            elsif Full_Switch = "-ok=" then
               The_Reporter.Add_OK (Parameter);
            end if;


         when 'c' =>
            null; --#TODO config
         when 'w' =>
            The_Reporter.add_word (Parameter);
         when 'W' =>
            The_Reporter.add_words (Parameter);

         when 'v' =>
            verbosity := verbosity + 1;
         when 'o' =>
            Create (Out_File, Ada.Text_IO.Out_File, Parameter);
            Report_File := Out_File'Unchecked_Access;
         when 'h' | '?' =>
            Print_Help;
            return;
         when others =>
            raise Program_Error;         -- cannot occur!
      end case;
   end loop;


   declare
      S : constant String := Get_Argument (True);
   begin
      if  S'Length = 0 then
         The_Reporter.analyze (S);
      end if;
   end;

   if report_in_xml then
      The_Reporter.print_Report_In_Xml (Report_File.all);
   else
      The_Reporter.print_Report (Report_File.all);
   end if;
   Close (Out_File);

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
end Ssprep.Report_Keywords.Main;
