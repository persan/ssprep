with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure Ssprep.Lnexe is

   function  Temp return String is
   begin
      return "C:\temp\lnexe_temp__.adb";
   end Temp;

   function Command_Template (NAME, COMMAND : String) return  String is
   begin
      return "" &
      "with GNAT.OS_Lib; use GNAT.OS_Lib;" & ASCII.LF &
      "with Ada.Command_Line; use Ada.Command_Line;" & ASCII.LF &
      "procedure " & NAME & " is " & ASCII.LF &
      "   Command : constant String := """ & COMMAND & """;" & ASCII.LF &
      "   Args : Argument_List_Access;" & ASCII.LF &
      "begin" & ASCII.LF &
      "   Args := new Argument_List (1 .. Ada.Command_Line.Argument_Count);" & ASCII.LF &
      "   for I in 1 .. Argument_Count loop" & ASCII.LF &
      "      Args (I) := new String'(Ada.Command_Line.Argument (I));" & ASCII.LF &
      "   end loop;" & ASCII.LF &
      "   Set_Errno (Spawn (Command, Args.all));" & ASCII.LF &
      "   Free (Args);" & ASCII.LF &
      "end " & NAME & ";";
   end Command_Template;

begin
   declare
      COMMAND      : constant String := Ada.Command_Line.Argument (1);
      NAME         : constant String := Ada.Directories.Base_Name (COMMAND);
      F            : Ada.Text_IO.File_Type;
      Args         : GNAT.OS_Lib.Argument_List_Access;
   begin
      Create (F, Out_File, Temp);
      Put_Line (F, Command_Template (NAME, COMMAND));
      Args := new GNAT.OS_Lib.Argument_List'(new String'(Temp),
                                             new String'("-gnatws"),
                                             new String'("-o"), new String'(NAME),
                                             new String'("-D"), new String'(Ada.Directories.Containing_Directory (Temp)));
      Close (F);
      Set_Errno (Spawn ("gnatmake", Args.all));
      Free (Args);
   end;
end Ssprep.Lnexe;
