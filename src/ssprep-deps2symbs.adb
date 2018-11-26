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
with Ada.Command_Line;
with Ada.Directories;
with GNAT.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ssprep.Transformers;
with GNAT.String_Split;
with GNAT.Spitbol;
with Ada.Strings.Unbounded;
with GNAT.Directory_Operations;
procedure Ssprep.Deps2symbs is
   use Ada.Strings.Unbounded;
   use GNAT.Command_Line;
   use GNAT.Directory_Operations;
   use GNAT.Spitbol;
   Command_Name : constant String := Ada.Directories.Simple_Name
     (Ada.Command_Line.Command_Name);

   procedure Print_Help;

   procedure Print_Help is
      use ASCII;
   begin
      Ada.Text_IO.Put_Line
        (Command_Name & " Version:");
      Ada.Text_IO.Put_Line
        ("Syntax:" & LF &
         "  " & Command_Name & " sourcefile outfile");
      Ada.Text_IO.Put_Line
        ("Options:" & LF &
         "  -x=file(s)         Exclude file(s)  files is a ',' separated list of simple" & LF &
         "                       filenames to be ignored" & LF &
         "  -h|-?|--help       Print this text");

      Ada.Text_IO.Put_Line
        ("Abstract:" & LF &
         " Reads dependencies from ""gnatmake -M"" output and stores the result in " & LF &
         " symbolfile that could be read by ssprep." & LF &
         " The created symbols are" & LF &
         " 'Source_Dirs' a vector of all source dirs found." & LF &
         " 'Impl' / 'Impl_Files' vectors containing all bodies and therir respective files." & LF &
         " 'Spec' / 'Spec_Files' vectors containing all bodies and therir respective files.");

      Ada.Text_IO.Put_Line
        ("Notes:" & LF &
         " If no output file is given then standard output is used." & LF &
         " If no input file is given then standard inputput is used."
        );
   end Print_Help;
   T             : Transformers.Transformer;
   Data_Read     : Boolean := False;
   Out_File_Name : VString;
begin
   loop
      case Getopt ("h ? -help x= o:") is
         when ASCII.NUL => exit;

         when '-' =>
            if Full_Switch = "-help" then
               Print_Help;
               return;
            end if;

         when 'o' =>
            Out_File_Name := V (Parameter);
         when 'x' =>
            declare
               Sl : GNAT.String_Split.Slice_Set;
               use GNAT.String_Split;
            begin
               Create (Sl, Parameter, ",");
               for I in 1 .. Slice_Count (Sl) loop
                  T.Add_Ignore (Slice (Sl, I));
               end loop;
            end;

         when 'h' | '?' =>
            Print_Help;
            return;

         when others =>
            raise Program_Error;         -- cannot occur!
      end case;
   end loop;

   loop
      declare
         S : constant String := Get_Argument (True);
      begin
         exit when S'Length = 0;
         if Length (Out_File_Name) = 0 then
            Out_File_Name := V (Base_Name (S, File_Extension (S)) & ".xml");
         end if;
         T.Read (S);
         Data_Read := True;
      end;
   end loop;
   if Data_Read then
      T.Write (S (Out_File_Name));
   end if;
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
end Ssprep.Deps2symbs;
