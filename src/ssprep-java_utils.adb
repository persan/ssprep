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
with GNAT.Regpat; use GNAT.Regpat;
with GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;

package body Ssprep.Java_Utils is


   -----------
   -- Write --
   -----------
   Package_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
                       GNAT.Regpat.Compile ("^\s*" &
                                            "package\s+([\w\.]+)"
                                            & ".*",
                                            Multiple_Lines + Single_Line);
   Class_Matcher : constant GNAT.Regpat.Pattern_Matcher :=
                     GNAT.Regpat.Compile ("^ *" &
                                          "(public\s+class|private\s+class|class)\s+([\w\.]+)"
                                          & ".*$",
                                          Multiple_Lines + Single_Line);
   Class_Name_Index  : constant := 2;
   procedure Write (Dir, Source : String) is
      Target_Dir  : constant String := Dir & GNAT.OS_Lib.Directory_Separator &  Get_Package_Dir (Source);
      Taget_File  : constant String := Dir & GNAT.OS_Lib.Directory_Separator & Get_Class_Path (Source);
      F           : Ada.Text_IO.File_Type;
      Fd          : GNAT.OS_Lib.File_Descriptor;
   begin
      if not Exists (Target_Dir) then
         Create_Path (Target_Dir);
      end if;
      Fd := GNAT.OS_Lib.Create_New_File (Taget_File, GNAT.OS_Lib.Binary);
      GNAT.OS_Lib.Close (Fd);
      Open (F, Out_File, Taget_File);
      Put_Line (F, Source);
      Close (F);
   end Write;

   -----------------
   -- Get_Package --
   -----------------

   function Get_Package (Source : String) return String is
      Result : Match_Array (1 .. Paren_Count (Package_Matcher));
   begin
      Match (Package_Matcher, Source, Result);
      if Result (1) /= No_Match then
         return Source (Result (1).First .. Result (1).Last);
      else
         return "";
      end if;

   end Get_Package;

   ---------------------
   -- Get_Package_Dir --
   ---------------------

   function Get_Package_Dir (Source : String) return String is
      Ret : String := Get_Package (Source);
   begin
      if Ret = "" then
         return "." & GNAT.OS_Lib.Directory_Separator;
      else
         for I in Ret'Range loop
            if Ret (I) = '.' then
               Ret (I) := GNAT.OS_Lib.Directory_Separator;
            end if;
         end loop;
         return Ret;
      end if;
   end Get_Package_Dir;

   ---------------
   -- Get_Class --
   ---------------

   function Get_Class (Source : String) return String is
      Result : Match_Array (1 .. Paren_Count (Class_Matcher));
   begin
      Match (Class_Matcher, Source, Result);
      if Result (Class_Name_Index) /= No_Match then
         return Source (Result (Class_Name_Index).First .. Result (Class_Name_Index).Last);
      else
         return "";
      end if;
   end Get_Class;

   --------------------
   -- Get_Class_Path --
   --------------------

   function Get_Class_Path (Source : String) return String is
   begin
      return Get_Package_Dir (Source) & GNAT.OS_Lib.Directory_Separator & Get_Class (Source) & Java_Suffix;
   end Get_Class_Path;

end Ssprep.Java_Utils;
