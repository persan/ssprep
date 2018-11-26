with Ada.Command_Line;
with GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNAT.Strings;
with GNAT.Regpat;
with Ada.Directories;
package body Ssprep.Tests.Utilities is

   ------------------
   -- Get_Test_Dir --
   ------------------

   function Get_Test_Dir return String is
      Exec_Name : constant String := Ada.Command_Line.Command_Name;
      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceeded by the absolute or relative
      --  path, e.g. "c:\usr\bin\gcc.exe". Returns the absolute directory
      --  where "bin" lies (in the example "C:\usr").
      --  If the executable is not in a "bin" directory, return "".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         use GNAT.Regpat;

         Exec      : constant String  := GNAT.Directory_Operations.Format_Pathname (Normalize_Pathname (S), GNAT.Directory_Operations.UNIX);
         Matcher   : constant Pattern_Matcher :=
                       Compile ("(.*tests)/bin/.*" & Ada.Directories.Base_Name (Exec));
         Matches   :  Match_Array (1 .. Paren_Count (Matcher));
      begin
         Match (Matcher, Exec, Matches);
         if Matches (1) /= No_Match then
            return Exec (Matches (1).First .. Matches (1).Last);
         else
            return "";
         end if;
      end Get_Install_Dir;

      --  Beginning of Executable_Prefix_Path

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      declare
         Ret : GNAT.Strings.String_Access := Locate_Exec_On_Path (Exec_Name);
      begin
         if Ret /= null and then Ret.all /= "" then
            declare
               R : constant String := Get_Install_Dir (Ret.all);
            begin
               Free (Ret);
               return R;
            end;
         end if;
      end;
      for J in reverse Exec_Name'Range loop
         if Exec_Name (J) = Directory_Separator then
            return Get_Install_Dir (Exec_Name);
         end if;
      end loop;
      return "";
   end Get_Test_Dir;
end Ssprep.Tests.Utilities;
