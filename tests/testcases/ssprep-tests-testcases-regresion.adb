with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.String_Split;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with AUnit.Assertions; use AUnit.Assertions;

package body Ssprep.Tests.Testcases.Regresion is

   function locate (f : String) return String
   is
      function inner (path : String) return String
      is
         name : constant String := Ada.Directories.Compose (path, f);
      begin
         if Ada.Directories.Exists (name) then
            return name;
         elsif path'Length > 2 then
            return inner (Ada.Directories.Containing_Directory (path));
         else
            return "";
         end if;

      end inner;
   begin
      return inner (Ada.Directories.Current_Directory);
   end locate;
   --------------------
   -- Register_Tests --
   --------------------
   function Suite return Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite := AUnit.Test_Suites.New_Suite;
      reultsPath : constant String := Compose (locate ("regresion"), "testsuite.res");
      procedure parse (Line : String) is
         s : GNAT.String_Split.Slice_Set;
         use GNAT.String_Split;
         t : Test_Case_Access;
      begin
         Create (s, Line, ":");
         if Slice_Count (s) > 1 then
            t := new Test_Case;
            t.Test_Name := new String'(Slice (s, 1));
            t.OK := Slice (s, 2) = "OK";
            Result.Add_Test (T => t);
         end if;
      end parse;

   begin
      if Exists (reultsPath) then
         declare
            f : Ada.Text_IO.File_Type;
         begin
            Open (f, In_File, reultsPath);
            while not End_Of_File (f) loop
               parse (Get_Line (f));
            end loop;
         end;
      end if;
      return Result;
   end Suite;


   ----------
   -- Name --
   ----------

   function Name
     (T : Test_Case)
      return AUnit.Message_String
   is
   begin
      return Format (T.Test_Name.all);
   end Name;

   -------------------------
   --  eval
   -------------------------
   procedure eval (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
   begin
      Assert (T.OK, "Regresion " & T.Test_Name.all & " Failed");
   end eval;

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Registration.Register_Routine (T, eval'Access, "Evaluate");
   end Register_Tests;


end Ssprep.Tests.Testcases.Regresion;
