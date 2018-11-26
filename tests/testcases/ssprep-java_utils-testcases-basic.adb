with GNAT.Source_Info;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with AUnit.Assertions; use AUnit.Assertions;
package body Ssprep.Java_Utils.Testcases.Basic is
   use AUnit;
   use ASCII;
   --  Fixture elements

   Source : constant String := "// sdfljk class Error" & LF &
              "  package org.sandat.play ; / adsfsdf" & LF &
              "   Class Error ( classess) {" & LF &
              "  package org.FOOL.play ; / adsfsdf" & LF &
              "public class CorrectData extends AbstractUIPlugin {{" & LF &
              "    package err" & LF &
              " Class appa"  & LF &
              "";
   procedure Set_Up_Case (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Set_Up_Case;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return Format (GNAT.Source_Info.File);
   end Name;


   -------------------------
   --  test_Get_Class_Path
   -------------------------
   procedure Test_Get_Package (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Actual   : constant String := Get_Package (Source);
      Expected : constant String := "org.sandat.play";
   begin
      Assert (Actual = Expected, "Got:'" & Actual & "', Expected: '" & Expected & "'");
   end Test_Get_Package;

   -------------------------
   --  Get_Package_Dir
   -------------------------
   procedure Get_Package_Dir (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Actual   : constant String := Get_Package_Dir (Source);
      Expected : constant String := Format_Pathname ("org/sandat/play");
   begin
      Assert (Actual = Expected, "Got:'" & Actual & "', Expected: '" & Expected & "'");
   end Get_Package_Dir;

   -------------------------
   --  test_Get_Class
   -------------------------
   procedure Test_Get_Class (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Result : constant String := Get_Class (Source);
   begin
      Assert (Result = "CorrectData", "class did not resolve got:'" & Result & "'");
   end Test_Get_Class;

   -------------------------
   --  test_Get_Class_Path
   -------------------------
   procedure Test_Get_Class_Path (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Result   : constant String := Get_Class_Path (Source);
      Expected : constant String := Format_Pathname ("org\sandat\play\CorrectData.java");
   begin
      Assert (Result = Expected, "class path did not resolve got:'" & Result & "'");
   end Test_Get_Class_Path;
   -------------------------
   --  test_write
   -------------------------
   procedure Test_Write (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
      pragma Unreferenced (T);
   begin
      Write (".", Source);
   end Test_Write;
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Get_Package'Access, "Test_Get_Package");
      Register_Routine (T, Get_Package_Dir'Access, "Get_Package_Dir");
      Register_Routine (T, Test_Get_Class'Access, "Test_Get_Class");
      Register_Routine (T, Test_Get_Class_Path'Access, "Test_Get_Class_Path");
      Register_Routine (T, Test_Write'Access, "Test_Write");
   end Register_Tests;

end Ssprep.Java_Utils.TestCases.Basic;
