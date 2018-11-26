@@IF@@ @_AUNIT_@ = "AUNIT1.X"
with Aunit.Test_Cases.Registration; use AUnit.Test_Cases.Registration;
with AUnit.Assertions;              use AUnit.Assertions;
@@END_IF@@
with GNAT.Source_Info;
with AUnit.Assertions;
package body @_test_case_package_@ is
   @@IF@@ @_AUNIT_@ = "AUNIT1.X"
   @@ELSE@@
   use AUnit;
   use AUnit.Assertions;
   @@END_IF@@

   --  Fixture elements


   @@IF@@ @_OVERRIDE_SET_UP_@
   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out @_TEST_CASE_TYPE_@) is
      pragma Unreferenced (T);
   begin
      null;
   end Set_Up;

   @@END_IF@@
   @@IF@@ @_OVERRIDE_TEAR_DOWN_@
   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out @_TEST_CASE_TYPE_@) is
      pragma Unreferenced (T);
   begin
      null;
   end Tear_Down;

   @@END_IF@@
   ----------
   -- Name --
   ----------

   function Name (T : @_TEST_CASE_TYPE_@)
   @@IF@@ @_AUNIT_@ = "AUNIT1.X"
                  return String_Access is
   @@ELSE@@
                  return AUnit.Message_String is
   @@END_IF@@
      pragma Unreferenced (T);
   begin
   @@IF@@ @_AUNIT_@ = "AUNIT1.X"
      return new String'(GNAT.Source_Info.File & ":@_TEST_CASE_DESCRIPTION_@");
   @@ELSE@@
      return Format (GNAT.Source_Info.File & ":@_TEST_CASE_DESCRIPTION_@");
   @@END_IF@@
   end Name;


   -------------------------
   --  SampleTest
   -------------------------
   procedure SampleTest (Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure SampleTest (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test); 
      pragma Unreferenced (T);
   begin
      Assert (False, "TODO Implement Test");  
   end SampleTest;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out @_TEST_CASE_TYPE_@) is
   
      use Test_Cases.Registration;

   begin
      Register_Routine  (T, SampleTest'Access, "SampleTest");
   end Register_Tests;

end @_test_case_package_@;
