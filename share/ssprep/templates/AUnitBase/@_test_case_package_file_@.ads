@@IF@@ @_AUNIT_@ = "AUNIT1.X"
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AUnit.Test_Cases;      use AUnit.Test_Cases;
@@ELSE@@
with AUnit;
with AUnit.Test_Cases;
@@END_IF@@

package @_test_case_package_@ is

   type @_TEST_CASE_TYPE_@ is new AUnit.Test_Cases.Test_Case with null record;

   @@IF@@ @_OVERRIDE_SET_UP_@
   procedure Set_Up (T : in out @_TEST_CASE_TYPE_@);
   --  Preparation performed before each routine

   @@END_IF@@
   @@IF@@ @_OVERRIDE_TEAR_DOWN_@
   procedure Tear_Down (T : in out @_TEST_CASE_TYPE_@);
   --  Cleanup performed after each routine

   @@END_IF@@
   procedure Register_Tests (T : in out @_TEST_CASE_TYPE_@);
   --  Register routines to be run

   function Name (T : @_TEST_CASE_TYPE_@)
   @@IF@@ @_AUNIT_@ = "AUNIT1.X"
                  return String_Access;
   @@ELSE@@
                  return AUnit.Message_String;
   @@END_IF@@
   --  Returns name identifying the test case

end @_test_case_package_@;
