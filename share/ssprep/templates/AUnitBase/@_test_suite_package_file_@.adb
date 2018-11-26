@@INCLUDE@@ ../Common/FileHeader.txt @_TEST_SUITE_PACKAGE_@ @_test_suite_package_file_@.adb

@@IF@@ @_AUNIT_@ = "AUNIT1.X"
with AUnit.Test_Suites; use AUnit.Test_Suites;
@@END_IF@@


--  Import tests and sub-suites to run
@@TABLE@@
with @_TEST_SUITE_PACKAGES_@;
@@END_TABLE@@

package body @_TEST_SUITE_PACKAGE_@ is
   use AUnit.Test_Suites;


   --  Statically allocate test suite:
   Result : aliased Test_Suite;


   --  Statically allocate test cases:
   @@TABLE@@
   @@IF@@ @_TEST_SUITE_TESTS_KIND_@ = "TEST_CASE"
   Test_@_TABLE_LINE_@ : aliased @_TEST_SUITE_PACKAGES_@.@_TEST_SUITE_TESTS_@;
   @@END_IF@@
   @@END_TABLE@@

   ---@_REPLACE_ALL(./-):TEST_SUITE_NAME_@---
   -- @_TEST_SUITE_NAME_@ --
   ---@_REPLACE_ALL(./-):TEST_SUITE_NAME_@---

   function @_TEST_SUITE_NAME_@ return AUnit.Test_Suites.Access_Test_Suite is
   begin
      @@TABLE@@
      @@IF@@ @_TEST_SUITE_TESTS_KIND_@ = "TEST_CASE"
      Add_Test (Result'Access, Test_@_TABLE_LINE_@'Access);
      @@ELSE@@
      Add_Test (Result'Access, @_TEST_SUITE_PACKAGES_@.@_TEST_SUITE_TESTS_@);
      @@END_IF@@
      @@END_TABLE@@
      return Result'Access;
   end Suite;

end @_TEST_SUITE_PACKAGE_@;
