@@INCLUDE@@ ../Common/FileHeader.txt @_TEST_SUITE_PACKAGE_@ @_test_suite_package_file_@.adb
with AUnit.Test_Suites;

package @_TEST_SUITE_PACKAGE_@ is

   function @_TEST_SUITE_NAME_@ return AUnit.Test_Suites.Access_Test_Suite;
   --  Return the test suite

end @_TEST_SUITE_PACKAGE_@;
