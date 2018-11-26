-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------
with AUnit.Test_Suites;

package TestSuite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;
   --  Return the test suite

end TestSuite;