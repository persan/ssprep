-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------



--  Import tests and sub-suites to run
with ATestcase;

package body TestSuite is
   use AUnit.Test_Suites;


   --  Statically allocate test suite:
   Result : aliased Test_Suite;


   --  Statically allocate test cases:

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, ATestcase.Test_Case);
      return Result'Access;
   end Suite;

end TestSuite;