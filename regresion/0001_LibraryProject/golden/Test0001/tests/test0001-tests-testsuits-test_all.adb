-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------



--  Import tests and sub-suites to run
with Test0001.Tests.Testcases.Compile;

package body Test0001.Tests.TestSuits.Test_All is
   use AUnit.Test_Suites;


   --  Statically allocate test suite:
   Result : aliased Test_Suite;


   --  Statically allocate test cases:
   Test_1 : aliased Test0001.Tests.Testcases.Compile.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;

end Test0001.Tests.TestSuits.Test_All;