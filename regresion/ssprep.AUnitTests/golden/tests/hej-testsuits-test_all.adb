-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------



--  Import tests and sub-suites to run
with hej.Testcases.Compile;

package body hej.TestSuits.Test_All is
   use AUnit.Test_Suites;


   --  Statically allocate test suite:
   Result : aliased Test_Suite;


   --  Statically allocate test cases:
   Test_1 : aliased hej.Testcases.Compile.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;

end hej.TestSuits.Test_All;