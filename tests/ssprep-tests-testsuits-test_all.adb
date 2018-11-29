pragma Warnings (Off);
with Ssprep.Tests.Testcases.Test_Transalte_File;
with Ssprep.Templates.Test_Templates;
with Ssprep.Java_Utils.TestCases.Basic;
with Ssprep.ConfigParsers.Testcases.Testbasic;
with Ssprep.Tests.Testcases.Regresion;
package body Ssprep.Tests.Testsuits.Test_All is
   use AUnit.Test_Suites;

   Result              : aliased Test_Suite;
   Test_Templates      : aliased Templates.Test_Templates.Test_Case;
   Test_Transalte_File : aliased Tests.Testcases.Test_Transalte_File.Test_Case;
   Basic               : aliased Java_Utils.TestCases.Basic.Test_Case;
   Testcases           : aliased ConfigParsers.Testcases.Testbasic.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_Templates'Access);
      Add_Test (Result'Access, Test_Transalte_File'Access);
      Add_Test (Result'Access, Basic'Access);
      Add_Test (Result'Access, Basic'Access);
      Add_Test (Result'Access, Tests.Testcases.Regresion.Suite);
      return Result'Access;
   end Suite;

end Ssprep.Tests.Testsuits.Test_All;
