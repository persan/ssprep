pragma Warnings (Off);
with Ssprep.Tests.Testcases.Test_Transalte_File;
with Ssprep.Templates.Test_Templates;
with Ssprep.Java_Utils.TestCases.Basic;
with Ssprep.ConfigParsers.Testcases.Testbasic;
with Ssprep.Tests.Testcases.Regresion;
package body Ssprep.Tests.Testsuits.Test_All is
   use AUnit.Test_Suites;

   Result : aliased Test_Suite;
   Test_1 : aliased Templates.Test_Templates.Test_Case;
   Test_2 : aliased Tests.Testcases.Test_Transalte_File.Test_Case;
   Test_3 : aliased Java_Utils.TestCases.Basic.Test_Case;
   Test_4 : aliased ConfigParsers.Testcases.Testbasic.Test_Case;

   -----------
   -- Suite --
   -----------

   function Suite return Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      Add_Test (Result'Access, Test_2'Access);
      Add_Test (Result'Access, Test_3'Access);
      Add_Test (Result'Access, Test_4'Access);
      Add_Test (Result'Access, Testcases.Regresion.Suite);
      return Result'Access;
   end Suite;

end Ssprep.Tests.Testsuits.Test_All;
