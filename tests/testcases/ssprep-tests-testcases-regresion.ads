with AUnit.Test_Suites;
with AUnit;
with AUnit.Test_Cases;
with GNAT.Strings;
package Ssprep.Tests.Testcases.Regresion is
   use AUnit;
   function Suite return Test_Suites.Access_Test_Suite;

   type Test_Case is new AUnit.Test_Cases.Test_Case with  record
      Test_Name : GNAT.Strings.String_Access;
      OK        : Boolean;
   end record;
   type Test_Case_Access is access all Test_Case'Class;
   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   function Name (T : Test_Case)
                  return AUnit.Message_String;
   --  Returns name identifying the test case

end Ssprep.Tests.Testcases.Regresion;
