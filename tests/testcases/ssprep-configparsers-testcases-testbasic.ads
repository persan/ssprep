with AUnit;
with AUnit.Test_Cases;
package Ssprep.Configparsers.Testcases.Testbasic is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   function Name (T : Test_Case)
                  return AUnit.Message_String;
   --  Returns name identifying the test case
   procedure Set_Up_Case (Test : in out Test_Case);

end Ssprep.ConfigParsers.Testcases.Testbasic;
