with AUnit;
with AUnit.Test_Cases;

package test is

   type Test_Case is new AUnit.Test_Cases.Test_Case with null record;

   procedure Set_Up (T : in out Test_Case);
   --  Preparation performed before each routine

   procedure Tear_Down (T : in out Test_Case);
   --  Cleanup performed after each routine

   procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   function Name (T : Test_Case)
                  return AUnit.Message_String;
   --  Returns name identifying the test case

end test;