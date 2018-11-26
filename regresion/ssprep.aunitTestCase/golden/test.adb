with GNAT.Source_Info;
with AUnit.Assertions;
package body test is
   use AUnit;
   use AUnit.Assertions;

   --  Fixture elements


   ------------
   -- Set_Up --
   ------------

   procedure Set_Up (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Set_Up;

   ---------------
   -- Tear_Down --
   ---------------

   procedure Tear_Down (T : in out Test_Case) is
      pragma Unreferenced (T);
   begin
      null;
   end Tear_Down;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return Format (GNAT.Source_Info.File & ":A_Good_Test");
   end Name;


   -------------------------
   --  SampleTest
   -------------------------
   procedure SampleTest (Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure SampleTest (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case (Test);
      pragma Unreferenced (T);
   begin
      Assert (False, "TODO Implement Test");
   end SampleTest;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases.Registration;

   begin
      Register_Routine  (T, SampleTest'Access, "SampleTest");
   end Register_Tests;

end test;