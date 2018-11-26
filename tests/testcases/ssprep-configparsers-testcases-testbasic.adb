with GNAT.Source_Info;
with AUnit.Assertions; use AUnit.Assertions;
package body Ssprep.Configparsers.Testcases.Testbasic is
   use AUnit;

   --  Fixture elements

   procedure Set_Up_Case (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Set_Up_Case;

   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return Format (GNAT.Source_Info.File);
   end Name;


   -------------------------
   --  SampleTest
   -------------------------
   procedure SampleTest (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      T  : RawConfigParser;
   begin
      T.Add_Section ("bulle");
      T.Set ("bulle", "String", "1");
      T.Set ("bulle", "Integer", 2);
      T.Set ("bulle", "Float", Float'(2.2));
      T.Add_Section ("Kalle");
      T.Set ("Kalle", "String", "1");
      T.Set ("", "Default", 999);
      T.Set ("", "Default-Form", "com.saabgroup");
      Assert (T.Get ("bulle", "String") = "1", "Wrong value Str");
      Assert (T.Get ("bulle", "Integer") = 2, "Wrong value Int");
      Assert (T.Get ("bulle", "Float") = Float (2.2), "Wrong value Float");
      T.Write ("hello.txt");
   end SampleTest;
   -------------------------
   --  SampleRead
   -------------------------
   procedure SampleRead (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      T  : RawConfigParser;
   begin
      T.Read ("hello.txt");
      T.Write ("hello2.txt");
   end SampleRead;

   procedure SampleRead2 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      T  : RawConfigParser;
   begin
      T.Read ("config.txt", Ignore_Nonexisting => False);
      declare
         R : constant String := T.Get ("bulle", "String");
         V : constant String :=  "1.Hej";
      begin
         Assert (R = V, "Got '" & R & "' Expected '" & V & "'");
      end;
      T.Write ("Config2.txt");
   end SampleRead2;
   -------------------------
   --  ReadProperties
   -------------------------
   procedure ReadProperties (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      T  : RawConfigParser;

   begin
      T.Read ("build.properties");
      T.Write ("build.test");
   end ReadProperties;
   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases.Registration;
   begin
      Register_Routine (T, SampleTest'Access, "SampleTest");
      Register_Routine (T, SampleRead'Access, "SampleRead");
      Register_Routine (T, SampleRead2'Access, "SampleRead2");
      Register_Routine (T, ReadProperties'Access, "ReadProperties");
   end Register_Tests;

end Ssprep.ConfigParsers.Testcases.Testbasic;
