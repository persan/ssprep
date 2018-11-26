
with GNAT.Source_Info;
with Ssprep.Translators; use Ssprep.Translators;
with Ssprep.Templates;
with Templates_Parser; use Templates_Parser;
with AUnit.Assertions; use AUnit.Assertions;
package body Ssprep.Tests.Testcases.Test_Transalte_File is
   use AUnit;
   use AUnit.Test_Cases.Registration;
   procedure Set_Up_Case (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Set_Up_Case;

   -------------------------
   --  test_1
   -------------------------
   procedure Test_1 (Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_1 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Map : Templates_Parser.Translate_Set;
   begin
      Set_Symbol (Map, "S1", "TEST");
      declare
         Actual   : constant String := Translate_String (Map, "@_S1_@");
         Expected : constant String := "TEST";
      begin
         Assert (Actual = Expected,
                 "Expected : " & Expected &
                   " Got  : " &  Actual);
      end;
   end Test_1;


   -------------------------
   --  test_2
   -------------------------
   procedure Test_2 (Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_2 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T   : Test_Case renames Test_Case (Test);
      pragma Unreferenced (T);
      Map : Translate_Set;
   begin
      Set_Symbol (Map, "ROOT", "Test.Apa");
      Set_Symbol (Map, "ROOT_FILE", "test-apa");
   end Test_2;

   -------------------------
   --  test_3
   -------------------------
   procedure Test_3 (Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_3 (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      pragma Unreferenced (Test);
      Map : Translate_Set;
      Tr  : Translators.Translator;
      Tp  : Templates.Templates;
   begin
      Set_Symbol (Map, "project", "Ture_Proj", Define_File => True);
      Set_Symbol (Map, "root", "Ture", Define_File => True);
      Set_Symbol (Map, "project_dir", "ture");
      Write_Symbols (Map, "symb.xml");
      Tp.Add_Directory (Templates.Get_System_Templates_Dir);
      Tr.Set_Verbose (False);
      Tp.Set_Quiet (True);
      declare
         Tmplt : aliased Templates.Template'Class := Tp.Get ("ssprep.simpleExecutableProject");
      begin
         Assert (Tmplt.Is_Valid, "No template for SimpleProject found");
         Tr.Translate (Tmplt'Unchecked_Access,
                       Map,
                       "output");
      end;
   end Test_3;

   function Name (T : Test_Case)
                  return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return Format (GNAT.Source_Info.File);
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_1'Access, "Test_1");
      Register_Routine (T, Test_2'Access, "Test_2");
      Register_Routine (T, Test_3'Access, "Test_3");
   end Register_Tests;

end Ssprep.Tests.Testcases.Test_Transalte_File;
