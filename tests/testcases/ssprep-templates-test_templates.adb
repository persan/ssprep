with GNAT.Source_Info;
with Ssprep.Tests;
package body Ssprep.Templates.Test_Templates is
   use AUnit;
   use AUnit.Test_Cases.Registration;
   procedure Set_Up_Case (Test : in out Test_Case) is
      pragma Unreferenced (Test);
   begin
      null;
   end Set_Up_Case;

   -------------------------
   --  test_Get_System_Dir
   -------------------------
   procedure Test_Get_System_Dir (Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_System_Dir (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T    : Test_Case renames Test_Case (Test);
      pragma Unreferenced (T);
      --        Temp : constant String := Get_System_Dir;
      --        pragma Unreferenced (Temp);
   begin
      null;
   end Test_Get_System_Dir;

   -------------------------
   --  test_Template_Name
   -------------------------
   procedure Test_Get_Full_Path (Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Full_Path (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T    : Test_Case renames Test_Case (Test);
      pragma Unreferenced (T);
      --        Temp : constant String := Get_Full_Path ("dd");
      --        pragma Unreferenced (Temp);
   begin
      null;
   end Test_Get_Full_Path;

   -------------------------
   --  test_get_templates
   -------------------------
   procedure Test_Get_Templates (Test : in out AUnit.Test_Cases.Test_Case'Class);
   procedure Test_Get_Templates (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      T  : Test_Case renames Test_Case (Test);
      pragma Unreferenced (T);
      --  Tp : Templates;
   begin
      --        Tp.Add_Path (Get_System_Dir);
      --        Tp.Dump (Ada.Text_IO.Put_Line'Access);
      null;
   end Test_Get_Templates;
   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return Message_String is
      pragma Unreferenced (T);
   begin
      return Format (GNAT.Source_Info.File);
   end Name;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
   begin
      Register_Routine (T, Test_Get_System_Dir'Access, "Test_Get_System_Dir");
      Register_Routine (T, Test_Get_Full_Path'Access, "Test_Get_Full_Path");
      Register_Routine (T, Test_Get_Templates'Access, "Test_Get_Templates");
   end Register_Tests;

end Ssprep.Templates.Test_Templates;
