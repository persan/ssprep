pragma Warnings (Off);
with AUnit.Run;
with AUnit.Reporter.XML;
with AUnit.Reporter.Empty;
with Ssprep.Tests.Testsuits.Test_All;
-----------------------------------------
-- Ssprep.Tests.Testharnesses.Test_All --
-----------------------------------------
with Ada.Directories;
with Ada.Command_Line;
with Ssprep.Tests.Utilities;
with Ada.Text_IO;

procedure Ssprep.Tests.Testharnesses.Test_All is
   function Run is new AUnit.Run.Test_Runner_With_Status (Ssprep.Tests.Testsuits.Test_All.Suite);
   Reporter : AUnit.Reporter.XML.XML_Reporter;
   --  Reporter : AUnit.Reporter.Empty.Empty_Reporter;

begin
   --  Ada.Text_IO.Put_Line (Utilities.Get_Test_Dir);
   Ada.Directories.Set_Directory (Utilities.Get_Test_Dir);
   case Run (Reporter) is
   when AUnit.Success =>
      null;
   when AUnit.Failure =>
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end case;
end Ssprep.Tests.Testharnesses.Test_All;
