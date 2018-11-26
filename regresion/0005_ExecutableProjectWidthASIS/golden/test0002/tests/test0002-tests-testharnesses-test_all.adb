-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------
with Ada.Command_Line;
with AUnit.Run;
--  with AUnit.Reporter.Text;
with AUnit.Reporter.XML;

with test0002.Tests.TestSuits.Test_All;

-------------------------------------------
-- test0002.Tests.TestHarnesses.Test_All --
-------------------------------------------

procedure test0002.Tests.TestHarnesses.Test_All is

   use AUnit;
   use Ada.Command_Line;
   function Run is new AUnit.Run.Test_Runner_With_Status (test0002.Tests.TestSuits.Test_All.Suite);

   --  Reporter : AUnit.Reporter.Text.Text_Reporter;
   Reporter : AUnit.Reporter.XML.XML_Reporter;

begin
   if Run (Reporter) = AUnit.Failure then
      Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end test0002.Tests.TestHarnesses.Test_All;