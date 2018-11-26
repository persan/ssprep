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

with hej.TestSuits.Test_All;

--------------------------------
-- hej.TestHarnesses.Test_All --
--------------------------------

procedure hej.TestHarnesses.Test_All is

   use AUnit;
   use Ada.Command_Line;
   function Run is new AUnit.Run.Test_Runner_With_Status (hej.TestSuits.Test_All.Suite);

   --  Reporter : AUnit.Reporter.Text.Text_Reporter;
   Reporter : AUnit.Reporter.XML.XML_Reporter;

begin
   if Run (Reporter) = AUnit.Failure then
      Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end hej.TestHarnesses.Test_All;