@@INCLUDE@@ ../Common/FileHeader.txt @_HARNESS_NAME_@ @_harness_name_file_@
with Ada.Command_Line;
with AUnit.Run;
--  with AUnit.Reporter.Text;
with AUnit.Reporter.XML;

with @_HARNESS_TEST_SUITE_PACKAGE_@;

---@_REPLACE_ALL(./-):HARNESS_NAME_@---
-- @_HARNESS_NAME_@ --
---@_REPLACE_ALL(./-):HARNESS_NAME_@---

procedure @_HARNESS_NAME_@ is

   use AUnit;
   use Ada.Command_Line;
   
   function Run is new AUnit.Run.Test_Runner_With_Status (@_HARNESS_TEST_SUITE_PACKAGE_@.@_HARNESS_TEST_SUITE_@);

   --  Reporter : AUnit.Reporter.Text.Text_Reporter;
   Reporter : AUnit.Reporter.XML.XML_Reporter;

begin
   if Run (Reporter) = AUnit.Failure then
      Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end @_HARNESS_NAME_@;
