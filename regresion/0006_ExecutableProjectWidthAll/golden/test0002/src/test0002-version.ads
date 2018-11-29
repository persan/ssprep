-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------
package test0002.Version  is
   Major_Version : constant String := "0";
   Minor_Version : constant String := "0";
   Patch_Version : constant String := "1";
   Date          : constant String := "20181128";
   Version       : constant String := Major_Version & "." & Minor_Version & "." & Patch_Version;
   Full_Version  : constant String := Version & " (" & Date & ")";
end test0002.Version;
