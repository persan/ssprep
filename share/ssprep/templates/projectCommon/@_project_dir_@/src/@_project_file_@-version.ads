@@INCLUDE@@ ../../../Common/FileHeader.txt  @_project_@
package @_project_@.Version  is
   Major_Version : constant String := "0";
   Minor_Version : constant String := "0";
   Patch_Version : constant String := "1";
   Date          : constant String := "@_YEAR_@@_MONTH_@@_DAY_@";
   Version       : constant String := Major_Version & "." & Minor_Version & "." & Patch_Version;
   Full_Version  : constant String := Version & " (" & Date & ")";
end @_project_@.Version;

