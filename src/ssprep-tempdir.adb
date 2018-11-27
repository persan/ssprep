with Ada.Environment_Variables;
with Ada.Directories;
function Ssprep.TempDir return String is
   use Ada;
begin

   if Environment_Variables.Exists ("TEMP") then
      return Environment_Variables.Value ("TEMP");

   elsif Environment_Variables.Exists ("TMP") then
      return Environment_Variables.Value ("TMP");

   elsif Directories.Exists ("/tmp") then
      return "/tmp";

   elsif Directories.Exists ("C:/temp") then
      return "C:/temp";

   end if;

   raise Program_Error with "no Temp folder found";

end Ssprep.TempDir;
