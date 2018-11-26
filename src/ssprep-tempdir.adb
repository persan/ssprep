with GNAT.OS_Lib; use GNAT.OS_Lib;

function Ssprep.TempDir return String is
   TempDir  : String_Access := Getenv ("TEMP");
begin
   if TempDir.all'Length > 2 then
      if Is_Directory (TempDir.all) then
         declare
            ret : constant String := TempDir.all;
         begin
            Free (TempDir);
            return ret;
         end;
      end if;
      Free (TempDir);
   end if;
   Free (TempDir);

   if Is_Directory ("/tmp") then
      return "/tmp";
   elsif Is_Directory ("C:/temp") then
      return "C:/temp";
   end if;

   raise Program_Error with "no Temp folder found";

end Ssprep.TempDir;
