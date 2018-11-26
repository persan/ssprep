-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------

with GNAT.Strings;
with Ssprep.String_Vectors;
package Ssprep.getBuildOrder is



   --  Sorts the brojects in build order

   type DumpFormatType is (Format_Verbose,
                           Format_Proj,
                           Format_Proj_Name,
                           Format_Dir);


   Ada_Project_Path   : GNAT.Strings.String_Access;
   DumpFormat         : DumpFormatType := Format_Dir;
   Verbosity_Level    : Integer := 0;
   Ignored_Projects   : String_Vectors.Vector;
end  Ssprep.getBuildOrder;
