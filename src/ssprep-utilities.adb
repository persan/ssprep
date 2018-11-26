with GNAT.Expect;
with GNAT.OS_Lib;
with GNAT.String_Split;
with Ada.Strings.Fixed;
with Ada.Directories;
package body Ssprep.Utilities is

   --------------------
   -- Get_Process_Id --
   --------------------

   function Get_Process_Id return String is
      I : constant String := Integer'Image (Get_Process_Id);
   begin
      return I (I'First + 1 .. I'Last);
   end Get_Process_Id;

   function Full_Pathname (s : String) return String is
      Ret : constant String := Ada.Strings.Fixed.Trim (s, Ada.Strings.Both);
   begin
      if  Ret = "<Current_Directory>" then
         return ".";
      else
         return Ret;
      end if;
   end Full_Pathname;

   function get_Ada_project_Path return Ssprep.String_Vectors.Vector is
      use GNAT.OS_Lib;
      use GNAT.Expect;
      use GNAT.String_Split;
      Status        : aliased Integer;
      res           : GNAT.String_Split.Slice_Set;
      args          : Argument_List_Access := new Argument_List'(1 => new String'("-v"));
      Key           : constant String := "Project Search Path:";
      inProjectPath : Boolean := False;
      ret           : Ssprep.String_Vectors.Vector;
   begin
      Create
        (res,
         Get_Command_Output ("gnatls",
           Arguments    => args.all,
           Input        => "",
           Status       => Status'Access), ASCII.LF & ASCII.CR, Multiple);

      for I in 1 .. Slice_Count (res) loop
         declare
            path : constant String := Full_Pathname (Slice (res, I));
         begin
            if path'Length > 0 then
               if path = Key then
                  inProjectPath := True;
               elsif inProjectPath and then Ada.Directories.Exists (path) then
                  ret.Append (path);
               end if;
            end if;
         end;
      end loop;
      Free (args);
      return ret;
   end get_Ada_project_Path;


   function First_Line (item : String) return String is
      s : GNAT.String_Split.Slice_Set;
      use GNAT;
      use type String_Split.Slice_Number;
   begin
      String_Split.Create (s, item, ASCII.LF & ASCII.CR);
      if String_Split.Slice_Count (s) > 0 then
         return String_Split.Slice (s, 1);
      else
         return item;
      end if;
   end First_Line;

end Ssprep.Utilities;
