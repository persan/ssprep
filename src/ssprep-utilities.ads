with Ssprep.String_Vectors;
package Ssprep.Utilities is

   function Get_Process_Id return Integer;
   pragma Import (C, Get_Process_Id, "getpid");
   function Get_Process_Id return String;
   --  Return the Pid of curent process

   function get_Ada_project_Path return String_Vectors.Vector;
   --  Returns the Ada_project_Path as a Vector

   function First_Line (item : String) return String;
   --  Return the first Line in a multiline string

end Ssprep.Utilities;
