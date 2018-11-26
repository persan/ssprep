@@INCLUDE@@ ../../../Common/FileHeader.txt

package body @_root_@.Actuals_For_Traversing is

   -------------
   -- Post_Op --
   -------------

   procedure Post_Op
     (Element :        Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State)
   is separate;

   ------------
   -- Pre_Op --
   ------------

   procedure Pre_Op
     (Element :        Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State)
   is separate;

end @_root_@.Actuals_For_Traversing;
