@@INCLUDE@@ ../../../Common/FileHeader.txt

with Asis.Iterator;

with @_root_@.Actuals_For_Traversing;

package body @_root_@.Element_Processing is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Recursive_Construct_Processing is new
      Asis.Iterator.Traverse_Element
        (State_Information => Actuals_For_Traversing.Traversal_State,
         Pre_Operation     => Actuals_For_Traversing.Pre_Op,
         Post_Operation    => Actuals_For_Traversing.Post_Op);
   --  This instantiation of Traverse_Element actually performs the recursive
   --  analysis of ASIS Elements. In this set of ASIS application templates
   --  it does nothing, but you may get a number of simple, but useful ASIS
   --  applications by providing some real code for Pre_Op and/or Post_Op,
   --  and keeping the rest of the template code unchanged. For example,
   --  see ASIS tutorials included in the ASIS distribution.

   -----------------------
   -- Process_Construct --
   -----------------------

   procedure Process_Construct (The_Element : Asis.Element) is
      Process_Control : Asis.Traverse_Control := Asis.Continue;

      Process_State   : Actuals_For_Traversing.Traversal_State :=
         Actuals_For_Traversing.Initial_Traversal_State;

   begin

      Recursive_Construct_Processing
        (Element => The_Element,
         Control => Process_Control,
         State   => Process_State);

   end Process_Construct;

end @_root_@.Element_Processing;
