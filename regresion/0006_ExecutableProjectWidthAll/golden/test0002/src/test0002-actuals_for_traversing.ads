-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------

--  This package contains the definition of the actual types and subprograms
--  to be used to instantiate Asis.Iterator.Traverse_Element.
--  Actually, the declarations given in this package are templates in the
--  sense, that, being formally correct and making the whole code of the
--  template ASIS applications compilable, they do nothing and therefore they
--  have to be replaced in the real ASIS applications.
--
--  The bodies of the subprograms declared in this package are implemented
--  as subunits to localize changes needed to provide real Pre- and
--  Post-operations in real operations

with Asis;

package test0002.Actuals_For_Traversing is

   type Traversal_State is (Not_Used);
   --  A placeholder declaration, used as an actual for State_Information in
   --  the template instantiation of Traverse_Element.
   --  If your ASIS application needs some non-trivial state, you should
   --  either change the definition of this type (and of the constant
   --  Initial_Traversal_State) below or use some other type as the actual
   --  for State_Information when instantiating Traverse_Element.

   Initial_Traversal_State : constant Traversal_State := Not_Used;

   procedure Pre_Op
     (Element :        Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State);
   --  This is the template for the actual Pre-Operation procedure. It does
   --  nothing, and it contains the exception handler which is supposed to
   --  catch all the exception raised in this procedure.
   --  The body of this procedure is implemented as a subunit - in case when
   --  you  would like to provide your own Pre-operation when building your
   --  ASIS tool from the given set of templates (and if you do not need
   --  non-trivial traversal state), you can replace this subunit by your own
   --  code and reuse the rest of the template code.

   procedure Post_Op
     (Element :        Asis.Element;
      Control : in out Asis.Traverse_Control;
      State   : in out Traversal_State);
   --  This is the template for the actual Post-Operation procedure. It does
   --  nothing, and it contains the exception handler which is supposed to
   --  catch all the exception raised in this procedure.
   --  The body of this procedure is implemented as a subunit - in case when
   --  you  would like to provide your own Post-operation when building your
   --  ASIS tool from the given set of templates (and if you do not need
   --  non-trivial traversal state), you can replace this subunit by your own
   --  code and reuse the rest of the template code.

end test0002.Actuals_For_Traversing;