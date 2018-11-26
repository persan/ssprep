-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------

with Asis.Elements;

with test0002.Element_Processing;

package body test0002.Unit_Processing is

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (The_Unit : Asis.Compilation_Unit) is
      Cont_Clause_Elements : constant Asis.Element_List :=
         Asis.Elements.Context_Clause_Elements (Compilation_Unit => The_Unit,
                                                Include_Pragmas  => True);
      --  This is the list of the context clauses, including pragmas, if any.
      --  If you do not want to process pragmas, set Include_Pragmas OFF when
      --  calling Asis.Elements.Context_Clause_Elements

      Unit_Decl : Asis.Element := Asis.Elements.Unit_Declaration (The_Unit);
      --  The top-level ctructural element of the library item or subunit
      --  contained in The_Unit.

   begin

      for J in Cont_Clause_Elements'Range loop
         Element_Processing.Process_Construct (Cont_Clause_Elements (J));
      end loop;
      --  Many applications are not interested in processing the context
      --  clause of the compilation units. If this is the case for your
      --  application, simply remove this loop statement.

      Element_Processing.Process_Construct (Unit_Decl);

      --  This procedure does not contain any exception handler because it
      --  supposes that Element_Processing.Process_Construct should handle
      --  all the exceptions which can be raised when processing the element
      --  hierarchy

   end Process_Unit;

end test0002.Unit_Processing;