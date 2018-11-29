-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------

--  This package contains routines for ASIS Compilation Units processing.

with Asis;

package test0002.Unit_Processing is

   procedure Process_Unit (The_Unit : Asis.Compilation_Unit);
   --  This procedure decomposes its argument unit and calls the element
   --  processing routine for all the top-level components of the unit
   --  element hierarchy. This element processing routine is the instance
   --  of Traverse_Element which performs the recursive traversing of
   --  the unit structural components. In the given set of the ASIS
   --  implementation templates, the element processing routine is used
   --  "empty" actual Pre- and Post-Operations to instantiate
   --  Traverse_Element, they should be replaced by real subprograms
   --  when using this set of templates to build a real ASIS application
   --  from it.
   --
   --  It the argument Unit is Nil or nonexistent unit, this procedure does
   --  nothing.

end test0002.Unit_Processing;