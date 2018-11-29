-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------

--  This package contains routines for high-level processing of
--  (terating through) an ASIS Context

with Asis;

package test0002.Context_Processing is

   procedure Process_Context
     (The_Context : Asis.Context;
      Trace       : Boolean := False);
   --  This procedure iterates through the whole content of its argument
   --  Context and it calls a unit processing routine for those ASIS
   --  Compilation Units which are of An_Application_Unit origin (that is,
   --  user-defined units). If Trace parameter is set ON, it generate the
   --  simple trace of the unit processing (consisting of the names of the
   --  units in the Context being processed or skipped).

   function Get_Unit_From_File_Name
     (Ada_File_Name : String;
      The_Context   : Asis.Context)
      return          Asis.Compilation_Unit;
   --  Supposing that Ada_File_Name is the name of an Ada source file which
   --  follows the GNAT file naming rules (see the GNAT Users Guide), this
   --  function tries to get from The_Context the ASIS Compilation Unit
   --  contained in this source file. The source file name may contain the
   --  directory information in relative or absolute form.
   --
   --  If The_Context does not contain the ASIS Compilation Unit which
   --  may be the content of the argument file, Nil_Compilation_Unit is
   --  returned.
   --
   --  Note, that this function always return Nil_Compilation_Unit, if
   --  Ada_File_Name is a file name which is krunched. Nil_Compilation_Unit
   --  is also returned if Ada_File_Name correspond to any name of a child
   --  unit from the predefined or GNAT-specific hierarchy (children of
   --  System, Ada, Interfaces, and GNAT)

end test0002.Context_Processing;