with Ada.Containers.Vectors;
with Ssprep.String_Vectors;
package Ssprep.ssfetch.projects is

   type Finder_Interface is interface;
   type Any_Finder_Interface is access all Finder_Interface'Class;
   function Find (this : Finder_Interface; proj : String) return String is abstract;
   procedure Add (this : in out Finder_Interface; path : String) is null;


   type file_finder is new Finder_Interface with private;
   function Find (this : file_finder; proj : String) return String;
   procedure Add (this : in out file_finder; path : String);

   type finders is tagged private;
   procedure register (this : in out finders; f : Any_Finder_Interface);
   function find (this : finders; proj : String) return String;

private
   type file_finder is new Finder_Interface with record
      files : Ssprep.String_Vectors.Vector;
   end record;

   package finder_vectors is new Ada.Containers.Vectors (Positive, Any_Finder_Interface);
   type finders is tagged record
      f : finder_vectors.Vector;
   end record;

end Ssprep.ssfetch.projects;
