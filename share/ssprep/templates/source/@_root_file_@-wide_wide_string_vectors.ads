@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Containers.Indefinite_Vectors;
package @_root_@.Wide_Wide_String_Vectors is new
Ada.Containers.Indefinite_Vectors
  (Index_Type   => Positive,
   Element_Type => Wide_Wide_String);
