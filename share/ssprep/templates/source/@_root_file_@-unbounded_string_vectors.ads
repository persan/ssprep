@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
package @_root_@.Unbounded_String_Vectors is
new Ada.Containers.Indefinite_Vectors
  (Index_Type   => Positive,
   Element_Type => Unbounded_String);
