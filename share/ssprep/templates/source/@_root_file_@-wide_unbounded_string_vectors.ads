@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Containers.Indefinite_Vectors;
package @_root_@.Wide_Unbounded_String_Vectors is
new Ada.Containers.Indefinite_Vectors
  (Index_Type   => Positive,
   Element_Type => Unbounded_Wide_String);
