@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Containers.Indefinite_Ordered_Sets;
package @_root_@.Wide_String_Ordered_Sets is new
Ada.Containers.Indefinite_Ordered_Sets
  (Element_Type => Wide_String);
