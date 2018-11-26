@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Containers.Ordered_Sets;
package @_root_@.Wide_Unbounded_String_Ordered_Sets is
new Ada.Containers.Ordered_Sets
  (Element_Type => Unbounded_Wide_String);
