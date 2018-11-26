@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
package @_root_@.Unbounded_String_Ordered_Sets is
new Ada.Containers.Ordered_Sets
  (Element_Type => Unbounded_String);
