@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;
with Ada.Strings.Wide_Unbounded.Wide_Hash;
with Ada.Containers.Hashed_Sets;
package @_root_@.Wide_Unbounded_String_Hashed_Sets is
new Ada.Containers.Hashed_Sets
  (Element_Type        => Unbounded_Wide_String,
   Hash                => Wide_Hash,
   Equivalent_Elements => "=");
