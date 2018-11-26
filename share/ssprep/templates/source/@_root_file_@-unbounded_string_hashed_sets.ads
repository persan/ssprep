@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Sets;
package @_root_@.Unbounded_String_Hashed_Sets is
new Ada.Containers.Hashed_Sets
  (Element_Type        => Unbounded_String,
   Hash                => Hash,
   Equivalent_Elements => "=");
