@@INCLUDE@@ ../Common/FileHeader.txt @_root_@

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;

package @_root_@.Wide_Wide_Unbounded_String_Hashed_Maps is new
Ada.Containers.Indefinite_Hashed_Maps
  (Key_Type        => Unbounded_Wide_Wide_String,
   Element_Type    => Unbounded_Wide_Wide_String,
   Hash            => Wide_Wide_Hash,
   Equivalent_Keys => "=");
