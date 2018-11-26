@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
package @_root_@.Unbounded_String_Hashed_Maps is new
Ada.Containers.Indefinite_Hashed_Maps
  (Key_Type        => Unbounded_String,
   Element_Type    => Unbounded_String,
   Hash            => Hash,
   Equivalent_Keys => "=");
