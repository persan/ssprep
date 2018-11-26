@@INCLUDE@@ ../Common/FileHeader.txt @_root_@

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
package @_root_@.String_Hashed_Maps is new
Ada.Containers.Indefinite_Hashed_Maps
  (Key_Type        => String,
   Element_Type    => String,
   Hash            => Ada.Strings.Hash,
   Equivalent_Keys => "=");
