@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Wide_Hash;
package @_root_@.Wide_String_Hashed_Maps is new
Ada.Containers.Indefinite_Hashed_Maps
  (Key_Type        => Wide_String,
   Element_Type    => Wide_String,
   Hash            => Ada.Strings.Wide_Hash,
   Equivalent_Keys => "=");
