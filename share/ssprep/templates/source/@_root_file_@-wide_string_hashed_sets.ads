@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Wide_Hash;
package @_root_@.Wide_String_Hashed_Sets is new
Ada.Containers.Indefinite_Hashed_Sets
  (Element_Type        => Wide_String,
   Hash                => Ada.Strings.Wide_Hash,
   Equivalent_Elements => "=");
