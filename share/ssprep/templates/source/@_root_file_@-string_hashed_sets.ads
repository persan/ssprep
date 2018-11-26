@@INCLUDE@@ ../Common/FileHeader.txt @_root_@
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;
package @_root_@.String_Hashed_Sets is new
Ada.Containers.Indefinite_Hashed_Sets
  (Element_Type        => String,
   Hash                => Ada.Strings.Hash,
   Equivalent_Elements => "=");
