with Ada.Finalization;
with GNAT.Strings;
package Ssprep.Savepoints is



   type Directory_Saver is tagged limited private;
   pragma Unreferenced_Objects (Directory_Saver);
   --  Saves the current directory and restores it when the
   --  object goes out of scope.

   type Environment_Saver (Name : GNAT.Strings.String_Access) is tagged limited private;
   pragma Unreferenced_Objects (Environment_Saver);
   --  Saves the the Value of the Environment variable and restores it
   --  it when the object goes out of scope all memory is freed when the object
   --  goes out of scope.



private
   type Directory_Saver is new Ada.Finalization.Limited_Controlled with record
      Dir : GNAT.Strings.String_Access;
   end record;
   procedure Initialize (Object : in out Directory_Saver);
   procedure Finalize   (Object : in out Directory_Saver);


   type Environment_Saver (Name : GNAT.Strings.String_Access) is new Ada.Finalization.Limited_Controlled with record
      Value : GNAT.Strings.String_Access;
   end record;
   procedure Initialize (Object : in out Environment_Saver);
   procedure Finalize   (Object : in out Environment_Saver);


end Ssprep.Savepoints;
