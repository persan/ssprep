with GNAT.Directory_Operations;
with GNAT.OS_Lib;
package body Ssprep.Savepoints is
   use GNAT.Strings;
   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Directory_Saver) is
   begin
      Object.Dir := new String'(GNAT.Directory_Operations.Get_Current_Dir);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Directory_Saver) is
   begin
      GNAT.Directory_Operations.Change_Dir (Object.Dir.all);
      Free (Object.Dir);
   end Finalize;




   procedure Initialize (Object : in out Environment_Saver) is
   begin
      Object.Value := GNAT.OS_Lib.Getenv (Object.Name.all);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Environment_Saver) is
      Temp : GNAT.Strings.String_Access := Object.Name;
   begin
      GNAT.OS_Lib.Setenv (Object.Name.all, Object.Value.all);
      Free (Object.Value);
      Free (Temp);
   end Finalize;


end Ssprep.Savepoints;
