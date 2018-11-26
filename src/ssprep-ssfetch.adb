with Ada.Text_IO;
package body Ssprep.ssfetch is

   --------------
   -- put_line --
   --------------

   procedure Put_Line (Verbosity : Natural; Message : String) is
   begin
      if Verbosity >= Ssprep.ssfetch.Verbosity then
         Ada.Text_IO.Put_Line (Message);
      end if;
   end Put_Line;

end Ssprep.ssfetch;
