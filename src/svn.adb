with GNAT.OS_Lib;
package body svn is

   --------
   -- ls --
   --------

   function ls (path : String) return Ssprep.String_Vectors.Vector is
      args : GNAT.OS_Lib.Argument_List_Access;
      Success : Boolean;
      Return_Code     : Integer;
   begin
      return ret : Ssprep.String_Vectors.Vector do
         args := new GNAT.OS_Lib.Argument_List'
           (new String'("ls"),
            new String'(path));
         GNAT.OS_Lib.Spawn ("svn", args.all, "temp.file", Success, Return_Code, Err_To_Out => True);
         GNAT.OS_Lib.Free (args);
         if not Success then
            raise Program_Error with "ls (""" & path & """)" & Return_Code'Img;
         end if;
      end return;
   end ls;

   --------
   -- co --
   --------

   procedure co (src  : String; tgt : String) is
      args : GNAT.OS_Lib.Argument_List_Access;
      Success : Boolean;
   begin
      args := new GNAT.OS_Lib.Argument_List'
        (new String'("co"),
         new String'(src),
         new String'(tgt));
      GNAT.OS_Lib.Spawn ("svn", args.all, Success);
      GNAT.OS_Lib.Free (args);
      if not Success then
         raise Program_Error with "co(""" & src & """, """ & tgt & """)";
      end if;
   end co;

   ---------
   -- url --
   ---------

   function url (path : String) return String is
      args : GNAT.OS_Lib.Argument_List_Access;
      Success : Boolean;
   begin
      args := new GNAT.OS_Lib.Argument_List'
        (new String'("info"),
         new String'("--xml"),
         new String'(path));
      GNAT.OS_Lib.Spawn ("svn", args.all, Success);
      GNAT.OS_Lib.Free (args);
      if not Success then
         raise Program_Error with "url (""" & path & """)";
      end if;
      return "Dummy";
   end url;

end svn;
