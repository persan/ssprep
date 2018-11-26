with GNAT.Strings; use GNAT.Strings;
with Ada.Text_IO;
with Ada.Directories;
with GNAT.Regpat;
package body Ssprep.ssfetch.projects is

   ----------
   -- Find --
   ----------


   function Find (this : file_finder; proj : String) return String is
      ret : GNAT.Strings.String_Access;
      matcher : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("^([a-zA-Z0-9_/]+) +([a-zA-Z0-9_/]+)");
      matches : GNAT.Regpat.Match_Array (0 .. GNAT.Regpat.Paren_Count (matcher));
      procedure parse (s : String) is
         use GNAT.Regpat;
      begin
         Match (matcher, s, matches);
         if matches (0) /= No_Match then
            if s (matches (1).First .. matches (1).Last) = proj then
               ret := new String'(s (matches (2).First .. matches (2).Last));
            end if;
         end if;
      end parse;

      procedure Process (Position : String_Vectors.Cursor) is
         path : constant String := String_Vectors.Element  (Position);

      begin
         if ret = null then
            if Ada.Directories.Exists (path) then
               declare
                  F : Ada.Text_IO.File_Type;
               begin
                  Ada.Text_IO.Open (F, Ada.Text_IO.In_File, path);
                  while not Ada.Text_IO.End_Of_File (F) loop
                     parse (Ada.Text_IO.Get_Line (F));
                     if ret /= null then
                        exit;
                     end if;
                  end loop;
                  Ada.Text_IO.Close (F);
               end;
            end if;
         end if;
      end Process;

   begin
      this.files.Iterate (Process'Access);
      if ret = null then
         return "";
      else
         declare
            r : constant String := ret.all;
         begin
            Free (ret);
            return r;
         end;
      end if;
   end Find;

   ---------
   -- Add --
   ---------

   procedure Add (this : in out file_finder; path : String) is
   begin
      this.files.Append (path);
   end Add;

   --------------
   -- register --
   --------------

   procedure register (this : in out finders; f : Any_Finder_Interface) is
   begin
      this.f.Append (f);
   end register;

   ----------
   -- find --
   ----------

   function find (this : finders; proj : String) return String is
      ret : GNAT.Strings.String_Access;
      procedure Process (Position : finder_vectors.Cursor) is
         t : constant String := finder_vectors.Element  (Position).Find (proj);
      begin
         if ret = null then
            if t'Length > 0 then
               ret := new String'(t);
            end if;
         end if;
      end Process;

   begin
      this.f.Iterate (Process'Access);
      if ret = null then
         Put_Line (1, "- """ & proj & """ Not found");
         return "";
      else
         declare
            r : constant String := ret.all;
         begin
            Free (ret);
            Put_Line (1, "+ """ & proj & """ found");
            return r;
         end;
      end if;
   end find;

end Ssprep.ssfetch.projects;
