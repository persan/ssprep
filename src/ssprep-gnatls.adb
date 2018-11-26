with GNAT.Expect;
with GNAT.Strings;
with GNAT.OS_Lib;
with Ada.Strings.Fixed;
with GNAT.String_Split;
with Ada.Strings.Unbounded;
package body Ssprep.GNATls is
   use Ada.Strings;
   use GNAT;
   ----------------------
   -- ADA_PROJECT_PATH --
   ----------------------

   function ADA_PROJECT_PATH return Ssprep.String_Vectors.Vector is
      Args               : Strings.String_List_Access := new GNAT.Strings.String_List'(1 => new String'("-v"));
      Ret                : String_Vectors.Vector;
      Status             : aliased Integer;
      Reply              : GNAT.String_Split.Slice_Set;
      Key_Section        : constant String := "Project Search Path:";
      Key_CurrentDir     : constant String := "<Current_Directory>";
      In_Project_Section : Boolean := False;
      procedure Try_Add (S : String) is
      begin
         if In_Project_Section and then S'Length > 0 then
            if S = Key_CurrentDir then
               Ret.Append  (".");
            else
               Ret.Append (S);
            end if;
         end if;
         if S = Key_Section then
            In_Project_Section := True;
         end if;
      end Try_Add;
   begin
      GNAT.String_Split.Create
        (Reply,
         GNAT.Expect.Get_Command_Output ("gnatls", Args.all, "", Status'Access, True),
         "" & ASCII.LF & ASCII.CR);
      GNAT.Strings.Free (Args);
      if not ((Status = 4)  or (Status = 0)) then
         GNAT.OS_Lib.Set_Errno (Status);
         raise Program_Error with "gnatls -l failed with " & Status'Img;
      end if;
      for I in 1 .. GNAT.String_Split.Slice_Count (Reply) loop
         Try_Add (Fixed.Trim (GNAT.String_Split.Slice (Reply, I), Both));
      end loop;
      return Ret;
   end ADA_PROJECT_PATH;


   function To_Path (Item : String) return Ssprep.String_Vectors.Vector is
      T   : String_Split.Slice_Set;
      Ret : String_Vectors.Vector;
   begin
      String_Split.Create (T, Item, GNAT.OS_Lib.Path_Separator & "");
      for I in  1 .. String_Split.Slice_Count (T) loop
         Ret.Append  (String_Split.Slice (T, I));
      end loop;
      return Ret;
   end To_Path;

   function ToString (Value : Ssprep.String_Vectors.Vector) return String is

      Ret : Ada.Strings.Unbounded.Unbounded_String;
      Cursor : String_Vectors.Cursor;
      use Ada.Strings.Unbounded;
      use String_Vectors;
   begin
      Cursor := Value.First;
      if String_Vectors.Has_Element (Cursor) then
         loop
            Append (Ret, String_Vectors.Element (Cursor));
            exit when Cursor = Value.Last;
            Append (Ret, GNAT.OS_Lib.Path_Separator);
            Cursor := String_Vectors.Next (Cursor);
         end loop;
      end if;
      return Ada.Strings.Unbounded.To_String (Ret);
   end ToString;


end Ssprep.GNATls;
