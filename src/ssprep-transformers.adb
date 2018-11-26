--  ---------------------------------------------------------------------------
--  Copyright 2008 Per Sandberg  <per.sandberg@bredband.net>
--  ---------------------------------------------------------------------------

--  $Id$

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--  ---------------------------------------------------------------------------
with Ada.Strings.Fixed;
with GNAT.String_Split;
with GNAT.Directory_Operations;
with Ada.Containers;
with Ada.Strings.Maps;
package body Ssprep.Transformers is
   use Ada.Strings;
   use Ada.Containers;
   function Is_Spec (S : String) return Boolean is
   begin
      return (S'Length > 4  and then S (S'Last - 3 .. S'Last) = ".ads") or
        (S'Length > 5  and then S (S'Last - 5 .. S'Last) = ".1.ada");
   end Is_Spec;

   function Is_Impl (S : String) return Boolean is
   begin
      return (S'Length > 4  and then S (S'Last - 3 .. S'Last) = ".adb") or
        (S'Length > 5  and then S (S'Last - 5 .. S'Last) = ".2.ada");
   end Is_Impl;

   function File2ada (S : String) return String is
      Ext  : constant String := GNAT.Directory_Operations.File_Extension (S);
      Last : Positive := S'Last;
   begin
      if Ext = ".ada" then
         Last := S'Last - 6;
      else
         Last := S'Last - 4;
      end if;
      return Fixed.Translate (S (S'First .. Last), Maps.To_Mapping ("-", "."));
   end File2ada;


   procedure Add_Ignore (This : in out Transformer; F : in String) is
   begin
      This.Ignore_Files.Include (F);
   end Add_Ignore;



   procedure Dump (This : in out Transformer) is


      procedure Print (C : String_Vectors.Cursor) is
      begin
         Ada.Text_IO.Put_Line (" " & String_Vectors.Element (C));
      end Print;
   begin
      Ada.Text_IO.Put_Line ("Full_List");
      This.Full_List.Iterate (Print'Access);

      Ada.Text_IO.Put_Line ("Src_Dirs");
      This.Source_Dirs.Iterate (Print'Access);

      Ada.Text_IO.Put_Line ("Impl");
      This.Impl.Iterate (Print'Access);

      Ada.Text_IO.Put_Line ("Impl_Files");
      This.Impl_Files.Iterate (Print'Access);

      Ada.Text_IO.Put_Line ("Spec");
      This.Impl.Iterate (Print'Access);

      Ada.Text_IO.Put_Line ("Spec_Files");
      This.Impl_Files.Iterate (Print'Access);

   end Dump;

   procedure Parse (This : in out Transformer; Src : in  String) is
      use GNAT.String_Split;
      Spl : Slice_Set;
   begin
      Create (Spl, Src, " " & ASCII.LF & ASCII.CR & ASCII.HT & ASCII.VT, Multiple);
      for I in  1 .. Slice_Count (Spl) loop
         declare
            S : constant String := Slice (Spl, I);
         begin
            if Is_Spec (S) or Is_Impl (S) then
               This.Full_List.Append (S);
            end if;
         end;
      end loop;
   end Parse;

   ----------
   -- Read --
   ----------



   procedure Read
     (This : in out Transformer;
      Src  : in  Ada.Text_IO.File_Type)
   is
      State : State_Type := None;
      function Translate (S : String) return String is
      begin
         return Fixed.Translate (S, Maps.To_Mapping ("\", "/"));
      end Translate;

      function Dirname (S : String) return String is
      begin
         if S = "<Current_Directory>" then
            return Translate (GNAT.Directory_Operations.Get_Current_Dir);
         else
            return Translate (S);
         end if;
      end Dirname;

   begin
      while not Ada.Text_IO.End_Of_File (Src) loop
         declare
            S : constant String := Fixed.Trim (Ada.Text_IO.Get_Line (Src), Both);
         begin
            if S'Length = 0 then
               State := None;
            end if;
            case State is
            when  Source_Dirs =>
               This.Source_Dirs.Append (Dirname (S));
            when  Object_Dirs =>
               This.Object_Dirs.Append (Dirname (S));
            when  Project_Dirs =>
               This.Project_Path.Append (Dirname (S));
            when  Source_Files =>
               if not This.Ignore_Files.Contains (S) then
                  if Is_Spec (S) then
                     This.Spec_Files.Append (S);
                     This.Spec.Append (File2ada (S));
                     This.Source_Files.Append (S);
                  elsif Is_Impl (S) then
                     This.Impl_Files.Append (S);
                     This.Impl.Append (File2ada (S));
                     This.Source_Files.Append (S);
                  end if;
               end if;
            when None =>
               null;
            end case;

            if  S = Source_Dirs_Flag then
               State := Source_Dirs;
            elsif  S = Object_Dirs_Flag then
               State := Object_Dirs;
            elsif  S = Project_Dirs_Flag then
               State := Project_Dirs;
            elsif  S = Source_Files_Flag then
               State := Source_Files;
            elsif  S = "" then
               State := None;
            end if;

         end;
      end loop;

   end Read;

   procedure Read (This : in out Transformer; Src : in String) is
      F  : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (F, Ada.Text_IO.In_File, Src);
      This.Read (F);
      Ada.Text_IO.Close (F);
   end Read;

   --------------
   -- WriteXml --
   --------------

   procedure Write
     (This : in out Transformer;
      Tgt  : in Ada.Text_IO.File_Type)
   is
      use Ada.Text_IO;
      Index : Natural := 1;
      function Img (N : Integer) return String is
      begin
         return Fixed.Trim (N'Img, Both);
      end Img;

      procedure CompositeTag (Vec : String_Vectors.Vector; Name : String) is
         procedure Print (C : String_Vectors.Cursor) is
         begin
            Put_Line (Tgt, "      <Entry>" &
                      "<Ind n=""1"">" &  Img (Index) &  "</Ind>" &
                      "<V>" & String_Vectors.Element (C) & "</V>" &
                      "</Entry>");
            Index := Index + 1;
         end Print;
      begin
         if Vec.Length > 0 then
            Index := 1;
            Put_Line (Tgt, "   <CompositeTag>");
            Put_Line (Tgt, "      <Tag>");
            Put_Line (Tgt, "         <Name>" & Name & "</Name>");
            Put_Line (Tgt, "      </Tag>");
            Vec.Iterate (Print'Access);
            Put_Line (Tgt, "   </CompositeTag>");
         end if;
      end CompositeTag;


   begin
      Put_Line (Tgt, "<?xml version=""1.0"" encoding=""UTF-8"" ?>");
      Put_Line (Tgt, "<Tagged xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"">");
      CompositeTag (This.Source_Dirs, "Source_Dirs");
      CompositeTag (This.Source_Files, "Source_Files");
      CompositeTag (This.Impl, "Impl");
      CompositeTag (This.Impl_Files, "Impl_Files");
      CompositeTag (This.Spec, "Spec");
      CompositeTag (This.Spec_Files, "Spec_Files");
      CompositeTag (This.Project_Path, "ADA_PROJECT_PATH");
      Put_Line (Tgt, "</Tagged>");
   end Write;

   procedure Write
     (This : in out Transformer;
      Tgt  : in String)
   is
      F  : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (F, Ada.Text_IO.Out_File, Tgt);
      This.Write (F);
      Ada.Text_IO.Close (F);
   end Write;

end Ssprep.Transformers;
