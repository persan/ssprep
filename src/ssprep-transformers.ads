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
with Ssprep.String_Vectors;
with Ssprep.String_Sets;

with Ada.Text_IO;
package Ssprep.Transformers is
   type Transformer is tagged private;

   procedure Read (This : in out Transformer; Src : in Ada.Text_IO.File_Type);
   procedure Read (This : in out Transformer; Src : in String);
   --  Reads output from gnat list -v To get:
   --   Source_Dirs
   --   Object_Dirs
   --   ADA_PROJECT_PATH
   --  Reads output from gnat bind -R to get
   --   Source_Files
   --   Spec/SpecFiles
   --   Impl/ImplFiles


   procedure Write (This : in out Transformer; Tgt : in Ada.Text_IO.File_Type);
   procedure Write (This : in out Transformer; Tgt : in String);
   --  Writes the Contenst of symbols in
   --  Templates_Parse symbol format.

   procedure Dump (This : in out Transformer);

   procedure Add_Ignore (This : in out Transformer; F : in String);

private
   procedure Parse (This : in out Transformer; Src : in  String);
   type Transformer is tagged record
      Full_List         : String_Vectors.Vector;
      Source_Dirs       : String_Vectors.Vector;
      Object_Dirs       : String_Vectors.Vector;
      Project_Path      : String_Vectors.Vector;

      Source_Files      : String_Vectors.Vector;
      Impl              : String_Vectors.Vector;
      Impl_Files        : String_Vectors.Vector;
      Spec              : String_Vectors.Vector;
      Spec_Files        : String_Vectors.Vector;
      Ignore_Files      : String_Sets.Set;
   end record;


   Source_Dirs_Flag : constant String := "Source Search Path:";
   Object_Dirs_Flag : constant String := "Object Search Path:";
   Project_Dirs_Flag : constant String := "Project Search Path:";
   Source_Files_Flag : constant String := "REFERENCED SOURCES";
   type State_Type is (None, Source_Dirs, Object_Dirs, Project_Dirs, Source_Files);

end Ssprep.Transformers;
