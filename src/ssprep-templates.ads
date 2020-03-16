--  ---------------------------------------------------------------------------
--  Copyright 2008 Per Sandberg  <per.s.sandberg@bahnhof.se>
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
with Templates_Parser;
private with Ssprep.String_Vectors;
private with DOM.Core;
private with GNAT.Spitbol.Table_Boolean;
private with Ada.Containers.Indefinite_Ordered_Maps;
with GNAT.Strings;
with Ssprep.String_Sets;
with Ada.Containers.Indefinite_Vectors;
package Ssprep.Templates is
   use GNAT.Strings;

   type Templates is tagged limited private;
   type Any_Templates is access all Templates'class;
   --  Database of templates
   type Template is tagged private;
   type Any_Template is access all Template'class;
   type Any_String_Set is access all String_Sets.Set'Class;

   procedure Add_Template (This : in out Templates;
                           Name : String;
                           Path : Template'Class);
   --  Adds a specific template and its locaion to the templates database.

   procedure Add_Template (This : in out Templates;
                           Name : String;
                           Path : String);

   procedure Add_Config_File (This        : in out Templates;
                              Config_File : String);
   --  Includes the templates in the configfile into the templates database.

   procedure Add_String (This : in out Templates;
                         Data : String);
   --  Includes the templates in the String into the templates database.
   --  Note this is read is context insensitive.

   procedure Add_Directory (This      : in out Templates;
                            Directory : String);
   --  Reads the Config_File in that direcory if it exists
   --  otherwise it does nothing.

   procedure Add_Path (This : in out Templates;
                       Path : String);
   --  Tries to add all directories in the path

   --- function Get (This : in Templates; Name : String) return String;
   --  Returns a full path to a template given a name.
   --  or if not template is found an empty string.
   --  Note that if the name is the path to an existing object
   --  then that file will be returned otherwise the templates databes will be
   --  searched.

   function Get (This : in Templates; Name : String) return Template'Class;
   --  Returns a template given a name.
   --  Note that if the name is the path to an existing object
   --  then that file will be returned otherwise the templates databes will be
   --  searched.
   --  If no template is found then an invalid template will be returned.


   function Get_Paths (This : Template) return Any_String_Set;
   --  Returns a full path to a template given a name.
   --  or if the template is invalid an empty string.

   function Get_Errors_Found (This : Templates) return Boolean;
   --  Returns a True if any errors was found.

   procedure Set_Break_On_Error (This : in out Templates; breake :  Boolean);
   --  Control error handling

   function Is_Valid (This : Template) return Boolean;
   --  Returns true if the Template is valid.

   procedure Dump (This     : in Templates;
                   Put_Line : not null access procedure (Item : String);
                   XML      : Boolean := False);
   --  dump the templates database

   function Get_System_Templates_Dir return String;
   --  Returns the systems templates directory that is
   --  ${command}/../../share/ssprep/templates

   function Get_User_Templates_Dir return String;
   --  Returns the users templates directory that is
   --  ${HOME}/.ssprep


   procedure Set_Verbose (This       : in out Templates;
                          Verbose    : Boolean);
   --  Sets Verbosity
   function Get_Path (This  : Templates) return String;

   procedure Set_Quiet (This       : in out Templates;
                        Quiet      : Boolean);
   --  If set then nothing but the translated test is printed.
   function Get_Symbols (This  : in Templates) return not null access constant
     Templates_Parser.Translate_Set;
   function New_Template (Location  : String) return Template;

   function getHint (this : Template) return access String;
private

   type Variable is abstract tagged record
      parent  : Any_Template;
      Name    : String_Access;
      ToolTip : String_Access;
      Prompt  : String_Access;
   end record;
   type Any_Variable is access all Variable'class;

   procedure Put_Element (This : Variable; Tag : String; Value : access String; indent : String := "");
   procedure Put_Element (This : Variable; Tag : String; Value : String; indent : String := "");
   procedure Put_Variable_Heading (This : Variable; V_Type : String; indent : String := "");
   procedure Put_Variable_Footing (This : Variable; Indent : String := "");
   procedure Read (this : access Variable; from : DOM.Core.Node) is null;
   procedure Dump_XML (This : Variable; indent : String := "") is null;

   type String_Variable is new Variable with null record;
   procedure Dump_XML (This : String_Variable; indent : String := "");

   type Project_Variable is new Variable with null record;
   procedure Dump_XML (This : Project_Variable; indent : String := "");

   type File_Selection_Variable is new Variable with record
      Must_exist : Boolean := False;
   end record;
   procedure Dump_XML (This : File_Selection_Variable; indent : String := "");
   procedure Read (this : access File_Selection_Variable; from : DOM.Core.Node);

   type Dir_Selection_Variable is new File_Selection_Variable with null record;

   procedure Dump_XML (This : Dir_Selection_Variable; indent : String := "");

   type Boolean_Variable is new Variable with null record;
   procedure Dump_XML (This : Boolean_Variable; indent : String := "");
   procedure Read (this : access Boolean_Variable; from : DOM.Core.Node);

   type Selection_Variable is new Variable with record
      Prompts : String_Vectors.Vector;
      Values  : String_Vectors.Vector;
   end record;
   procedure dump_xml (This : Selection_Variable; indent : String := "");
   procedure Read (this : access Selection_Variable; from : DOM.Core.Node);

   package Variable_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Any_Variable);

   type Template is tagged record
      Locations : aliased String_Sets.Set;
      Valid     : Boolean := False;
      Simple    : Boolean := False;
      Helper    : Boolean := False;
      Variables : Variable_Vectors.Vector;
      Alias     : String_Access;
      Prompt    : String_Access;
      Hint      : String_Access;
      ToolTip   : String_Access;
      Class     : String_Access;
      Path      : String_Access;
      Output    : String_Access;
      Load      : String_Access;
   end record;

   procedure Parse_Variables (This    : in out Template;
                              N       : DOM.Core.Node);
   procedure Parse_Template (This     : in out Templates;
                             N        : DOM.Core.Node);

   procedure Parse_Variable (This      : in out Template;
                             N         : DOM.Core.Node;
                             V_Name    : String;
                             V_Type    : String;
                             V_Prompt  : String;
                             V_ToolTip : String);
   procedure add_Variables (This       : in out Templates;
                            N          : DOM.Core.Node);
   package Template_Storages is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Template'Class, "<", "=");

   type Templates is tagged limited record
      Symbols        : aliased Templates_Parser.Translate_Set;
      Sources        : Template_Storages.Map;
      Verbose        : Boolean := False;
      PATH           : String_Vectors.Vector;
      Config_Files   : GNAT.Spitbol.Table_Boolean.Table (32);
      Quiet          : Boolean := False; -- Dont print anything if quiet.
      Errors_Found   : Boolean := False;
      Abort_On_Error : Boolean := False;
   end record;

   procedure Parse_Document (This        : in out Templates;
                             Doc         : in DOM.Core.Node);

   procedure Parse_Ssprep (This        : in out Templates;
                           Doc         : in DOM.Core.Node);

   procedure Parse_Templates (This        : in out Templates;
                              N           : in DOM.Core.Node);


   function Get_Value (N : in DOM.Core.Node) return String;

   procedure Error (Node : in DOM.Core.Node; Message : in String);

end Ssprep.Templates;
