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
with Ada.Text_IO;
with GNAT.Regexp;
with Ssprep.String_Vectors;
with GNAT.Spitbol;
with Ssprep.Templates;
package Ssprep.Translators is
   type Translator is tagged private;

   procedure Translate (This         : in out Translator;
                        Source       : Templates.Any_Template;
                        Translations : Templates_Parser.Translate_Set;
                        Target_Dir   : String);

   procedure Translate (This         : in out Translator;
                        Source       : String;
                        Translations : Templates_Parser.Translate_Set;
                        Target_Dir   : String);
   --
   --  Copys a file or directory structure from source to target
   --  Using the string translations defined in this.
   ------------------------------------------------------------


   procedure Set_Symbol (This           : in out Templates_Parser.Translate_Set;
                         Value_And_Name : String;
                         Delimiter      : Character := ',';
                         Define_File    : Boolean := False);
   --  Sets the symbol Name to value.
   --  Where the Value_And_Name string has the folowing syntax:
   --    Name=Value

   procedure Set_Symbol (This        : in out Templates_Parser.Translate_Set;
                         Name        : String;
                         Value       : String;
                         Delimiter   : Character := ',';
                         Define_File : Boolean := False);
   --  Sets the symbol Name to value.

   procedure Read_Symbols (This            : in out Templates_Parser.Translate_Set;
                           From_File       : String);
   --  Read symbols from xml-file From_File where format is defined
   --  in Templates_Parser.XML

   procedure Read_Symbols_From_Manifest (This        : in out Templates_Parser.Translate_Set;
                                         Source      : String;
                                         Delimiter   : Character := ',';
                                         Define_File : Boolean);
   --  Read symbols from a Manfest file, where the format is a

   procedure Append_Symbols (This      : in out Templates_Parser.Translate_Set;
                             Symbols   : Templates_Parser.Translate_Set);

   procedure Write_Symbols (This    : Templates_Parser.Translate_Set;
                            To_File : String);
   --  writes the defined symbols to file in XML format

   procedure Set_Replace (This       : in out Translator;
                          Replace    : Boolean);
   --  Sets Replace mode

   procedure Set_Verbose (This       : in out Translator;
                          Verbose    : Boolean);
   --  Sets Verbosity

   procedure Set_Echo (This       : in out Translator;
                       Echo       : Boolean);
   --  If echo is set to true the no files or directores wil be created
   --  and the resulting textstream from the translations will
   --  be echoed to standard output.
   procedure Set_Hint (This       : in out Translator;
                       Hint       : Boolean);

   procedure Set_Java_Expand (This       : in out Translator;
                              Expand     : Boolean);
   --  If java expand is set then *.{Java_Suffix} files wil be expanded to the correct
   --  package directory relative its source
   --  Note that target Java files is calculated from the contents of
   --  the file.

   procedure Set_Effort_Only (This           : in out Translator;
                              Effort_Only    : Boolean);
   --  Set Effort only

   procedure Add_Environment (This : in out Templates_Parser.Translate_Set);
   --  Include the environment variables into the
   --  Translateset.

   --     function Ada2file (This : String) return String;
   --     --  Translates an Ada name to a filename part
   --     --  with GNAT namining rules
   --
   --     function Ada2dir (This : String) return String;
   --     --  Replaces "." in names with "-"


   procedure Translate_File (This         : in out Translator;
                             Source       : String;
                             Translations : Templates_Parser.Translate_Set;
                             Target       : String);

   procedure Translate_Dir (This         : in out Translator;
                            Source       : String;
                            Translations : Templates_Parser.Translate_Set;
                            Target       : String);

   function Translate_String (Translations : Templates_Parser.Translate_Set;
                              Source       : String) return String;

   procedure Execute_Children (This   : Translator);

   procedure Set_Log_Exceptions (This              : in out Translator;
                                 Log_Exceptions    : Boolean);


   procedure Set_Delimiter (This         : in out Translator;
                            Delimiter    : Character);
   function get_Delimiter (this : Translator) return Character;
   --  sets the delimiter to be used in vector values.
   procedure Set_Dont_Expand (This         : in out Translator;
                              Regexp       : String);

   procedure Set_Ignore (This         : in out Translator;
                         Regexp       : String);

private

   type Translator is tagged record
      Replace        : Boolean := False;
      Verbose        : Boolean := False;
      Effort_Only    : Boolean := False;
      Echo           : Boolean := False;
      Expand_Java    : Boolean := False;
      Hint           : Boolean := False;
      Log_File       : Ada.Text_IO.File_Access := Ada.Text_IO.Current_Output;
      Ignore         : GNAT.Regexp.Regexp := GNAT.Regexp.Compile (Default_Ignore);
      Dont_Expand    : GNAT.Regexp.Regexp := GNAT.Regexp.Compile (Default_Dont_Expand);
      Execution_List : String_Vectors.Vector;
      Root           : GNAT.Spitbol.VString;
      Log_Exceptions : Boolean := False;
      Delimiter      : Character := Default_Delimiter;
      Template       : Templates.Any_Template;
   end record;

   procedure Translate_Inner (This          : in out Translator;
                              Source        : String;
                              Translations  : Templates_Parser.Translate_Set;
                              Target_Root   : String);

end Ssprep.Translators;
