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
with Ada.Text_IO;
with GNAT.Spitbol.Table_VString;

package Ssprep.ConfigParsers is
   pragma Elaborate_Body;
   --  The configuration file consists of sections,
   --  led by a "[section]" header and followed by "name: value" entries,
   --  with continuations in the style of RFC 822; "name=value" is also accepted.
   --  Note that leading whitespace is removed from values.
   --  The optional values can contain format strings which refer to other
   --  values in the same section, or values in a special DEFAULT section.
   --  Additional defaults can be provided on initialization and retrieval.
   --  Lines beginning with "#" or ";" "--" are ignored and may be used to provide comments.
   --
   --  For example:
   --
   --  [My Section]
   --    foodir: $(dir)s/whatever
   --    foodir2: %(dir)s/whatever
   --    dir=frob
   --    MultiLine= fool,\
   --               next_line,\
   --               $(dir)
   --
   --  would resolve the "%(dir)" and $(dir)to the
   --  value of "dir" ("frob" in this case).
   --  All reference expansions are done on demand.


   use GNAT.Spitbol;

   type VString_Table_Access is access all GNAT.Spitbol.Table_VString.Table;
   function To_String (Item : VString_Table_Access) return String;

   package Table_Table_VString is new
     GNAT.Spitbol.Table (VString_Table_Access, null, To_String);

   NoSectionError : exception;
   --  Exception raised when a specified section is not found.

   DuplicateSectionError : exception;
   --  Exception raised if add_section() is called with the name
   --  of a section that is already present.

   NoOptionError  : exception;
   --  Exception raised when a specified option is not found
   --  in the specified section.

   ValueError            : exception;

   type RawConfigParser is tagged private;
   --  The basic configuration object.
   --  This class does not support the magical interpolation behavior.

   function Sections (This : RawConfigParser)
                      return Table_Table_VString.Table;
   function Sections (This : RawConfigParser)
                      return Table_Table_VString.Table_Array;
   function Sections (This : RawConfigParser)
                      return String_Vectors.Vector;
   --  Return a list of the sections available;
   --  DEFAULT is not included in the list.

   procedure Add_Section (This : in out RawConfigParser; Section : String);
   --   Add a section named section to the instance.
   --   If a section by the given name already exists,
   --   DuplicateSectionError is raised.

   function Has_Section (This    : RawConfigParser;
                         Section : String) return Boolean;
   --  Indicates whether the named section is present in the configuration.
   --  The DEFAULT section is not acknowledged.


   function Options (This    : RawConfigParser;
                     Section : String) return String_Vectors.Vector;

   function Options (This    : RawConfigParser;
                     Section : String) return Table_VString.Table_Array;

   function Options (This    : RawConfigParser;
                     Section : String) return Table_VString.Table;


   --  Returns a list of options available in the specified section.


   function Has_Option (This    : RawConfigParser;
                        Section : String;
                        Option  : String) return Boolean;
   --  If the given section exists, and contains the given option, return True;
   --  otherwise return False.



   procedure Read (This               : in out RawConfigParser;
                   Filename           : String;
                   Ignore_Nonexisting : Boolean := True);
   procedure Read (This      : in out RawConfigParser;
                   Filenames : String_Vectors.Vector);

   --   Attempt to read and parse a list of filenames,
   --   If a file named in filenames cannot be opened, that file will be ignored.
   --   This is designed so that you can specify a
   --   list of potential configuration file locations
   --   (for example, the current directory, the user's home directory,
   ---  and some system-wide directory),
   --   and all existing configuration files in the list will be read.
   --   If none of the named files exist,
   --   the ConfigParser instance will contain an empty dataset.
   --   An application which requires initial values to be loaded from a file
   --   should load the required file or files using readfp()
   --   before calling read() for any optional files:
   --
   --  import ConfigParser, os
   --
   --  config = ConfigParser.ConfigParser()
   --  config.readfp(open('defaults.cfg'))
   --  config.read(['site.cfg', os.path.expanduser('~/.myapp.cfg')])
   --

   procedure Read (This      : in out RawConfigParser;
                   File      : Ada.Text_IO.File_Type);
   --  Read and parse configuration data from the file

   function Get (This    : RawConfigParser;
                 Section : String;
                 Option  : String) return String;
   --  Get an option value for the named section.

   function Get (This    : RawConfigParser;
                 Section : String;
                 Option  : String) return Integer;
   --  A convenience method which coerces the option in the specified section
   --  to an integer.

   function Get (This    : RawConfigParser;
                 Section : String;
                 Option  : String) return Float;
   --  A convenience method which coerces the option in the specified section
   --  to a floating point number.

   function Get (This    : RawConfigParser;
                 Section : String;
                 Option  : String) return Long_Float;
   --  A convenience method which coerces the option in the specified section
   --  to a floating point number.


   function Get (This    : RawConfigParser;
                 Section : String;
                 Option  : String) return Boolean;
   --   A convenience method which coerces the option in the specified section
   --  to a Boolean value. Note that the accepted values for the option are
   --  "1", "yes", "true", and "on", which cause this method to return True,
   --  and "0", "no", "false", and "off", which cause it to return False.
   --  These string values are checked in a case-insensitive manner.
   --  Any other value will cause it to raise ValueError.

   --
   function Items (This    : RawConfigParser;
                   Section : String) return Table_VString.Table_Array;
   function Items (This    : RawConfigParser;
                   Section : String) return Table_VString.Table;
   --  Return a list of (name, value) pairs for each option in the given section.


   procedure Set (This    : in out RawConfigParser;
                  Section : String;
                  Option  : String;
                  Value   : String);

   procedure Set (This    : in out RawConfigParser;
                  Section : String;
                  Option  : String;
                  Value   : Integer);

   procedure Set (This    : in out RawConfigParser;
                  Section : String;
                  Option  : String;
                  Value   : Boolean);

   procedure Set (This    : in out RawConfigParser;
                  Section : String;
                  Option  : String;
                  Value   : Float);

   procedure Set (This    : in out RawConfigParser;
                  Section : String;
                  Option  : String;
                  Value   : Long_Float);
   --  If the given section exists, set the given option to the specified value;
   --  otherwise raise NoSectionError.
   --  While it is possible to use RawConfigParser
   --  (or ConfigParser with raw parameters set to true)
   --  for internal storage of non-string values,
   --  full functionality (including interpolation and output to files)
   --  can only be achieved using string values.

   procedure Null_Handle_Section
     (This : in RawConfigParser; Section : String) is null;
   procedure Null_Handle_Value
     (This : in RawConfigParser; Section, Option, Value : String) is null;

   procedure Iterate
     (This                             : in RawConfigParser;
      Handle_Section                   : not null access procedure
        (This           : in RawConfigParser;
         Section : String) := Null_Handle_Section'Access;
      Handle_Value                     : not null access procedure
        (This           : in RawConfigParser;
         Section, Option, Value : String) := Null_Handle_Value'Access);
   --  Iterates iver all values and sections in the configuration

   procedure Write (This       : in RawConfigParser;
                    File       : in Ada.Text_IO.File_Type;
                    Header     : Boolean := False);
   procedure Write (This       : in RawConfigParser;
                    File       : in String);

   --  Write a representation of the configuration to the specified file object.
   --  This representation can be parsed by a future read() call.
   --
   procedure Remove (This    : in out RawConfigParser;
                     Section : String;
                     Option  : String);
   --  Remove the specified option from the specified section.
   --  If the section does not exist, raise NoSectionError.

   procedure Remove (This    : in out RawConfigParser;
                     Section : String);
   --  Remove the specified section from the configuration.
   --  If the section in fact existed, return True. Otherwise return False.

   --  optionxform( option)
   --
   --  Transforms the option name option as found in an input file or as
   --  passed in by client code to the form that should be used in the internal structures.
   --  The default implementation returns a lower-case version of option;
   --  subclasses may override this or client code can set an attribute of this
   --  name on instances to affect this behavior. Setting this to str(),
   --  for example, would make option names case sensitive.

private

   type RawConfigParser is tagged record
      Data     : Table_Table_VString.Table (32);
      Default  : Table_VString.Table (32);
   end record;


end Ssprep.ConfigParsers;
