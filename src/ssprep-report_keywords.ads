with Ada.Containers.Vectors;
with Ada.Text_IO;
with Ada.Finalization;
with GNAT.Regexp;
with GNAT.Strings;
package Ssprep.Report_Keywords is

   Default_File_Types : constant String := ".ads|.adb|.c|.java|.cpp|.cxx|.txt|.h|.hh";
   Default_Ignore     : constant String := ".+\.exe|.+\o|.+\ali|.svn|CVS|lib-obj|bin|obj|lib";

   type reporter is tagged private;

   procedure Add_OK (self : in out reporter;
                     file : String;
                     Line : Integer;
                     Column : Integer;
                     Word   : String);
   procedure Add_OK (self : in out reporter;
                     Location : String);

   procedure Add_OK (self : in out reporter;
                     File : Ada.Text_IO.File_Type);

   procedure analyze (self     : in out reporter;
                      Root     : String);
   procedure setCaseSensetive (self          : in out reporter;
                               CaseSensetive : Boolean);
   procedure add_word (self  : in out reporter;
                       word  : String);
   procedure add_words (self : in out reporter;
                        path  : String);
   procedure print_Report
     (self    : in out reporter;
      To_File : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output);
   procedure print_Report_In_Xml
     (self    : in out reporter;
      To_File : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output);

private

   type report is record
      file   : GNAT.Strings.String_Access;
      Line   : Integer;
      Column : Integer;
      word   : GNAT.Strings.String_Access;
   end record;
   function Image (self : report) return String;
   function Value (self : String) return report;

   procedure Add_OK (self : in out reporter;
                     Location : report);

   package report_Vectors is new
     Ada.Containers.Vectors (Natural, report);
   package String_Vectors is new
     Ada.Containers.Vectors (Natural,
                             GNAT.Strings.String_Access,
                             GNAT.Strings."=");

   type reporter is new Ada.Finalization.Controlled with  record
      Scaned_Files  : Natural := 0;
      reports       : report_Vectors.Vector;
      OK_Files      : String_Vectors.Vector;
      Fail_Files    : String_Vectors.Vector;
      OkReports     : report_Vectors.Vector;
      Words         : String_Vectors.Vector;
      casesensetive : Boolean := False;
      Ignored       : GNAT.Regexp.Regexp;
      ScanSuffixes  : GNAT.Regexp.Regexp;
   end record;
   procedure Initialize (Object : in out reporter);
   procedure Finalize   (Object : in out reporter);

   procedure Analyze_file (self     : in out reporter;
                           path     : String);

   procedure Analyze_Dir (self     : in out reporter;
                          path     : String);
   procedure Analyze_Buffer (self     : in out reporter;
                             fileName : String;
                             buffer   : String;
                             OK       : out Boolean);
end Ssprep.Report_Keywords;
