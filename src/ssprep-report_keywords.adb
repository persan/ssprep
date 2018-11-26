with Ada.IO_Exceptions;
with GNAT.Regpat;
with GNAT.OS_Lib;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Fixed;
package body Ssprep.Report_Keywords is

   ------------
   -- Add_OK --
   ------------
   LC  : array (Character) of Character;

   procedure Add_OK
     (self   : in out reporter;
      file   : String;
      Line   : Integer;
      Column : Integer;
      Word   : String)
   is
      ok : report;
   begin

      ok :=
        (new String'(Ada.Directories.Simple_Name (file)),
         Line,
         Column,
         new String'(Word));
      self.Add_OK (ok);
   end Add_OK;

   ------------
   -- Add_OK --
   ------------

   procedure Add_OK
     (self     : in out reporter;
      Location : String)
   is
   begin
      self.Add_OK (Value (Location));
   end Add_OK;

   procedure Add_OK (self     : in out reporter;
                     Location : report) is
   begin
      self.OkReports.Append (Location);
   end Add_OK;

   procedure Add_OK (self : in out reporter;
                     File : Ada.Text_IO.File_Type) is
      procedure parse (line : String) is
      begin
         if line'Length > 3 then
            declare
               t : constant String (1 .. 3) := line (line'First .. line'First + 2);
            begin
               if  t (1) /= '#'  then
                  self.Add_OK (line);
               end if;
            end;
         end if;
      end parse;
   begin
      while not Ada.Text_IO.End_Of_File (File) loop
         parse (Ada.Text_IO.Get_Line (File));
      end loop;
   end Add_OK;



   procedure add_word (self  : in out reporter;
                       word  : String) is
   begin
      self.Words.Append (new String'(word));
   end add_word;

   procedure add_words (self  : in out reporter;
                        path  : String) is
      f : Ada.Text_IO.File_Type;
   begin
      if not Ada.Directories.Exists (path) then
         raise Ada.IO_Exceptions.Name_Error with path & " does not exists.";
      end if;
      Ada.Text_IO.Open (f, Ada.Text_IO.In_File, path);
      while not Ada.Text_IO.End_Of_File (f) loop
         declare
            l : constant String := Ada.Strings.Fixed.Trim (Ada.Text_IO.Get_Line (f), Ada.Strings.Both);
         begin
            if l'Length > 1 then
               if l (l'First) /= '#' and then l (l'First) /= '-' then
                  self.add_word (l);
               end if;
            end if;
         end;
      end loop;
   end add_words;


   -------------
   -- analyze --
   -------------

   procedure analyze
     (self     : in out reporter;
      Root     : String)
   is
      use Ada.Directories;

   begin
      case Kind (Root) is
         when Directory =>
            self.Analyze_Dir (Root);
         when Ordinary_File =>
            self.Analyze_file (Root);
         when Special_File =>
            null;
      end case;
   end analyze;


   procedure Analyze_Buffer (self     : in out reporter;
                             fileName : String;
                             buffer   : String;
                             OK       : out Boolean) is
      Line       : Integer := 1;
      Line_Start : Natural := buffer'First;
      cursor     : Natural := buffer'First;
      procedure scan (s : String) is
         procedure process (Position : String_Vectors.Cursor) is
            index : Natural;
            check : constant GNAT.Strings.String_Access := String_Vectors.Element (Position);
         begin
            index := Ada.Strings.Fixed.Index (s, check.all);
            if index /= 0 then
               self.reports.Append
                 ((new String'(fileName),
                  Line,
                  cursor - Line_Start,
                  check));
               OK := False;
            end if;
         end process;

      begin
         self.Words.Iterate (process'Access);
      end scan;
   begin
      OK := True;

      loop
         cursor := cursor + 1;
         exit when cursor > buffer'Last;
         if buffer (cursor) = ASCII.LF then
            scan (buffer (Line_Start .. cursor));
            Line := Line + 1;
            Line_Start := cursor + 1;
         end if;
      end loop;
   end Analyze_Buffer;

   procedure Analyze_file (self     : in out reporter;
                           path     : String) is

      f : GNAT.OS_Lib.File_Descriptor;
      buffer : GNAT.Strings.String_Access;
      use GNAT.OS_Lib;
      size : Integer;
      OK   : Boolean;
      Simple_Name : constant String_Access := new String'(Ada.Directories.Simple_Name (path));
   begin
      f := Open_Read (path, Text);
      buffer := new String (1 .. Natural (File_Length (f)));
      size := Read (f, buffer.all'Address, buffer.all'Length);
      if size /= buffer.all'Length then
         raise Constraint_Error;
      end if;
      if self.casesensetive then
         for i in buffer.all'Range loop
            buffer.all (i) := LC (buffer.all (i));
         end loop;
      end if;
      self.Analyze_Buffer (buffer.all,
                           Ada.Directories.Simple_Name (path),
                           OK);
      if not OK then
         self.Fail_Files.Append (Simple_Name);
      else
         self.OK_Files.Append (Simple_Name);
      end if;
      Free (buffer);
      Close (f);
   exception
      when e : others =>
         Free (buffer);
         Close (f);
         Ada.Exceptions.Raise_Exception (Ada.Exceptions.Exception_Identity (e),
                                         "Unable to read :" & path);
   end Analyze_file;

   procedure Analyze_Dir (self     : in out reporter;
                          path     : String) is
      use Ada.Directories;
      procedure process (Directory_Entry : Directory_Entry_Type) is
         name : constant String := Simple_Name (Directory_Entry);
      begin
         if name /= ".." and name /= "." then
            self.analyze (Full_Name (Directory_Entry));
         end if;
      end process;
   begin
      Ada.Directories.Search (path, "*", Process =>  process'Access);
   end Analyze_Dir;

   ------------------
   -- print_Report --
   ------------------

   procedure print_Report
     (self    : in out reporter;
      To_File : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output)
   is
      procedure Process (Position : report_Vectors.Cursor) is
      begin
         Ada.Text_IO.Put_Line (To_File, Image (report_Vectors.Element (Position)));
      end Process;
   begin
      self.reports.Iterate (Process'Access);
   end print_Report;

   -------------------------
   -- print_Report_In_Xml --
   -------------------------
   function image (i : Integer) return String is
   begin
      return Ada.Strings.Fixed.Trim (i'Img, Ada.Strings.Both);
   end image;

   procedure print_Report_In_Xml
     (self    : in out reporter;
      To_File : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output)
   is
      use Ada.Text_IO;

   begin
      Put_Line (To_File, "<?xml version='1.0' encoding='utf-8' ?>");
      Put_Line (To_File, "<TestRun elapsed='1.107E-4'>");
      Put_Line (To_File, "  <Statistics>");
      Put_Line (To_File, "    <Tests>" & image (self.Scaned_Files) & "</Tests>");
      Put_Line (To_File, "    <FailuresTotal>" & image (Integer (self.OK_Files.Length)) & "</FailuresTotal>");
      Put_Line (To_File, "    <Failures></Failures>");
      Put_Line (To_File, "    <Errors></Errors>");
      Put_Line (To_File, "  </Statistics>");

      Put_Line (To_File, "  <SuccessfulTests>");
      for i in self.OK_Files.First_Index .. self.OK_Files.Last_Index loop
         Put_Line (To_File, "    <Test>");
         Put_Line (To_File, "       <Name>Invalid Words:" & self.OK_Files.Element (i).all & "</Name>");
         Put_Line (To_File, "    </Test>");
      end loop;
      Put_Line (To_File, "  </SuccessfulTests>");


      Put_Line (To_File, "  <FailedTests>");

      for i in self.OK_Files.First_Index .. self.OK_Files.Last_Index loop
         Put_Line (To_File, "    <Test>");
         declare
            r : constant report := self.reports.Element (i);
         begin
            Put_Line (To_File, "      <Name>Test addition (failure expected)</Name>");
            Put_Line (To_File, "      <FailureType>Invalid Word</FailureType>");
            Put_Line (To_File, "      <Message>Invalid word " & r.word.all & "found in file</Message>");
            Put_Line (To_File, "      <Location>");
            Put_Line (To_File, "        <File>" & r.file.all & "</File>");
            Put_Line (To_File, "        <Line>" & image (r.Line) & "</Line>");
            Put_Line (To_File, "        <Column>" & image (r.Column) & "</Column>");
            Put_Line (To_File, "      </Location>");
         end;
         Put_Line (To_File, "    </Test>");
      end loop;
      Put_Line (To_File, "  </FailedTests>");

      Put_Line (To_File, "</TestRun>");
   end print_Report_In_Xml;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out reporter) is
   begin
      Object.Ignored := GNAT.Regexp.Compile (Default_Ignore);
      Object.ScanSuffixes := GNAT.Regexp.Compile (Default_File_Types);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out reporter) is
   begin
      null;
   end Finalize;

   function Image (self : report) return String is
   begin
      return self.file.all & ":" & image (self.Line) & ":" & image (self.Column) & " " &  self.word.all;
   end Image;

   Matcher : constant GNAT.Regpat.Pattern_Matcher := GNAT.Regpat.Compile ("^([^:]:?[^:]*):(\d+):((\d+):)? ((warning)?(\(style)?.*)");

   function Value (self : String) return report is
      use GNAT.Regpat;
      matches : Match_Array (1 .. Paren_Count (Matcher));
   begin
      return ret : report do
         Match (Matcher, self, matches);
         if matches (1) /= No_Match then
            ret := (new String'(self (matches (1).First .. matches (1).Last)),
                    Integer'Value (self (matches (1).First .. matches (1).Last)),
                    Integer'Value (self (matches (1).First .. matches (1).Last)),
                    new String'(self (matches (1).First .. matches (1).Last)));
         end if;
      end return;
   end Value;


   procedure setCaseSensetive (self          : in out reporter;
                               CaseSensetive : Boolean) is
      s : GNAT.Strings.String_Access;
   begin
      self.casesensetive := CaseSensetive;
      if CaseSensetive then
         for i in self.Words.First_Index .. self.Words.Last_Index loop
            s := self.Words.Element (i);
            for j in s'Range loop
               s.all (j) := LC (s (j));
            end loop;
         end loop;
      end if;
   end setCaseSensetive;

begin
   for i in Character'Range loop
      LC (i) := i;
   end loop;
   LC ('A' .. 'Z') := "abcdefghijklmnopqrstuvwxyz";
   LC ('Å') := 'å';
   LC ('Ä') := 'ä';
   LC ('Ö') := 'ö';
end Ssprep.Report_Keywords;
