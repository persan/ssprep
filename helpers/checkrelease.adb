with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Ssprep;
with Ssprep.Version;
with GNAT.Calendar.Time_IO;
with Ada.Calendar;

procedure Checkrelease is
begin

   -- Check for dirty folder
   declare
      F : Ada.Text_IO.File_Type;
   begin
      Open (F, In_File, Argument (1));
      while not End_Of_File (F) loop
         declare
            S : constant String := Get_Line (F);
         begin
            if S /= "" then
               Put_Line ("Dirty files:" & S);
               Set_Exit_Status (Failure);
            end if;
         end;
      end loop;
      Close (F);
   end;

   -- Check Version version vesrions in repository
   declare
      F            : Ada.Text_IO.File_Type;
   begin
      Open (F, In_File, Argument (2));
      while not End_Of_File (F) loop
         declare
            S : constant String := Get_Line (F);
         begin
            if Ssprep.Version.Rev = S (S'First .. S'Last - 1) then
               Put_Line ("Version " &  Ssprep.Version.Rev & " does alredy exists.");
               Set_Exit_Status (Failure);
            end if;
         end;
      end loop;
      Close (F);
   end;
   -- Check Release notes
   declare
      F            : Ada.Text_IO.File_Type;
      Date_Line    : natural := 0;
      Version_Line : natural := 0;
      Now          : constant String := GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock,
                                                                     GNAT.Calendar.Time_IO.ISO_Date);
      NowCode      : constant String := GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock,"%Y%m%d");

   begin
      Open (F, In_File, Argument (3));
      while not End_Of_File (F) loop
         declare
            line : constant String := Get_Line (F);
         begin
            if Version_Line = 0 then
               Version_Line := ada.Strings.Fixed.Index (line, Ssprep.Version.Rev);
            end if;
            if Date_Line = 0 then
               Date_Line := ada.Strings.Fixed.Index (line, Now);
            end if;
         end;
      end loop;

      if Date_Line = 0  or Date_Line = 15 then
         Put_line ("Current date '" & Now & "' not found in release notes");
         Set_Exit_Status (Failure);
      end if;

      if Version_Line = 0  or
        Version_Line >= 15 then
         Put_line ("Current Version '" & Ssprep.Version.Rev & "' not found in release notes");
         Set_Exit_Status (Failure);
         Close (F);
      end if;
      if Ssprep.Version.Date /= NowCode then
         Put_Line ("Current Date '" & NowCode & "' not found in code '" & Ssprep.Version.Date & "'");
         Set_Exit_Status (Failure);
      end if;
   end;
end;
