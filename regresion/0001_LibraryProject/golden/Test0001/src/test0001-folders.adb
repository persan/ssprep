-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------

with Ada.Command_Line;
with GNAT.Regpat; use GNAT.Regpat;
with Ada.Directories; use Ada.Directories;
package body Test0001.Folders  is

   function prefix return string is
      cmd : constant string := Ada.Command_Line.Command_Name;
      matcher : Pattern_Matcher := compile ("(^.+)(/bin/|\\bin\\).+");
      matches : Match_Array (1 .. Paren_Count (matcher));
   begin
      match (matcher, cmd, matches);
      if matches (1) /= no_Match then
         return cmd (matches (1).First .. matches (1).last);
      else
         return "";
      end if;
   end prefix;

   function share return string is
   begin
      return Compose (Compose (prefix, "share"), name);
   end share;

   function doc return string is
   begin
      return Compose (Compose (prefix, "doc"), name);
   end doc;

end Test0001.Folders;