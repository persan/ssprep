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
with GNAT.Directory_Operations; use  GNAT.Directory_Operations;
with GNAT.OS_Lib; use GNAT.OS_Lib;
package body Ssprep.ProjectUtils is

   ----------------------
   -- Get_Project_Name --
   ----------------------

   function Get_Project_Name (Src : String) return String is
   begin
      return Base_Name (Get_Project_Path (Src));
   end Get_Project_Name;

   ----------------------
   -- Get_Project_Path --
   ----------------------

   function Get_Project_Path (Src : String) return String is

   begin
      if Is_Regular_File (Src & Directory_Separator & ".project") then
         return Src;
      else
         if Src'Length = 0 then
            return "";
         elsif Src'Length = 2 and then Src (Src'Last) = ':' then
            return "";
         else
            declare
               D : constant String := Dir_Name (Src);
            begin
               if D (D'Last) = '\' or D (D'Last) = '/' then
                  return Get_Project_Path (D (D'First .. D'Last - 1));
               else
                  return Get_Project_Path (D);
               end if;
            end;
         end if;
      end if;
   end Get_Project_Path;

end Ssprep.ProjectUtils;
