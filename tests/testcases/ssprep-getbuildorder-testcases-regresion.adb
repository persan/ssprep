with GNAT.Source_Info;
with GNAT.String_Split;
with Ada.Text_IO;
with GNAT.Spitbol.Table_VString;
package body Ssprep.Getbuildorder.Testcases.Regresion is
   use AUnit;
   use GNAT.String_Split;
   use GNAT.Spitbol;

   --  Fixture elements

   Regresion_Results : constant String := "../regresion/testsuite.res";

   Results : GNAT.Spitbol.Table_VString.Table (64);
   ----------
   -- Name --
   ----------

   function Name (T : Test_Case)
                  return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return Format (GNAT.Source_Info.File & "Rersults from regresion test");
   end Name;


   -------------------------
   --  SampleTest
   -------------------------
   procedure CheckRegresion (Test : in out AUnit.Test_Cases.Test_Case'Class) is
      Name : constant Message_String :=  Test.Routine_Name;
      Result : constant VString := Table_VString.Get (Results, Name.all);
   begin
      Test.Assert (S (Result) = "OK", "Got " & S (Result));
   end CheckRegresion;

   --------------------
   -- Register_Tests --
   --------------------

   procedure Register_Tests (T : in out Test_Case) is
      use Test_Cases.Registration;
      procedure Add (S : String) is
         Slices : GNAT.String_Split.Slice_Set;
      begin
         Create (Slices, S, ":");
         Register_Routine (T, CheckRegresion'Access, Slice (Slices, 1));
         Table_VString.Set (Results, Slice (Slices, 1), V (Slice (Slices, 1)));
      end Add;
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (F, Ada.Text_IO.In_File, Regresion_Results);
      while not Ada.Text_IO.End_Of_File (F) loop
         Add (Ada.Text_IO.Get_Line (F));
      end loop;
      Ada.Text_IO.Close (F);
   end Register_Tests;

end Ssprep.getBuildOrder.Testcases.Regresion;
