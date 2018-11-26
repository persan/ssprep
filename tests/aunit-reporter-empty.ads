package AUnit.Reporter.Empty is

   type Empty_Reporter is new Reporter with null record;

   procedure Report (Engine  : Empty_Reporter;
                     R       : in out Result'Class;
                     Options : AUnit_Options := Default_Options) is null;
end AUnit.Reporter.Empty;
