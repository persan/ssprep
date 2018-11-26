with Ssprep.String_Vectors;
package Ssprep.GNATls is
   function ADA_PROJECT_PATH return Ssprep.String_Vectors.Vector;
   function To_Path (Item : String) return Ssprep.String_Vectors.Vector;
   function ToString (Value : Ssprep.String_Vectors.Vector) return String;
end Ssprep.GNATls;
