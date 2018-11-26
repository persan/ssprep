with Ssprep.String_Vectors;
package svn is
   function ls (path : String) return Ssprep.String_Vectors.Vector;
   procedure co (src  : String; tgt : String);
   function  url (path : String) return String;
end svn;
