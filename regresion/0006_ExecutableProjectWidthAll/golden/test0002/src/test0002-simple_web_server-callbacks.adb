-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------
package body test0002.Simple_WEB_Server.Callbacks is

   -----------
   -- HW_CB --
   -----------

   function HW_CB (Request : AWS.Status.Data) return AWS.Response.Data is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "<p>Hello world !");
   end HW_CB;

end test0002.Simple_WEB_Server.Callbacks;