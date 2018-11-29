-- ---------------------------------------------
--
--
--
--
-- ---------------------------------------------
with AWS.Response;
with AWS.Status;

package test0002.Simple_WEB_Server.Callbacks is

   function HW_CB (Request : AWS.Status.Data) return AWS.Response.Data;

end test0002.Simple_WEB_Server.Callbacks;