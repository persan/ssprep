@@INCLUDE@@ ../../../Common/FileHeader.txt
with AWS.Response;
with AWS.Status;

package @_project_@.Simple_WEB_Server.Callbacks is

   function HW_CB (Request : AWS.Status.Data) return AWS.Response.Data;

end @_project_@.Simple_WEB_Server.Callbacks;
