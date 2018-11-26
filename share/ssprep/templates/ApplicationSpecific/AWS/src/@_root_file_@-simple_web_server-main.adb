@@INCLUDE@@ ../../../Common/FileHeader.txt

--  The famous Hello Word demo, using AWS framework.

with Ada.Text_IO;

with AWS.Default;
with AWS.Server;

with @_project_@.Simple_WEB_Server.Callbacks;

procedure @_project_@.Simple_WEB_Server.Main is

   WS : AWS.Server.HTTP;

begin
   Ada.Text_IO.Put_Line
     ("Call me on port"
      & Positive'Image (AWS.Default.Server_Port)
      & ", I will stop in 60 seconds...");

   AWS.Server.Start (WS, "Hello World",
                     Max_Connection => 1,
                     Callback       => Callbacks.HW_CB'Access);

   delay 60.0;

   AWS.Server.Shutdown (WS);
end @_project_@.Simple_WEB_Server.Main;
