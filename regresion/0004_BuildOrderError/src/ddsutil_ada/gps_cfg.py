import inspect
import os
import string


def thisDir():
    return os.path.dirname(inspect.getsourcefile(thisDir))


class ProjectConfigurator:
    def __init__(self,Menues_to_hide, Contextuals_to_hide):
        import GPS
        import os
        self.Menues_to_hide = []
        self.Contextuals_to_hide = []
        for i in Menues_to_hide:
            m=GPS.Menu.get(i)
            m.set_sensitive(False)
            self.Menues_to_hide.append(m)

        for i in Contextuals_to_hide:
            m=GPS.Contextual(i)
            m.hide()
            self.Contextuals_to_hide.append(m)
        self.ADA_PROJECT_PATH=os.getenv("ADA_PROJECT_PATH")
        if self.ADA_PROJECT_PATH:
            ADA_PROJECT_PATH=self.ADA_PROJECT_PATH.split(os.pathsep)
        else:
            ADA_PROJECT_PATH=[]
            self.ADA_PROJECT_PATH=""
        ADA_PROJECT_PATH.insert(0,r"C:\GNATPRO\NDDSAda-4.4d")
        ADA_PROJECT_PATH=string.join(ADA_PROJECT_PATH,os.pathsep)
        os.putenv("ADA_PROJECT_PATH",ADA_PROJECT_PATH)
        GPS.Hook("project_changing").add(self.onUnload)
        GPS.parse_xml("""<case_exceptions>
          <word>com</word>
          <word>saabgroup</word>
          <substring>cms</substring>
          <substring>pha</substring>
          <substring>DDS</substring>
       </case_exceptions>""")

    def onUnload(self,hook,project):
        import GPS
        import os
        for i in self.Menues_to_hide:
            i.set_sensitive(True)
        for i in self.Contextuals_to_hide:
            i.show()
        if self.ADA_PROJECT_PATH:
            os.putenv("ADA_PROJECT_PATH",self.ADA_PROJECT_PATH)

ProjectConfigurator(["/Project/Edit Project Properties",
                     "/Project/Edit File Switches",
                     "/Project/Save All"],
                    ["Edit project properties",
                      "Project dependencies",
                      "Add configuration variable",
                      "Edit file switches",
                      "Add to extending project"])




