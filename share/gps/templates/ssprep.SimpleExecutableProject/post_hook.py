# import GPS
import os
from os.path import *
import glob
os.system("chmod +x doit.sh")
os.system("./doit.sh")
# os.system("rm doit.sh")
f=file("project_name")
name=f.read().strip()
f.close()
project_file=join(name,name + ".gpr")
print project_file

if exists(project_file):
   GPS.Project.load(project_file)


