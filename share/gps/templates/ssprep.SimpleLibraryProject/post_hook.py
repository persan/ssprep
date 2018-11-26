# import GPS
import os
from os.path import *
import glob


os.system("chmod +x run_ssprep.sh")
os.system("./run_ssprep.sh")


f=file("project_name")
name=f.read().strip()
f.close()

os.system("rm run_ssprep.sh project_name")
project_file=join(name,name + ".gpr")
if exists(project_file):
   GPS.Project.load(project_file)


