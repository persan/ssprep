from os.path import *
from glob import glob
import os
import subprocess
for i in glob("*/*"):
    if isdir(i):
        print i
        subprocess.call(["rm","-rf",i]) 
