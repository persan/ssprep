import os
from os.path import *
import string
import inspect

def thisDir():
    return dirname(inspect.getsourcefile(thisDir))

ADA_PROJECT_PATH=os.getenv("ADA_PROJECT_PATH")
if ADA_PROJECT_PATH:
    ADA_PROJECT_PATH=ADA_PROJECT_PATH.split(os.pathsep)
else:
    ADA_PROJECT_PATH=[]

ADA_PROJECT_PATH.insert(0,join(thisDir(),"src"))
print
os.environ["ADA_PROJECT_PATH"]=string.join(ADA_PROJECT_PATH,os.pathsep)
print "ADA_PROJECT_PATH="+string.join(ADA_PROJECT_PATH,os.pathsep)

