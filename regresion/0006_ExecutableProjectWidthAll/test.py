#!/usr/bin/env python

import sys
from os.path import *
sys.path.append(dirname(dirname(abspath(__file__))))

from test_support import *


run("ssprep",["ssprep.simpleExecutableProject","-Dproject=test0002","-dWITH_ASIS=True","-dWITH_XMLAda=True","-dWITH_AWS=True"])
exec_cmd("make",["-C", "output/test0002" ,"compile"])
exec_cmd("output/test0002/bin/test0002-main",["--version"],"output/version")
diff("golden/version","output/version")


