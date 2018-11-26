from test_support import *
# from 
run("ssprep",["ssprep.simpleExecutableProject","--noexec",
                                               "-Dproject=test0002"])
os.system("rm output/test0002/src/test0002-version.ads")
diff("golden","output")
