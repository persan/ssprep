from test_support import *
# from 
run("ssprep",["ssprep.simpleLibraryProject","--noexec",
                                              "-Dproject=Test0001"])
os.system("rm output/Test0001/src/test0001-version.ads")
diff("golden","output")
