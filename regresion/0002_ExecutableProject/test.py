from test_support import *
# from
run("ssprep",["ssprep.simpleExecutableProject","-Dproject=test0002"])
os.remove("output/test0002/src/test0002-version.ads")
diff("golden","output")
