from test_support import *
import os

# from


run("ssprep",["ssprep.simpleLibraryProject","-Dproject=Test0001"])
os.remove("output/Test0001/src/test0001-version.ads")
diff("golden/Test0001","output/Test0001")

