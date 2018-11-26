from test_support import *
# from 
run("ssprep",["ssprep.INSTALL","--noexec",
                                 "-Dproject=Test0001"])
diff("golden","output")
