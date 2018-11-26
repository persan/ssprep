from test_support import *
# from
run("ssprep",["ssprep.projectCommon","--noexec",
                                     "-Dproject=Test0001"])
diff("golden","output")
