from test_support import *
# from 
run("ssprep",["ssprep.GNU_Header","--noexec",
                                    "-Dproject=Test0001"])
diff("golden","output")
