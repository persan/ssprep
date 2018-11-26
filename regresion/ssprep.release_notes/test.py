from test_support import *
# from 
run("ssprep",["ssprep.release_notes","--noexec",
                                       "-Dproject=Test0001"])
diff("golden","output")
