from test_support import *
# from
run("ssprep",["ssprep.AUnitTests","-Droot=hej",
                                  "-dparent=Gnat_Default",
                                  "-dparent_path=gnat_default.gpr"])
diff("golden/tests","output/tests")
