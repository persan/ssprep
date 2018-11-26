from test_support import *
# from
run("ssprep",["ssprep.aunitTestSuite","--noexec",
                                      "-DTEST_SUITE_PACKAGE=TestSuite",
                                      "-DTEST_SUITE_PACKAGES=(ATestcase)",
                                      "-DTEST_SUITE_TESTS=(Test_Case)",
                                      "-dTEST_SUITE_NAME=Suite"])
diff("golden","output")
