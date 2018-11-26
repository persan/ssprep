from test_support import *
# from
run("ssprep",["ssprep.aunitTestHarness","-DHARNESS_NAME=TestHarness",
                                        "-DHARNESS_TEST_SUITE_PACKAGE=TestSuit",
                                        "-dHARNESS_TEST_SUITE=Suite",
                                        "--noexec"])
diff("golden","output")
