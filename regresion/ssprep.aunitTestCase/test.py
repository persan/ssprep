from test_support import *
# from
run("ssprep",["ssprep.aunitTestCase","-Dtest_case_package=test" ,
                                     "-dTEST_CASE_TYPE=Test_Case",
                                     "-dOVERRIDE_SET_UP=True",
                                     "-dOVERRIDE_TEAR_DOWN=True",
                                     "-dTEST_CASE_DESCRIPTION=A_Good_Test",
                                     "--noexec"
                                       ])
diff("golden","output")
