from test_support import *
import inspect
from os.path import *
# from


run("make",["-C", "..","all"])
diff("golden.txt","result.txt")
