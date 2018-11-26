tmplt="""from test_support import *
# from 
run("ssprep",['"%s"',"--noexec"
                     "-Dproject=Test0001"])
diff("golden","output")
"""

import subprocess 
import os.path
import os
import re
excludes=["ssprep.documentationProject"]
M=re.compile("^(.*)=>.*")
p1 = subprocess.Popen(["ssprep","--dump"], stdout=subprocess.PIPE).stdout
while True:
    line=p1.readline()
    if not line:
        break
    m=M.match(line.decode())
    if m:
        name=m.group(1).strip()
        if not name in excludes:
            if not os.path.exists(name):
                os.makedirs(name)
                p=os.path.join(name,"test.py")
                f=open(p,"w")
                f.write(tmplt % name)
                f.close()


    
    
    
