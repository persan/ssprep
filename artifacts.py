import os
from os.path import *
import sys
_files = []
_dirs = []

for root, dirs, files in os.walk("share"):
    for f in files:
        path = join(root, f)
        if ("#" not in f) and ("~" not in f):
            _files.append(path)
            if root not in _dirs:
                _dirs.append(root)

buffer = []
for _dir in _dirs:
    buffer.append("""      for Required_Artifacts ("%s") use""" % _dir)
    __files=[]
    for f in _files:
        if dirname(f) == _dir:
            __files.append('"%s"' % f)
    buffer.append("        (" + (",\n         ".join(__files)) + ");\n")

with open("ssprep.gpr") as inf:
    _buffer = inf.read().split("\n")

outBuffer = []
state = 0

for line in _buffer:
    if ("begin" in line) and ("Required_Artifacts" in line):
        outBuffer.append(line)
        for line in buffer:
            outBuffer.append(line)
        state=1
    elif ("end" in line) and  ("Required_Artifacts" in line):
        outBuffer.append(line)
        state=0
    elif state == 0:
        outBuffer.append(line)
with open("ssprep.gpr", "w") as outf:
    outf.write("\n".join(outBuffer))
