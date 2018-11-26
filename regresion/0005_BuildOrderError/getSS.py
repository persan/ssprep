import glob
from os.path import *
SVN="http://m-svnmirror/svn/ninelv/"
for i in glob.glob(join("sdk_gcc","hudson","*.txt")):
    for line in file(i):
        if "${SVN_CORP}" in line:
            print "svn co " + line.strip().replace("${SVN_CORP}",SVN)
            
