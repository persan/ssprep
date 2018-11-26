
export ADA_PROJECT_PATH=${CURDIR}/
SVNDB=file://${CURDIR}/DB
export HOME=${CURDIR}
WORKSPACE=${CURDIR}/output/ws
ROOTPROJ=$SVNDB/components/c001/trunk/c001_code.ss
all:
	mkdir -p .ssprep
	cat  ssfetch-file.conf | sed "s-SVNDB-$SVNDB-" >.ssprep/ssfetch-file.conf
	ssfetch  --workspace=${WORKSPACE}  ${ROOTPROJ}

	echo rm -rf .ssprep
	echo rm -rf `find  ${ROOTPROJ} -name .svn`

