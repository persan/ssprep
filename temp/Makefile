export PATH:=${CURDIR}/../bin:${PATH}
all:
	rm -rf testGen
	ssprep ssprep.simpleExecutableProject -Dproject=testGen
	${MAKE} -C testGen compile

	rm -rf testGenASIS
	ssprep ssprep.simpleExecutableProject -Dproject=testGenASIS -dWITH_ASIS=True
	${MAKE} -C testGenASIS compile

	rm -rf testGenAWS
	ssprep ssprep.simpleExecutableProject -Dproject=testGenAWS -dWITH_AWS=True
	${MAKE} -C testGenAWS compile

	rm -rf testGenXML
	ssprep ssprep.simpleExecutableProject -Dproject=testGenXML -dWITH_XMLADA=True
	${MAKE} -C testGenXML compile


