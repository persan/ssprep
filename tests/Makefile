
compile:
	gprbuild -p -P ssprep-tests.gpr

install:
ifeq (${MAKELEVEL},0)
	${MAKE} -C .. install
else
endif

docs:
	${MAKE} -C .. docs



test:
ifeq ("${MAKELEVEL}","0")
	${MAKE}	-C .. ${@}
else
	./bin/ssprep-tests-testharnesses-test_all 2>&1 |  tee test-results.xml
endif

release: # release
	${MAKE}	-C .. release

regresion: # regresion
ifeq ("${MAKELEVEL}","0")
	${MAKE}	-C .. ${@}
else
endif
	${MAKE}	-C .. regresion

clean:  # IGNORE
	${MAKE}	-C .. ${@}


