
include ../Makefile.configuration


# Default target
compile:

################################################################

test:
	./bin/test0002-tests-testharnesses-test_all
################################################################
all:
	${MAKE} -C compile
	${MAKE} -C tests


################################################################
compile:
	gprbuild -p -P test0002-tests.gpr

