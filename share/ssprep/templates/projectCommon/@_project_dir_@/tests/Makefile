
include ../Makefile.configuration


# Default target
compile:

################################################################

test:
	./bin/@_project_file_@-tests-testharnesses-test_all
################################################################
all:
	${MAKE} -C compile
	${MAKE} -C tests


################################################################
compile:
	gprbuild -p -P @_project_file_@-tests.gpr


