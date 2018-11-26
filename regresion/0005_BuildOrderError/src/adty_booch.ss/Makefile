RESULT_DIR=.

MKDIR=mkdir -p
CP=cp -rf
RM=rm -rf

SS:=$(strip $(basename $(notdir ${CURDIR})))
PREFIX=${RESULT_DIR}

.PHONY:

build:.PHONY
	gprbuild -p -P ${SS}.gpr -XBUILD=release -XLIBRARY_TYPE=relocateble
	gprbuild -p -P ${SS}.gpr -XBUILD=release -XLIBRARY_TYPE=static
	gprbuild -p -P ${SS}.gpr -XBUILD=debug   -XLIBRARY_TYPE=relocateble
	gprbuild -p -P ${SS}.gpr -XBUILD=debug   -XLIBRARY_TYPE=static
test:.PHONY
	#${MAKE} -C test

install:.PHONY
	${MKDIR} ${PREFIX}/${SS}.ss
	${CP}    lib-obj/lib       ${PREFIX}/${SS}.ss
	${CP}    lib-obj/include   ${PREFIX}/${SS}.ss
	${CP}    ${SS}_.gpr        $(PREFIX)/${SS}.ss/${SS}.gpr
	chmod a-w -R  ${PREFIX}/${SS}.ss

clean:.PHONY
	${RM} ${PREFIX}/${SS}.ss
	${RM} lib-obj

all: build tests install

rebuild: clean all
