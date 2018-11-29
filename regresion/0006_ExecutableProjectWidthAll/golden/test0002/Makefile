
include Makefile.configuration


# Default target
compile:

################################################################

pack:
	rm -rf  ${PROJECT}-${VERSION}*
	svn export . ${PROJECT}-${VERSION}
	tar -czf  ${PROJECT}-${VERSION}-${DATE}.tgz ${PROJECT}-${VERSION}}
	${RM} ${PROJECT}-${VERSION}

################################################################
clean:
	${MAKE} -C tools $@
	rm -rf .obj


################################################################
all:
	${MAKE} -C compile
	${MAKE} -C tests
	${MAKE} -C install


################################################################
compile:
	gprbuild -p -P test0002.gpr
	gprbuild -p -P helpers/helpers.gpr
	${MAKE} -C tests $@

################################################################
install:
	${MKDIR} -p ${INCLUDEDIR}

	cp  src/*.ad? ${INCLUDEDIR}
	${CHMOD} a-w ${INCLUDEDIR}/*.ad?

	${MKDIR} -p ${LIBDIR}

	${MKDIR} -p ${LIBDIR}/gnat
	${CP} test0002_.gpr ${LIBDIR}/gnat/test0002.gpr

	${MAKE} -C tools $@

################################################################
configure:

test:

	${MAKE} -C tests $@
	${MAKE} -C regresion $@



release:
	 svn stat        .        >.obj/svn-stat.txt
	 svn --xml info  .       >.obj/svn-info.txt
	 svn ls ${SVNRoot}/tags >.obj/releases.txt
	./helpers/checkrelease --dirtylist=.obj/svn-stat.txt --svnversions=.obj/releases.txt
	./helpers/checkrelease --releasenote=release_notes.txt

Makefile.configuration:
	echo "PREFIX=$(dir $(shell which gnatls))..">$@
	echo OBJECT_DIR=.obj >>$@
	echo PROJECT=test0002  >>$@
	echo export LIBDIR=${PREFIX}/lib/test0002  >>$@
	echo export INCLUDEDIR=${PREFIX}/include/test0002  >>$@
