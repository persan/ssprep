ifndef (PREFIX)
PREFIX=$(dir $(shell which gnatls))..
endif
INCLUDEDIR=${PREFIX}/include/coco3d
LIBDIR=${PREFIX}/lib

# Default target
compile::

################################################################

all::
	${MAKE} compile
	${MAKE} install


################################################################
compile::
	gprbuild -p -P coco3d.gpr
	gprbuild -p -P coco3d.gpr -XLIBRARY_TYPE=relocatable

################################################################
install::
	mkdir -p ${INCLUDEDIR}
	mkdir -p ${LIBDIR}/gnat

	cp -f src/coco*.ad? ${INCLUDEDIR}
	cp -f src/geodetic_calculations/coco*.ad? ${INCLUDEDIR}
	cp -rf lib      ${LIBDIR}/coco3d
	cp coco3d__.gpr  ${LIBDIR}/gnat/coco3d.gpr

	chmod a-w ${INCLUDEDIR}/*.ad?





################################################################
configure::
	@echo ${@}

test::
	${MAKE} -C tests $@


