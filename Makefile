

include Makefile.config
export HOME=${CURDIR}

export PATH:=$(shell bin/calculatePATH .)
ifeq (${PREFIX},..)
$(error   No GNAT Found)
endif
a:
	echo ${PATH}
info: # IGNORE
	@echo "targets is"
	@echo "   compile"
	@echo "   test"
	@echo "   install"
	@echo "   release"
	@echo "   dist"
	@echo "   upload"


compile: # compile
	gprbuild -p -P ssprep.gpr
	gprbuild -p -P helpers/helpers.gpr


all:compile install  # IGNORE

metric: # Get metrics
	gnat metric -P ssprep.gpr --complexity-all --lines-all  --syntax-all  --coupling-all -d obj/metrics -x -xs -ox obj/metrics/summary.xml src/*.ad?


install: .PHONY # install
	mkdir -p ${DESTDIR}${PREFIX}/bin
	cp bin/deps2symbs  ${DESTDIR}${PREFIX}/bin
	cp bin/getbuildorder ${DESTDIR}${PREFIX}/bin
	cp bin/report-keywords ${DESTDIR}${PREFIX}/bin
	cp bin/ssprep ${DESTDIR}${PREFIX}/bin
	for i in `find share -type d | grep -v .svn` ; do \
		mkdir -p ${DESTDIR}${PREFIX}/$$i; \
        done

	for i in `find share -type f | grep -v .svn` ; do \
		cp  $$i $; ${DESTDIR}${PREFIX}/`dirname $$i`; \
        done

docs: # docs
	echo not implemented
	env | sort
test: .PHONY compile # test
	${MAKE} -C regresion regresion
	${MAKE} -C tests compile
	${MAKE} -C tests test

release: # release
	${MAKE}	compile
	${MAKE}	test
	$(MAKE) dist

regresion: .PHONY  # IGNORE
	${MAKE}	-C regresion regresion

clean:  # IGNORE
	git clean -xdf

TARGET=ssprep-$(shell bin/ssprep --version)

dist:
	${RM} -rf  ${TARGET}
	git clone  ${CURDIR} ${TARGET}
	rm -rf ${TARGET}/.git
	tar -czf   ${TARGET}.tgz ${TARGET}
	#${RM} ${TARGET}

rebuild:
	${MAKE} clean
	${MAKE} compile
	${MAKE} test

.PHONY: # IGNORE
Makefile.config: # IGNOREs
	echo CP=cp -f >$@
	echo RM=rm -rf >>$@
	echo TAR=tar >>$@
	echo GPRBUILD=gprbuild -p >>$@
	echo PREFIX=$(dir $(shell which gnatls)).. >>$@
