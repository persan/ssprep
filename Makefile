

include Makefile.config
export HOME=${CURDIR}

ifeq (${PREFIX},..)
$(error   No GNAT Found)
endif

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
	gprinstall -f -p -P ssprep.gpr --prefix=${PREFIX}

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
	git clean . -xdf

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
	@echo "export CP=cp -f" >$@
	@echo "export PATH:=${PATH}" >>$@
	@echo "export RM=rm -rf" >>$@
	@echo "export TAR=tar" >>$@
	@echo "export GPRBUILD=gprbuild -p" >>$@
	@echo "export PREFIX=$(dir $(shell dirname $(shell which gnatls)))" >>$@
