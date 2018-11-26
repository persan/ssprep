

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
	gnatmake -p -P ssprep.gpr
	gnatmake -p -P helpers/helpers.gpr


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
test: .PHONY # test
	${MAKE} -C regresion regresion
	${MAKE} -C tests compile
	${MAKE} -C tests test

release: # release
	${MAKE}	compile
	${MAKE}	test
	svn stat       .       >.obj/svn-stat.txt
	svn info       .       >.obj/svn-info.txt
	svn ls ${SVNROOT}/tags >.obj/releases.txt
	./helpers/checkrelease .obj/svn-stat.txt .obj/releases.txt README
	$(MAKE) dist
	svn cp ${SVNROOT}/trunk ${SVNROOT}/tags/$(shell bin/ssprep --version) "-mRelease $(shell bin/ssprep --version)"
	$(MAKE) upload

regresion: .PHONY  # IGNORE
	${MAKE}	-C regresion regresion

clean:  # IGNORE
	rm -rf bin .obj
	${MAKE} -C regresion $@
	${MAKE} -C tests $@
dist:
	${RM} ssprep-$(shell bin/ssprep --version) -f
	svn export . ssprep-$(shell bin/ssprep --version)
	tar -czf     ssprep-$(shell bin/ssprep --version).tgz ssprep-$(shell bin/ssprep --version)
	${RM} ssprep-$(shell bin/ssprep --version)

rebuild:
	${MAKE} clean
	${MAKE} compile
	${MAKE} test

.PHONY: # IGNORE

upload:
	echo mkdir  /home/frs/project/s/ss/ssprep/ssprep/$(shell bin/ssprep --version) >cmd
	echo cd /home/frs/project/s/ss/ssprep/ssprep/$(shell bin/ssprep --version) >>cmd
	echo put ssprep-$(shell bin/ssprep --version).tgz  >>cmd
	echo put README README.txt >>cmd
	echo exit                  >>cmd
	sftp ${SourceForgeUser},ssprep@frs.sourceforge.net <cmd
	rm cmd


rpm:
	echo %_topdir ${CURDIR}/RPMbuild >.rpmmacros
	mkdir -p ${CURDIR}/RPMbuild/{BUILD,RPMS,S{OURCE,PEC,RPM}S}
	rpmbuild -tb ssprep-$(shell bin/ssprep --version).tgz



Makefile.config:
	echo CP=cp -f >$@
	echo RM=rm -rf >>$@
	echo TAR=tar >>$@
	echo GPRBUILD=gnatmake -p >>$@
	echo PREFIX=$(dir $(shell which gnatls)).. >>$@
	echo SVNROOT=https://ssprep.svn.sourceforge.net/svnroot/ssprep >>$@
	echo SourceForgeUser=sombody >>$@



		
