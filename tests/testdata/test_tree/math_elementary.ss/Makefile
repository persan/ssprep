#  -*- Makefile -*-


all::
	gnatmake -Pmath_elementary.gpr
	gnatmake -Pprogram/math_elementary-program.gpr

installer::
	ISCC.exe math_elementary.iss

install::

clean::
	gnatclean -P-Pprogram/math_elementary-program.gpr
	gnatclean -Pmath_elementary.gpr

all clean gcov test::
	${MAKE} -C tests $@

