############################################################################
#                              Ada Web Server                              #
#                                                                          #
#                     Copyright (C) 2003-2008, AdaCore                     #
#                                                                          #
#  This library is free software; you can redistribute it and/or modify    #
#  it under the terms of the GNU General Public License as published by    #
#  the Free Software Foundation; either version 2 of the License, or (at   #
#  your option) any later version.                                         #
#                                                                          #
#  This library is distributed in the hope that it will be useful, but     #
#  WITHOUT ANY WARRANTY; without even the implied warranty of              #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       #
#  General Public License for more details.                                #
#                                                                          #
#  You should have received a copy of the GNU General Public License       #
#  along with this library; if not, write to the Free Software Foundation, #
#  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          #
#                                                                          #
#  As a special exception, if other files instantiate generics from this   #
#  unit, or you link this unit with other files to produce an executable,  #
#  this  unit  does not  by itself cause  the resulting executable to be   #
#  covered by the GNU General Public License. This exception does not      #
#  however invalidate any other reasons why the executable file  might be  #
#  covered by the  GNU Public License.                                     #
############################################################################

.SILENT:

#  To run the testsuite:
#
#     $ ./testsuite.py
#
#  To run some specific tests only:
#
#     $ ./testsuite.py --tests='0001_turl 0009_once'

ifeq (${OS}, Windows_NT)
  EXEEXT = .exe
  RLIMIT_SRC = support/rlimit-NT.c
else
  RLIMIT_SRC = support/rlimit.c
endif

all regresion:rlimit$(EXEEXT)
	./testsuite.py

rlimit$(EXEEXT): $(RLIMIT_SRC)
	gcc -o rlimit$(EXEEXT) $(RLIMIT_SRC)

install:

clean:
	-$(RM) -f testsuite.tags testsuite.res testsuite.duration_* *.pyc testsuite.res testsuite.diff
	-$(RM) -fr .outputs .build rlimit$(EXEEXT) $(wildcard */output)



%: