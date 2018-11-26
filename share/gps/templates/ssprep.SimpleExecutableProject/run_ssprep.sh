#!/bin/sh

ssprep ssprep.simpleExecutableProject -Dproject=@_project_name_@\
                                      -dWITH_ASIS=@_With_Asis_@\
                                      -dWITH_AWS=@_With_Aws_@\
                                      -dWITH_XMLADA=@_With_Xmlada_@
echo @_project_name_@ >project_name

