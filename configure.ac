AC_INIT(omp, 1.6.2, [], [], forge.ocamlcore.org/projects/omp)
AC_REVISION([$Revision: 1.0 $])

VERSION=$PACKAGE_VERSION
AC_SUBST(VERSION)
AC_MSG_RESULT([configuring $PACKAGE_STRING])

# look for ocamlopt in the path; if not found, punt
AC_CHECK_PROG(OCAMLOPT,ocamlopt,`which ocamlopt`,no)
if test "$OCAMLOPT" = no ; then
    AC_MSG_ERROR(Cannot find ocamlopt.)
fi

# get directory name
OCAMLBIN=`dirname $OCAMLOPT`

MACHINE=`uname -m`
AC_SUBST(MACHINE)

# get version number and library path
# TODO: if fails to compile on an older version, enforce version number
OCAMLVERSION=`$OCAMLOPT -v | sed -n -e 's|.*version* *\(.*\)$|\1|p' `
echo "ocaml version is $OCAMLVERSION"
OCAMLLIB=`$OCAMLOPT -v | tail -1 | cut -f 4 -d " "`
echo "ocaml library path is $OCAMLLIB"

# check for otk
AC_CHECK_FILE("$OCAMLLIB/otk", found_otk=yes, found_otk=no)
if test "$found_otk" = no ; then
    AC_MSG_ERROR(Cannot find otk library.)
fi

# check for ocamlopt.opt
AC_PATH_PROG(OCAMLOPTDOTOPT,ocamlopt.opt,no)
if test "$OCAMLOPTDOTOPT" != no ; then
    OCAMLOPT=$OCAMLOPTDOTOPT
fi

AC_PROG_INSTALL
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
