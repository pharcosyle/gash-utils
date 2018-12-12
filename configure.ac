AC_INIT([Geesh],
        m4_esyscmd([build-aux/git-version-gen .tarball-version]))
AC_CONFIG_SRCDIR([geesh])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests silent-rules -Wall -Werror foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([2.2])
GUILE_PROGS

AC_PATH_PROG([GENHTML], [genhtml])
AM_CONDITIONAL([HAVE_GENHTML], [test -n $GENHTML])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([tests/unit/config.scm])
AC_CONFIG_FILES([tests/spec/Makefile])
AC_CONFIG_FILES([tools/coverage], [chmod +x tools/coverage])

AC_OUTPUT
