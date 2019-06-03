AC_INIT([Geesh],
        m4_esyscmd([build-aux/git-version-gen .tarball-version]))
AC_CONFIG_SRCDIR([geesh])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests silent-rules -Wall -Werror foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([2.2])
GUILE_PROGS

AC_PATH_PROG([COMPRESS], [compress])

AC_PATH_PROG([GZIP], [gzip])
if test x"$GZIP" = x ; then
   AC_MSG_ERROR([could not find gzip])
fi

AC_PATH_PROG([BZIP2], [bzip2])
if test x"$BZIP2" = x ; then
   AC_MSG_ERROR([could not find bzip2])
fi

AC_PATH_PROG([XZ], [xz])
if test x"$XZ" = x ; then
   AC_MSG_ERROR([could not find xz])
fi

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([gash/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([tests/unit/config.scm])
AC_CONFIG_FILES([tests/spec/Makefile])

AC_OUTPUT
