# Gash-Utils
# Copyright © 2017, 2018, 2019 Timothy Sample <samplet@ngyro.com>
# Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of Gash-Utils.
#
# Gash-Utils is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Gash-Utils is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Gash-Utils.  If not, see <http://www.gnu.org/licenses/>.

AC_INIT([Gash-Utils],
        m4_esyscmd([build-aux/git-version-gen .tarball-version]),
        [gash-devel@nongnu.org],
        [gash-utils])
AC_CONFIG_SRCDIR([gash])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests silent-rules -Wall -Werror foreign])
AM_SILENT_RULES([yes])

GUILE_PKG([2.2])
GUILE_PROGS

AC_ARG_VAR([GUILD], [guild (Guile compiler) command])
AS_IF([test "x$GUILD" = "x"],
  [PKG_CHECK_VAR([GUILD], [guile-$GUILE_EFFECTIVE_VERSION], [guild], [],
    [AC_MSG_ERROR(m4_normalize([
      'guild' binary not found; please check your Guile installation.]))])])

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
AC_CONFIG_FILES([gash/commands/config.scm])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
AC_CONFIG_FILES([tests/run-test-suite], [chmod +x tests/run-test-suite])

AC_OUTPUT
