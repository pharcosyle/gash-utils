#! /bin/sh

# Gash --- Guile As SHell
# Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of Gash.
#
# Gash is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Gash is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Gash.  If not, see <http://www.gnu.org/licenses/>.

if [ -n "$BUILD_DEBUG" ]; then
    set -x
fi

export GUILE
export GUILE_AUTO_COMPILE
export GUILE_LOAD_PATH
export GUILE_LOAD_COMPILED_PATH

GUILE_LOAD_PATH=$HOME/src/geesh:$GUILE_LOAD_PATH
GUILE_LOAD_COMPILED_PATH=$HOME/src/geesh:$GUILE_LOAD_COMPILED_PATH

GUILE_LOAD_PATH=$(pwd):$GUILE_LOAD_PATH
GUILE_LOAD_COMPILED_PATH=$(pwd):$GUILE_LOAD_COMPILED_PATH
GUILE=${GUILE-$(command -v guile)}
GUILE_TOOLS=${GUILE_TOOLS-$(command -v guile-tools)}
GUILE_AUTO_COMPILE=0

set -e

SCM_FILES="
gash/bournish-commands.scm
gash/guix-build-utils.scm
gash/builtins.scm
gash/config.scm
gash/environment.scm
gash/geesh.scm
gash/gash.scm
gash/io.scm
gash/job.scm
gash/peg.scm
gash/pipe.scm
gash/script.scm
gash/ustar.scm
gash/util.scm
"

export srcdir=.
export host=$($GUILE -c "(display %host-type)")

for i in $SCM_FILES; do
    go=${i%%.scm}.go
    if [ $i -nt $go ]; then
        echo "  GUILEC $i"
        $GUILE_TOOLS compile -L bin -L gash -o $go $i
    fi
done

SCRIPTS="
bin/cat
bin/cp
bin/find
bin/gash
bin/grep
bin/ls
bin/reboot
bin/tar
bin/wc
bin/which
"

for i in $SCRIPTS; do
    go=${i%%.scm}.go
    if [ $i -nt $go ]; then
        echo "  GUILEC $i"
        $GUILE_TOOLS compile -L guile -L scripts -o $go $i
    fi
done
