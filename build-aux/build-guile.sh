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
# Gash is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License
# along with Gash.  If not, see <http://www.gnu.org/licenses/>.

srcdir=${srcdir-.}
. ${srcdest}build-aux/trace.sh

export GUILE
export GUILE_AUTO_COMPILE
GUILE=${GUILE-$(command -v guile)}
GUILE_TOOLS=${GUILE_TOOLS-$(command -v guile-tools)}
GUILE_AUTO_COMPILE=0

set -e

SCM_FILES="
${srcdest}gash/bournish-commands.scm
${srcdest}gash/guix-utils.scm
${srcdest}gash/builtins.scm
${srcdest}gash/compress.scm
${srcdest}gash/config.scm
${srcdest}gash/environment.scm
${srcdest}gash/geesh.scm
${srcdest}gash/gash.scm
${srcdest}gash/io.scm
${srcdest}gash/job.scm
${srcdest}gash/lzw.scm
${srcdest}gash/peg.scm
${srcdest}gash/pipe.scm
${srcdest}gash/readline.scm
${srcdest}gash/script.scm
${srcdest}gash/shell-utils.scm
${srcdest}gash/ustar.scm
${srcdest}gash/util.scm

${srcdest}gash/commands/cat.scm
${srcdest}gash/commands/compress.scm
${srcdest}gash/commands/cp.scm
${srcdest}gash/commands/find.scm
${srcdest}gash/commands/grep.scm
${srcdest}gash/commands/ls.scm
${srcdest}gash/commands/reboot.scm
${srcdest}gash/commands/rm.scm
${srcdest}gash/commands/sed.scm
${srcdest}gash/commands/tar.scm
${srcdest}gash/commands/wc.scm
${srcdest}gash/commands/which.scm

"

SCRIPTS="
${srcdest}bin/cat
${srcdest}bin/compress
${srcdest}bin/cp
${srcdest}bin/find
${srcdest}bin/gash
${srcdest}bin/grep
${srcdest}bin/ls
${srcdest}bin/reboot
${srcdest}bin/rm
${srcdest}bin/sed
${srcdest}bin/tar
${srcdest}bin/wc
${srcdest}bin/which
"

export host=$($GUILE -c "(display %host-type)")

abs=$srcdest
if [ "$GUILE_EFFECTIVE_VERSION" = "2.0" ]; then
    srcdest=$abs_top_srcdir/
fi

GUILE_AUTO_COMPILE=0
WARNINGS="
--warn=unsupported-warning
--warn=unused-variable
--warn=unused-toplevel
--warn=unbound-variable
--warn=macro-use-before-definition
--warn=arity-mismatch
--warn=duplicate-case-datum
--warn=bad-case-datum
--warn=format
"

for i in $SCM_FILES $SCRIPTS; do
    b=$(basename $i)
    go=${i%%.scm}.go
    if [ $i -nt $go ]; then
        trace "GUILEC     $b" $GUILE_TOOLS compile -L ${srcdir} $WARNINGS -o $go $i
    fi
done
