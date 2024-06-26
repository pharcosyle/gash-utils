# Gash-Utils
# Copyright © 2017, 2018, 2019 Timothy Sample <samplet@ngyro.com>
# Copyright © 2018, 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
#
# This file is part of Gash-Utils.
#
# Gash-Utils is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Gash-Utils is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Gash-Utils.  If not, see <https://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/guile.am

EXTRA_DIST +=                                   \
  .dir-locals.el                                \
  COPYING.CC0                                   \
  bootstrap                                     \
  build-aux/git-version-gen                     \
  build-aux/gitlab-ci.yml                       \
  build-aux/gitlog-to-changelog                 \
  build-aux/gitlog-to-changelog-repro.patch     \
  doc/awk-syntax.txt                            \
  guix.scm                                      \
  tests/data/README

# Modules and scripts
#####################

SOURCES =                                       \
  gash/commands/awk.scm                         \
  gash/commands/awk/lexer.scm                   \
  gash/commands/awk/parser.scm                  \
  gash/commands/basename.scm                    \
  gash/commands/cat.scm                         \
  gash/commands/chmod.scm                       \
  gash/commands/cmp.scm                         \
  gash/commands/compress.scm                    \
  gash/commands/config.scm                      \
  gash/commands/cp.scm                          \
  gash/commands/cut.scm                         \
  gash/commands/diff.scm                        \
  gash/commands/dirname.scm                     \
  gash/commands/env.scm                         \
  gash/commands/expr.scm                        \
  gash/commands/false.scm                       \
  gash/commands/find.scm                        \
  gash/commands/grep.scm                        \
  gash/commands/head.scm                        \
  gash/commands/ln.scm                          \
  gash/commands/ls.scm                          \
  gash/commands/mkdir.scm                       \
  gash/commands/mv.scm                          \
  gash/commands/printf.scm                      \
  gash/commands/pwd.scm                         \
  gash/commands/reboot.scm                      \
  gash/commands/rm.scm                          \
  gash/commands/rmdir.scm                       \
  gash/commands/sed.scm                         \
  gash/commands/sed/reader.scm                  \
  gash/commands/sleep.scm                       \
  gash/commands/sort.scm                        \
  gash/commands/tar.scm                         \
  gash/commands/test.scm                        \
  gash/commands/testb.scm                       \
  gash/commands/touch.scm                       \
  gash/commands/tr.scm                          \
  gash/commands/true.scm                        \
  gash/commands/uname.scm                       \
  gash/commands/uniq.scm                        \
  gash/commands/wc.scm                          \
  gash/commands/which.scm                       \
  gash/compress.scm                             \
  gash/diff.scm                                 \
  gash/guix-utils.scm                           \
  gash/io.scm                                   \
  gash/lzw.scm                                  \
  gash/shell-utils.scm                          \
  gash/ustar.scm                                \
  gash/util.scm                                 \
  gash-utils/file-formats.scm                   \
  gash-utils/options.scm                        \
  gash-utils/regex.scm

gash/commands/testb.scm: $(srcdir)/gash/commands/test.scm Makefile
	$(AM_V_GEN)sed \
	    -e 's,gash commands test,gash commands testb,g' \
	    -e 's,apply test (cdr,apply test/bracket (cdr,g' \
	    < $< > $@

WRAPPERS =                                      \
  scripts/awk                                   \
  scripts/basename                              \
  scripts/cat                                   \
  scripts/chmod                                 \
  scripts/cmp                                   \
  scripts/compress                              \
  scripts/cp                                    \
  scripts/cut                                   \
  scripts/diff                                  \
  scripts/dirname                               \
  scripts/env                                   \
  scripts/expr                                  \
  scripts/false                                 \
  scripts/find                                  \
  scripts/grep                                  \
  scripts/head                                  \
  scripts/ln                                    \
  scripts/ls                                    \
  scripts/mkdir                                 \
  scripts/mv                                    \
  scripts/printf                                \
  scripts/pwd                                   \
  scripts/reboot                                \
  scripts/rm                                    \
  scripts/rmdir                                 \
  scripts/sed                                   \
  scripts/sleep                                 \
  scripts/sort                                  \
  scripts/tar                                   \
  scripts/test                                  \
  scripts/touch                                 \
  scripts/tr                                    \
  scripts/true                                  \
  scripts/uname                                 \
  scripts/uniq                                  \
  scripts/wc                                    \
  scripts/which

bin_SCRIPTS =                                   \
  $(WRAPPERS)                                   \
  scripts/[                                     \
  scripts/egrep                                 \
  scripts/fgrep

do_subst = $(AM_V_GEN)sed                       \
  -e 's,[@]GUILE[@],$(GUILE),g'                 \
  -e 's,[@]MODDIR[@],$(moddir),g'               \
  -e 's,[@]GODIR[@],$(ccachedir),g'

$(WRAPPERS): $(srcdir)/scripts/template.in Makefile
	$(AM_V_at) mkdir -p scripts
	$(do_subst) -e 's,@UTILITY@,'"$$(basename '$@')"',' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/[: $(srcdir)/scripts/template.in Makefile
	$(AM_V_at) mkdir -p scripts
	$(do_subst) -e 's,@UTILITY@,testb,' < $< > $@
	$(AM_V_at)chmod a+x $@

EXTRA_DIST +=                                   \
  scripts/egrep                                 \
  scripts/fgrep                                 \
  scripts/template.in

CLEANFILES +=                                   \
  $(WRAPPERS)                                   \
  gash/commands/testb.scm                       \
  scripts/[

# Tests
#######

TEST_EXTENSIONS = .scm .org
SCM_LOG_COMPILER = $(top_builddir)/pre-inst-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile
ORG_LOG_COMPILER = $(top_builddir)/pre-inst-env \
                       $(top_builddir)/tests/run-test-suite

UNIT_TESTS =                                    \
  tests/unit/awk-parser.scm                     \
  tests/unit/file-formats.scm                   \
  tests/unit/shell-utils.scm                    \
  tests/unit/sed-reader.scm

FULL_TESTS =                                    \
  tests/awk.org                                 \
  tests/core-utils.org                          \
  tests/grep.org                                \
  tests/printf.org                              \
  tests/sed.org                                 \
  tests/sort.org                                \
  tests/tar.org

TEST_DATA_FILES =                               \
  tests/data/empty                              \
  tests/data/foo                                \
  tests/data/fooRbar                            \
  tests/data/ro.tar                             \
  tests/data/star/0                             \
  tests/data/star/1                             \
  tests/data/star/2                             \
  tests/data/star/3

TESTS = $(UNIT_TESTS) $(FULL_TESTS)

EXTRA_DIST +=                                   \
  $(TESTS)                                      \
  $(TEST_DATA_FILES)                            \
  tests/unit/automake.scm

# Distribution
##############

dist-hook: gen-ChangeLog
	echo $(VERSION) > $(distdir)/.tarball-version
	if test -e .git; then \
	    git ls-tree -r --name-only HEAD \
	        > $(distdir)/.tarball-manifest; \
	    git show HEAD --format=%ct --no-patch 2>/dev/null \
	        > $(distdir)/.tarball-timestamp; \
	else \
	    cp $(srcdir)/.tarball-manifest $(distdir)/.tarball-manifest; \
	    cp $(srcdir)/.tarball-timestamp $(distdir)/.tarball-timestamp; \
	fi

build-aux/gitlog-to-changelog-repro: \
		$(srcdir)/build-aux/gitlog-to-changelog \
		$(srcdir)/build-aux/gitlog-to-changelog-repro.patch
	$(AM_V_GEN){ mkdir -p $$(dirname $@) && \
	             patch -t --output=$@ $? && \
	             chmod a+x $@; }

.PHONY: gen-ChangeLog
gen-ChangeLog: build-aux/gitlog-to-changelog-repro
	$(AM_V_GEN)if test -e .git; then \
	    { ./build-aux/gitlog-to-changelog-repro \
	          --format='%s%n%n%b%n' -- v0.1.0.. && \
	      echo && \
	      sed -n -e '/^Copyright/,$$p' < $(top_srcdir)/ChangeLog; \
	    } > $(distdir)/ChangeLog-t && \
	    { rm -f $(distdir)/ChangeLog && \
	      mv $(distdir)/ChangeLog-t $(distdir)/ChangeLog; } \
	fi

# Reproducible tarball
# Be friendly to Debian; avoid using EPOCH
override GZIP_ENV = "--best --no-name"
am__tar = $${TAR-tar} -chof - --sort=name \
  --mtime=@$$(cat "$$tardir"/.tarball-timestamp) \
  --owner=0 --group=0 --numeric-owner \
  --mode=go=rX,u+rw,a-s "$$tardir"

distcheck-hook:
	set -e;                                 \
	manifest=$(distdir)/.tarball-manifest;  \
	test -f "$$manifest";                   \
	for x in `cat "$$manifest"`;            \
	do                                      \
	    if ! test -f $(distdir)/"$$x"       \
                && ! test "$$x" = .gitignore;   \
	    then                                \
	        echo "Missing: $$x";            \
	        exit 1;                         \
	    fi;                                 \
	done

DISTCLEANFILES = build-aux/gitlog-to-changelog-repro
