include $(top_srcdir)/build-aux/guile.am

# Modules and scripts
#####################

SOURCES =                                       \
  gash/bournish-commands.scm                    \
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
  gash/util.scm

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
  -e 's,[@]CCACHEDIR[@],$(ccachedir),g'

$(WRAPPERS): $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,'"$$(basename '$@')"',' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/[: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,testb,' < $< > $@
	$(AM_V_at)chmod a+x $@

# Tests
#######

TEST_EXTENSIONS = .scm .org
SCM_LOG_COMPILER = $(top_builddir)/pre-inst-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile
ORG_LOG_COMPILER = $(top_builddir)/pre-inst-env \
                       $(top_builddir)/tests/run-test-suite

.PHONY: check-bootstrap
check-bootstrap:
	guix build -f tests/bootstrap/bash-without-bash.scm

UNIT_TESTS =                                    \
  tests/unit/shell-utils.scm

FULL_TESTS =                                    \
  tests/awk.org                                 \
  tests/core-utils.org                          \
  tests/sed.org                                 \
  tests/tar.org

TESTS = $(UNIT_TESTS) $(FULL_TESTS)

# Distribution
##############

dist-hook:
	echo $(VERSION) > $(distdir)/.tarball-version
