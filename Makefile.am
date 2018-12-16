moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/ccache

GOBJECTS = $(MODULES:%.scm=%.go)

nobase_mod_DATA = $(MODULES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_nodist_guileobjectDATA
$(guile_install_go_files): install-nobase_dist_guilemoduleDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env \
	    $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -o "$@" "$<"

TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER = $(top_builddir)/pre-inst-env $(GUILE)
AM_SCM_LOG_FLAGS = --no-auto-compile

if HAVE_GENHTML

lcov.info: all
	$(top_builddir)/pre-inst-env $(top_builddir)/tools/coverage

coverage: lcov.info
	$(GENHTML) -o $(top_builddir)/coverage lcov.info

endif # HAVE_GENHTML

test-list: ; @echo $(TESTS)

.PHONY: check-spec
check-spec:
	$(MAKE) $(AM_MAKEFLAGS) -L -C tests/spec check

.PHONY: check-bootstrap
check-bootstrap:
	guix build -f tests/bootstrap/bash-without-bash.scm

.PHONY: check-gash
check-gash:
	SHELL=gash $(top_builddir)/pre-inst-env $(top_builddir)/check.sh

dist-hook:
	echo $(VERSION) > $(distdir)/.tarball-version

MODULES =                                       \
  gash/bournish-commands.scm                    \
  gash/builtins.scm                             \
  gash/commands/basename.scm                    \
  gash/commands/cat.scm                         \
  gash/commands/chmod.scm                       \
  gash/commands/compress.scm                    \
  gash/commands/cp.scm                          \
  gash/commands/dirname.scm                     \
  gash/commands/find.scm                        \
  gash/commands/grep.scm                        \
  gash/commands/ls.scm                          \
  gash/commands/mkdir.scm                       \
  gash/commands/mv.scm                          \
  gash/commands/reboot.scm                      \
  gash/commands/rm.scm                          \
  gash/commands/rmdir.scm                       \
  gash/commands/sed.scm                         \
  gash/commands/sed/reader.scm                  \
  gash/commands/tar.scm                         \
  gash/commands/touch.scm                       \
  gash/commands/tr.scm                          \
  gash/commands/wc.scm                          \
  gash/commands/which.scm                       \
  gash/compress.scm                             \
  gash/config.scm                               \
  gash/environment.scm                          \
  gash/gash.scm                                 \
  gash/geesh.scm                                \
  gash/grammar.scm                              \
  gash/guix-utils.scm                           \
  gash/io.scm                                   \
  gash/job.scm                                  \
  gash/lzw.scm                                  \
  gash/peg.scm                                  \
  gash/peg/cache.scm                            \
  gash/peg/codegen.scm                          \
  gash/peg/simplify-tree.scm                    \
  gash/peg/string-peg.scm                       \
  gash/peg/using-parsers.scm                    \
  gash/pipe.scm                                 \
  gash/readline.scm                             \
  gash/script.scm                               \
  gash/shell-utils.scm                          \
  gash/ustar.scm                                \
  gash/util.scm                                 \
  geesh/built-ins/break.scm                     \
  geesh/built-ins/cd.scm                        \
  geesh/built-ins/colon.scm                     \
  geesh/built-ins/continue.scm                  \
  geesh/built-ins/dot.scm                       \
  geesh/built-ins/echo.scm                      \
  geesh/built-ins/eval.scm                      \
  geesh/built-ins/exec.scm                      \
  geesh/built-ins/exit.scm                      \
  geesh/built-ins/export.scm                    \
  geesh/built-ins/false.scm                     \
  geesh/built-ins/pwd.scm                       \
  geesh/built-ins/read.scm                      \
  geesh/built-ins/readonly.scm                  \
  geesh/built-ins/return.scm                    \
  geesh/built-ins/set.scm                       \
  geesh/built-ins/shift.scm                     \
  geesh/built-ins/trap.scm                      \
  geesh/built-ins/true.scm                      \
  geesh/built-ins/umask.scm                     \
  geesh/built-ins/unset.scm                     \
  geesh/built-ins/utils.scm                     \
  geesh/built-ins.scm                           \
  geesh/environment.scm                         \
  geesh/eval.scm                                \
  geesh/lexer.scm                               \
  geesh/parser.scm                              \
  geesh/pattern.scm                             \
  geesh/repl.scm                                \
  geesh/shell.scm                               \
  geesh/word.scm

bin_SCRIPTS =                                   \
  scripts/gash                                  \
  scripts/geesh

do_subst = sed                                  \
  -e 's,[@]GUILE[@],$(GUILE),g'                 \
  -e 's,[@]MODDIR[@],$(moddir),g'               \
  -e 's,[@]GODIR[@],$(godir),g'

scripts/gash: scripts/gash.in Makefile
	$(do_subst) < $(srcdir)/scripts/gash.in > scripts/gash
	chmod a+x scripts/gash

scripts/geesh: scripts/geesh.in Makefile
	$(do_subst) < $(srcdir)/scripts/geesh.in > scripts/geesh
	chmod a+x scripts/geesh

UNIT_TESTS =                                    \
  tests/unit/lexer.scm                          \
  tests/unit/parser.scm                         \
  tests/unit/pattern.scm                        \
  tests/unit/shell.scm                          \
  tests/unit/word.scm

TESTS = $(UNIT_TESTS)

EXTRA_DIST =                                    \
  scripts/geesh.in

CLEANFILES =                                    \
  $(GOBJECTS)                                   \
  $(bin_SCRIPTS)                                \
  $(TESTS:tests/unit/%.scm=%.log)               \
  $(TESTS:tests/unit/%.scm=%.trs)

clean-local:
	$(MAKE) $(AM_MAKEFLAGS) -L -C tests/spec clean
