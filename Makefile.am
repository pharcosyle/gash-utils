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

MODULES =                                       \
  geesh/lexer.scm                               \
  geesh/repl.scm

bin_SCRIPTS =                                   \
  scripts/geesh

TESTS =                                         \
  tests/lexer.scm                               \
  tests/repl.scm

CLEANFILES =                                    \
  $(GOBJECTS)                                   \
  $(TESTS:tests/%.scm=%.log)                    \
  $(TESTS:tests/%.scm=%.trs)
