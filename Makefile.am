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
  gash/commands/find.scm                        \
  gash/commands/grep.scm                        \
  gash/commands/ln.scm                          \
  gash/commands/ls.scm                          \
  gash/commands/mkdir.scm                       \
  gash/commands/mv.scm                          \
  gash/commands/printf.scm                      \
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

bin_SCRIPTS =                                   \
  scripts/[                                     \
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
  scripts/egrep                                 \
  scripts/expr                                  \
  scripts/find                                  \
  scripts/grep                                  \
  scripts/ln                                    \
  scripts/ls                                    \
  scripts/mkdir                                 \
  scripts/mv                                    \
  scripts/printf                                \
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
  scripts/uname                                 \
  scripts/uniq                                  \
  scripts/wc                                    \
  scripts/which

do_subst = $(AM_V_GEN)sed                       \
  -e 's,[@]GUILE[@],$(GUILE),g'                 \
  -e 's,[@]MODDIR[@],$(moddir),g'               \
  -e 's,[@]CCACHEDIR[@],$(ccachedir),g'

scripts/[: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,testb,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/awk: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,awk,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/basename: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,basename,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/cat: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,cat,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/chmod: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,chmod,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/cmp: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,cmp,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/compress: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,compress,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/cp: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,cp,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/cut: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,cut,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/diff: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,diff,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/dirname: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,dirname,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/expr: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,expr,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/find: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,find,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/grep: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,grep,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/ln: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,ln,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/ls: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,ls,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/mkdir: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,mkdir,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/mv: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,mv,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/printf: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,printf,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/reboot: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,reboot,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/rm: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,rm,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/rmdir: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,rmdir,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/sed: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,sed,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/sleep: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,sleep,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/sort: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,sort,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/tar: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,tar,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/test: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,test,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/touch: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,touch,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/tr: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,tr,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/uname: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,uname,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/uniq: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,uniq,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/wc: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,wc,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/which: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,which,' < $< > $@
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
