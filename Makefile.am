include $(top_srcdir)/build-aux/guile.am

# Modules and scripts
#####################

SOURCES =                                       \
  gash/bournish-commands.scm                    \
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
  gash/guix-utils.scm                           \
  gash/io.scm                                   \
  gash/lzw.scm                                  \
  gash/shell-utils.scm                          \
  gash/ustar.scm                                \
  gash/util.scm

bin_SCRIPTS =                                   \
  scripts/basename                              \
  scripts/cat                                   \
  scripts/chmod                                 \
  scripts/compress                              \
  scripts/cp                                    \
  scripts/dirname                               \
  scripts/find                                  \
  scripts/grep                                  \
  scripts/ls                                    \
  scripts/mkdir                                 \
  scripts/mv                                    \
  scripts/reboot                                \
  scripts/rm                                    \
  scripts/rmdir                                 \
  scripts/sed                                   \
  scripts/tar                                   \
  scripts/touch                                 \
  scripts/tr                                    \
  scripts/wc                                    \
  scripts/which

do_subst = $(AM_V_GEN)sed                       \
  -e 's,[@]GUILE[@],$(GUILE),g'                 \
  -e 's,[@]MODDIR[@],$(moddir),g'               \
  -e 's,[@]CCACHEDIR[@],$(ccachedir),g'

scripts/basename: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,basename,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/cat: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,cat,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/chmod: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,chmod,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/compress: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,compress,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/cp: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,cp,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/dirname: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,dirname,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/find: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,find,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/grep: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,grep,' < $< > $@
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

scripts/tar: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,tar,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/touch: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,touch,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/tr: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,tr,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/wc: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,wc,' < $< > $@
	$(AM_V_at)chmod a+x $@

scripts/which: $(srcdir)/scripts/template.in Makefile
	$(do_subst) -e 's,@UTILITY@,which,' < $< > $@
	$(AM_V_at)chmod a+x $@

# Tests
#######

TEST_EXTENSIONS = .sh
SH_LOG_COMPILER = $(top_builddir)/pre-inst-env $(top_builddir)/test.sh

.PHONY: check-bootstrap
check-bootstrap:
	guix build -f tests/bootstrap/bash-without-bash.scm

TESTS =                                         \
  tests/100-test.sh                             \
  tests/100-test-file.sh                        \
  tests/100-bracket-file.sh                     \
  tests/100-basename-root.sh                    \
  tests/100-dirname-root.sh                     \
  tests/100-basename-autoconf.sh                \
  tests/100-dirname-autoconf.sh                 \
  tests/100-sed.sh                              \
  tests/100-sed-once.sh                         \
  tests/100-sed-global.sh                       \
  tests/100-sed-case.sh                         \
  tests/100-sed-group.sh                        \
  tests/100-sed-group-extended.sh               \
  tests/100-sed-twice.sh                        \
  tests/100-sed-undo.sh                         \
  tests/100-sed-file.sh                         \
  tests/100-sed-fooRbar.sh                      \
  tests/100-sed-pattern-address.sh              \
  tests/100-sed-quit.sh                         \
  tests/100-sed-autoconf-basename.sh            \
  tests/100-tar.sh                              \
  tests/100-tar-Z.sh                            \
  tests/100-tar-Z-old.sh                        \
  tests/100-tar-Z-pipe.sh                       \
  tests/100-tar-ro.sh                           \
  tests/100-tr.sh

XFAIL_TESTS =                                   \
  tests/100-tar-Z-pipe.sh

# Distribution
##############

dist-hook:
	echo $(VERSION) > $(distdir)/.tarball-version
