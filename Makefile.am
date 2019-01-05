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

# Tests
#######

TEST_EXTENSIONS = .sh
SH_LOG_COMPILER = $(top_builddir)/pre-inst-env $(top_builddir)/test.sh

.PHONY: check-bootstrap
check-bootstrap:
	guix build -f tests/bootstrap/bash-without-bash.scm

TESTS =                                         \
  tests/00-exit.sh                              \
  tests/00-exit-0.sh                            \
  tests/00-exit-1.sh                            \
  tests/00-exit-2.sh                            \
  tests/00-exit-error.sh                        \
  tests/00-exit-var.sh                          \
  tests/01-script-newline.sh                    \
  tests/01-script-semi.sh                       \
  tests/01-script-backslash.sh                  \
  tests/01-script-backslash-space.sh            \
  tests/01-script-backslash-twice.sh            \
  tests/01-script-arg-0.sh                      \
  tests/01-script-arg-length.sh                 \
  tests/01-script-arg-list.sh                   \
  tests/01-script-shift.sh                      \
  tests/03-echo.sh                              \
  tests/03-echo-doublequotes.sh                 \
  tests/03-echo-nesting.sh                      \
  tests/03-echo-escaped-doublequotes.sh         \
  tests/03-echo-quoted-doublequotes.sh          \
  tests/03-echo-brace.sh                        \
  tests/04-echo-var.sh                          \
  tests/04-echo-equal.sh                        \
  tests/04-echo-word-at.sh                      \
  tests/04-echo-word-at-word.sh                 \
  tests/05-assignment.sh                        \
  tests/05-assignment-echo.sh                   \
  tests/05-assignment-empty.sh                  \
  tests/05-assignment-singlequote.sh            \
  tests/05-assignment-double-quote.sh           \
  tests/05-assignment-variable-word.sh          \
  tests/05-assignment-word-variable.sh          \
  tests/05-assignment-doublequoted-doublequotes.sh \
  tests/06-compound-word.sh                     \
  tests/06-command-compound-word.sh             \
  tests/07-variable-or.sh                       \
  tests/07-variable-not-or.sh                   \
  tests/07-variable-or-slash.sh                 \
  tests/07-variable-or-variable.sh              \
  tests/07-variable-or-doublequote.sh           \
  tests/07-variable-or-empty.sh                 \
  tests/08-variable-and.sh                      \
  tests/08-variable-not-and.sh                  \
  tests/10-if.sh                                \
  tests/10-if-false.sh                          \
  tests/10-if-word-variable.sh                  \
  tests/10-if-multiple.sh                       \
  tests/10-if-else.sh                           \
  tests/10-else-multiple.sh                     \
  tests/10-if-elif.sh                           \
  tests/10-if-test.sh                           \
  tests/10-if-test-false.sh                     \
  tests/10-if-bracket.sh                        \
  tests/10-if-bracket-false.sh                  \
  tests/11-for.sh                               \
  tests/11-for-split-sequence.sh                \
  tests/11-for-done-subshell.sh                 \
  tests/20-semi.sh                              \
  tests/20-or.sh                                \
  tests/20-and.sh                               \
  tests/20-pipe-exit-0.sh                       \
  tests/20-pipe-exit-1.sh                       \
  tests/20-pipe-sed.sh                          \
  tests/20-exec.sh                              \
  tests/30-substitution.sh                      \
  tests/30-substitution-word.sh                 \
  tests/30-substitution-backtick.sh             \
  tests/30-substitution-assignment.sh           \
  tests/30-for-substitution.sh                  \
  tests/30-substitution-assignment.sh           \
  tests/30-substitution-assignment-at.sh        \
  tests/30-substitution-redirect.sh             \
  tests/40-eval.sh                              \
  tests/40-eval-echo-variable.sh                \
  tests/40-assignment-eval-echo.sh              \
  tests/41-dot.sh                               \
  tests/42-sh.sh                                \
  tests/42-sh-export.sh                         \
  tests/50-iohere.sh                            \
  tests/50-iohere-builtin.sh                    \
  tests/50-redirect.sh                          \
  tests/50-redirect-in.sh                       \
  tests/50-redirect-append.sh                   \
  tests/50-redirect-pipe.sh                     \
  tests/50-redirect-merge.sh                    \
  tests/50-redirect-in-out.sh                   \
  tests/60-function.sh                          \
  tests/60-function-at.sh                       \
  tests/60-subst.sh                             \
  tests/70-hash.sh                              \
  tests/70-hash-hash.sh                         \
  tests/70-percent.sh                           \
  tests/70-percent-percent.sh                   \
  tests/70-percent-space.sh                     \
  tests/70-slash.sh                             \
  tests/70-slash-string.sh                      \
  tests/70-slash-string-slash.sh                \
  tests/100-cd-foo.sh                           \
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
  tests/00-exit-var.sh                          \
  tests/03-echo-brace.sh                        \
  tests/07-variable-or-doublequote.sh           \
  tests/10-if-redirect.sh                       \
  tests/11-for-split-sequence.sh                \
  tests/11-for-done-subshell.sh                 \
  tests/20-or.sh                                \
  tests/20-exec.sh                              \
  tests/30-substitution-word.sh                 \
  tests/30-substitution-backtick.sh             \
  tests/30-for-substitution.sh                  \
  tests/30-substitution-assignment-at.sh        \
  tests/30-substitution-redirect.sh             \
  tests/40-assignment-eval-echo.sh              \
  tests/42-sh-export.sh                         \
  tests/50-redirect-append.sh                   \
  tests/50-redirect-pipe.sh                     \
  tests/50-redirect-merge.sh                    \
  tests/60-function-at.sh                       \
  tests/100-basename-autoconf.sh                \
  tests/100-dirname-autoconf.sh                 \
  tests/100-tar-Z-pipe.sh

# These tests will not be run.  Put tests here
# that pass or fail based on environmental
# factors.
BROKEN_TESTS =                                  \
  tests/10-if-redirect.sh                       \
  tests/50-redirect-sed.sh                      \
  tests/50-redirect-space.sh                    \
  tests/100-cd.sh

# Distribution
##############

dist-hook:
	echo $(VERSION) > $(distdir)/.tarball-version
