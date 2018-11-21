(use-modules (gnu packages bash)
             (gnu packages python)
             (gnu packages time)
             (guix gexp)
             (guix git-download)
             (guix modules)
             (guix monads)
             (guix packages)
             (guix store))

(let* ((commit "4d10a3d2a477a9f4e0d92b8c1fe37c7ee9507aa8")
       (version (git-version "0.6.pre5" "0" commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/oilshell/oil.git")
                (commit commit)))
          (file-name (string-append "oil-" version "-checkout"))
          (sha256
           (base32
            "0gq3c4m4sz27qmfk6c6k0l7q0fas2gbal1yj0dd97yz3pgkl1vqz")))))
  (run-with-store (open-connection)
    (with-imported-modules (source-module-closure
                            '((guix build utils)))
      (gexp->derivation
       (string-append "oil-tests-" version)
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 match)
                        (ice-9 rdelim)
                        (ice-9 regex)
                        (srfi srfi-1))
           (copy-recursively #$source #$output)
           (setenv "PATH" (list->search-path-as-string
                           (map (lambda (p)
                                  (string-append p "/bin"))
                                (list #$bash-minimal
                                      #$python-2
                                      #$time))
                           ":"))
           (chdir #$output)
           (for-each patch-shebang
                     '("spec/bin/argv.py"
                       "spec/bin/printenv.py"
                       "test/common.sh"
                       "test/sh_spec.py"
                       "test/spec-runner.sh"
                       "test/spec.sh"))
           (substitute* "test/common.sh"
             (("/usr/bin/env time") (which "time")))

           ;; We do not have the BusyBox shell, but the test suite
           ;; will try to use it anyway.  This change fixes that.
           (substitute* "test/spec.sh"
             (("BUSYBOX_ASH=_tmp/shells/ash") "BUSYBOX_ASH="))

           ;; This is not necessary, but it makes the output nicer.
           (substitute* "test/spec.sh"
             (("which \\$name") "which $name 2>/dev/null"))

           ;; We want to omit tests that use features we do not
           ;; support yet.  This lets us add tests quickly, and expand
           ;; to the more integrated tests as we are able.
           (let ((filter-tests
                  (lambda (tests file)
                    (format #t "Removing tests from ~a:~%" file)
                    (with-atomic-file-replacement file
                      (lambda (in out)
                        (let loop ((line (read-line in 'concat))
                                   (transformers #t))
                          (cond
                           ((eof-object? line) #t)
                           ((string-prefix? "####" line)
                            (let* ((name-part (substring line 4))
                                   (name (string-trim-both name-part)))
                              (match (assoc name tests)
                                ((_ . ())
                                 (format #t "  - ~a~%" name)
                                 (loop (read-line in 'concat) #f))
                                ((_ . (transformers ..1))
                                 (format #t "  * ~a~%" name)
                                 (display line out)
                                 (loop (read-line in 'concat) transformers))
                                (#f
                                 (display line out)
                                 (loop (read-line in 'concat) #t)))))
                           (else
                            (match transformers
                              (#f #t)
                              (#t (display line out))
                              (((targets replacements) ..1)
                               (display
                                (fold (lambda (target replacement line)
                                        (regexp-substitute/global
                                         #f target line
                                         'pre replacement 'post))
                                      line
                                      targets replacements)
                                out)))
                            (loop (read-line in 'concat) transformers))))))))
                 (tests-to-filter
                  '(("spec/case_.test.sh"
                     (;; These two are Bash specific.
                      ("Case statement with ;;&")
                      ("Case statement with ;&")))
                    ("spec/loop.test.sh"
                     (;; We do not do tilde expansion yet.
                      ("Tilde expansion within for loop")
                      ;; This is beyond POSIX.
                      ("Brace Expansion within Array"
                       ("N-I dash" "N-I dash/geesh"))
                      ;; We match Bash here.
                      ("for loop with invalid identifier"
                       ("OK bash" "OK bash/geesh"))
                      ;; For these next two, we do not have arithmetic
                      ;; substitution, but we can use 'expr' instead.
                      ("while in pipe"
                       ("\\$\\(\\(i\\+1\\)\\)" "$(expr $i + 1)"))
                      ("while in pipe with subshell"
                       ("\\$\\(\\(i\\+1\\)\\)" "$(expr $i + 1)"))))
                    ("spec/quote.test.sh"
                     (;; We match KornShell on these two tests.
                      ("Unterminated single quote"
                       ("OK mksh" "OK mksh/geesh"))
                      ("Unterminated double quote"
                       ("OK mksh" "OK mksh/geesh"))
                      ;; The rest of these are well beyond POSIX.
                      ("$''")
                      ("$'' with quotes")
                      ("$'' with newlines")
                      ("$'' octal escapes don't have leading 0")
                      ("$'' octal escapes with fewer than 3 chars")
                      ("$\"\"")))
                    ("spec/word-split.test.sh"
                     (;; This test requires local variables, which is
                      ;; a Bash extension.
                      ("IFS is scoped")
                      ;; We do not do tilde expansion yet.
                      ("Tilde sub is not split, but var sub is")
                      ;; These tests rely on 'echo -e', but we can use
                      ;; Guile instead.
                      ("IFS empty doesn't do splitting"
                       ("echo -e ' a b\\\\tc\\\\n'"
                        "guile -c '(display \" a b\\tc\\n\")'"))
                      ("IFS unset behaves like $' \\t\\n'"
                       ("echo -e ' a b\\\\tc\\\\n'"
                        "guile -c '(display \" a b\\tc\\n\")'"))))
                    ("spec/var-sub.test.sh"
                     (;; We match Bash here.
                      ("Bad var sub"
                       ("OK bash" "OK bash/geesh"))
                      ;; This test expects 'ls' to be at '/bin/ls',
                      ;; which isn't the case on GuixSD.
                      ("Braced block inside ${}"
                       ("which ls" "guile -c '(display \"/bin/ls\")'"))
                      ;; We match KornShell here.
                      ("Here doc with bad \"$@\" delimiter"
                       ("OK mksh" "OK mksh/geesh"))))
                    ("spec/redirect.test.sh"
                     (;; We match Bash and Dash here, just not Oil.
                      ("Redirect in assignment is invalid"
                       ("OK bash" "OK bash/geesh"))
                      ;; Again, we match Dash here (though not Bash).
                      ("Redirect in assignment"
                       ("OK dash" "OK dash/geesh"))
                      ;; This test requires arithmetic substitutions.
                      ("Redirect in function body is evaluated multiple times")
                      ;; We match KornShell here.
                      ("Prefix redirect for loop -- not allowed"
                       ("OK mksh" "OK mksh/geesh"))
                      ;; We do not support named file descriptors
                      ;; (they are not in POSIX).
                      ("Named file descriptor")
                      ;; This test relies on 'set', which we do not
                      ;; have yet.
                      (">| to clobber")
                      ;; This is Bash specific.
                      ("&> redirects stdout and stderr")
                      ;; This seems to go beyond POSIX.
                      ("1>&2- to close file descriptor")
                      ;; Again, this is Bash specific.
                      ("&>> appends stdout and stderr"))))))
             (for-each (match-lambda
                         ((file tests) (filter-tests tests file)))
                       tests-to-filter)))))))

;; Local Variables:
;; eval: (put 'with-atomic-file-replacement 'scheme-indent-function 1)
;; End:
