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
                        (ice-9 rdelim))
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
                       "test/common.sh"
                       "test/sh_spec.py"
                       "test/spec-runner.sh"
                       "test/spec.sh"))
           (substitute* "test/common.sh"
             (("/usr/bin/env time") (which "time")))

           ;; This is not necessary, but it makes the output nicer.
           (substitute* "test/spec.sh"
             (("which \\$name") "which $name 2>/dev/null"))

           ;; We want to omit tests that use features we do not
           ;; support yet.  This lets us add tests quickly, and expand
           ;; to the more integrated tests as we are able.
           (let ((remove-tests
                  (lambda (tests file)
                    (format #t "Removing tests from ~a:~%" file)
                    (with-atomic-file-replacement file
                      (lambda (in out)
                        (let loop ((line (read-line in 'concat)) (ignore? #f))
                          (cond
                           ((eof-object? line) #t)
                           ((string-prefix? "####" line)
                            (let* ((name-part (substring line 4))
                                   (name (string-trim-both name-part)))
                              (if (member name tests)
                                  (begin
                                    (format #t "  - ~a~%" name)
                                    (loop (read-line in 'concat) #t))
                                  (begin
                                    (display line out)
                                    (loop (read-line in 'concat) #f)))))
                           (else
                            (unless ignore? (display line out))
                            (loop (read-line in 'concat) ignore?))))))))
                 (tests-to-remove '()))
             (for-each (match-lambda
                         ((file tests) (remove-tests tests file)))
                       tests-to-remove)))))))

;; Local Variables:
;; eval: (put 'with-atomic-file-replacement 'scheme-indent-function 1)
;; End:
