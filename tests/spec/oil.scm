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
           (use-modules (guix build utils))
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
             (("which \\$name") "which $name 2>/dev/null")))))))
