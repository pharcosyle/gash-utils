(use-modules (git)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages code)
             (gnu packages guile)
             (gnu packages pkg-config)
             (guix build utils)
             (guix build-system gnu)
             (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix utils)
             (ice-9 popen)
             (ice-9 textual-ports))

(define *srcdir* (canonicalize-path (current-source-directory)))

(define *version*
  (with-directory-excursion *srcdir*
    (let* ((script "./build-aux/git-version-gen")
           (pipe (open-pipe* OPEN_READ script ".tarball-version"))
           (version (get-string-all pipe)))
      (close-pipe pipe)
      version)))

(define (make-select)
  (let* ((directory (repository-discover "/home/samplet/code/geesh"))
         (repository (repository-open directory))
         (oid (reference-target (repository-head repository)))
         (commit (commit-lookup repository oid))
         (tree (commit-tree commit))
         (paths (tree-list tree)))
    (lambda (file stat)
      (let ((relative (substring file (1+ (string-length *srcdir*)))))
        (member relative paths)))))

(package
  (name "geesh")
  (version *version*)
  (source (local-file *srcdir* #:recursive? #t #:select? (make-select)))
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("lcov" ,lcov)                ; For generating test coverage data
     ("pkg-config" ,pkg-config)))
  (inputs
   `(("guile" ,guile-2.2)))
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       (add-before 'configure 'bootstrap
         (lambda _
           (zero? (system* "sh" "bootstrap")))))))
  (home-page "https://gitlab.com/samplet/geesh")
  (synopsis "POSIX-compatible shell written in Guile Scheme")
  (description "Geesh is a POSIX-compatible shell written in Guile
Scheme.  It is designed to be capable of bootstrapping Bash.")
  (license license:gpl3+))
