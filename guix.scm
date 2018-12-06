(use-modules (gnu packages)
             (gnu packages autotools)
             (gnu packages code)
             (gnu packages guile)
             (gnu packages pkg-config)
             (guix build utils)
             (guix build-system gnu)
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

(package
  (name "geesh")
  (version *version*)
  (source #f)
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
  (home-page #f)
  (synopsis #f)
  (description #f)
  (license #f))
