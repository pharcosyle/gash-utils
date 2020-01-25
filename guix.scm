;;; Guix Package File for Gash-Utils
;;; Copyright © 2017, 2018, 2019, 2020 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is free software; as a special exception the author gives
;;; unlimited permission to copy and/or distribute it, with or without
;;; modifications, as long as this notice is preserved.
;;;
;;; This program is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY, to the extent permitted by law; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(use-modules (git)
             (gnu packages)
             (gnu packages autotools)
             (gnu packages code)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages shells)
             (guix build utils)
             (guix build-system gnu)
             (guix download)
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
  (define paths
    (or (false-if-exception
         (let* ((directory (repository-discover *srcdir*))
                (repository (repository-open directory))
                (oid (reference-target (repository-head repository)))
                (commit (commit-lookup repository oid))
                (tree (commit-tree commit)))
           (tree-list tree)))
        (false-if-exception
         (with-directory-excursion *srcdir*
           (call-with-input-file ".tarball-manifest"
             (lambda (port)
               (let loop ((line (get-line port)) (acc '()))
                 (if (eof-object? line)
                     acc
                     (loop (get-line port) (cons line acc))))))))
        (error "Cannot make file selector")))
  (lambda (file stat)
    (let ((relative (substring file (1+ (string-length *srcdir*)))))
      (or (eq? (stat:type stat) 'directory)
           (member relative paths)))))

(define guile-2.0.9
  (package
    (inherit guile-2.0)
    (version "2.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/guile/guile-" version ".tar.xz"))
       (sha256
        (base32
         "0nw9y8vjyz4r61v06p9msks5lm58pd91irmzg4k487vmv743h2pp"))))
    (arguments
     (substitute-keyword-arguments (package-arguments guile-2.0)
       ;; XXX: There are some encoding and network test failures.
       ((#:tests? _ #f) #f)))))

(define guile-2.0.9-gash
  (package
    (inherit gash)
    (arguments
     ;; The unit tests fail because SRFI 64 is missing.
     (substitute-keyword-arguments (package-arguments gash)
       ((#:tests? tests? #f) #f)))
    (inputs
     `(("guile" ,guile-2.0.9)))))

(package
  (name "gash-utils")
  (version *version*)
  (source (local-file *srcdir* #:recursive? #t #:select? (make-select)))
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("pkg-config" ,pkg-config)))
  (inputs
   `(("gash" ,gash)
     ("guile" ,guile-2.2)))
  (home-page "https://savannah.nongnu.org/projects/gash/")
  (synopsis "Select GNU utilities reimplemented in Guile Scheme")
  (description "Gash-Utils provides a number of GNU utilities (e.g.,
Sed, Awk, and Grep) implemented in Guile Scheme.")
  (license license:gpl3+))
