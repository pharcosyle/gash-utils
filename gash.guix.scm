;;; guix.scm -- Guix package definition

;;; Mes --- Maxwell Equations of Software
;;; Copyright © 2016,2017,2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>

;;; Also borrowing code from:
;;; guile-sdl2 --- FFI bindings for SDL2
;;; Copyright © 2015 David Thompson <davet@gnu.org>

;;;
;;; guix.scm: This file is part of Mes.
;;;
;;; Mes is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Mes is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Mes.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, run:
;;
;;   guix package -f guix.scm
;;
;; To build it, but not install it, run:
;;
;;   guix build -f guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment -l guix.scm
;;
;;; Code:

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (gnu packages)
             (gnu packages base)
             (gnu packages bash)
             (gnu packages guile)
             (gnu packages mes)
             (gnu packages package-management)
             (gnu packages texinfo)
             ((guix build utils) #:select (with-directory-excursion))
             (guix build-system gnu)
             (guix build-system guile)
             (guix gexp)
             (guix download)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages))

(define %source-dir (getcwd))

(define git-file?
  (let* ((pipe (with-directory-excursion %source-dir
                 (open-pipe* OPEN_READ "git" "ls-files")))
         (files (let loop ((lines '()))
                  (match (read-line pipe)
                    ((? eof-object?)
                     (reverse lines))
                    (line
                     (loop (cons line lines))))))
         (status (close-pipe pipe)))
    (lambda (file stat)
      (match (stat:type stat)
        ('directory #t)
        ((or 'regular 'symlink)
         (any (cut string-suffix? <> file) files))
        (_ #f)))))

(define-public guile-gash
  (let ((version "0.1")
        (commit "5b7f85aa3d15523edd05a07ed2b16b6f69690d53")
        (revision "0")
        (builtins '(
                    "basename"
                    "cat"
                    "chmod"
                    "compress"
                    "cp"
                    "dirname"
                    "find"
                    "grep"
                    "ls"
                    "mkdir"
                    "mv"
                    "reboot"
                    "rm"
                    "rmdir"
                    "sed"
                    "tar"
                    "touch"
                    "tr"
                    "wc"
                    "which"
                    ))
        (shells '("bash" "gash" "sh")))
    (package
      (name "guile-gash")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source (origin
                (method url-fetch)
                (uri (string-append "https://gitlab.com/janneke/gash"
                                    "/-/archive/" commit
                                    "/gash-" commit ".tar.gz"))
                (sha256
                 (base32
                  "05nq0knklgk2iczsqmnhnh1365iv6gs3cxam38qf7dmdlglbf0sa"))))
      (build-system guile-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'remove-geesh
             (lambda _
               (delete-file "guix.scm") ; should not and cannot be compiled
               (delete-file "gash/geesh.scm") ; no Geesh yet
               #t))
           (add-after 'unpack 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (guile (assoc-ref inputs "guile"))
                      (bin/guile (string-append guile "/bin/guile"))
                      (effective (target-guile-effective-version))
                      (guile-site-dir
                       (string-append out "/share/guile/site/" effective))
                      (guile-site-ccache-dir
                       (string-append out
                                      "/lib/guile/" effective "/site-ccache")))
                 (define (make-script source name)
                   (let ((script (string-append "bin/" name)))
                     (copy-file source script)
                     (substitute* script
                       (("@GUILE@") bin/guile)
                       (("@guile_site_dir@") guile-site-dir)
                       (("@guile_site_ccache_dir@") guile-site-ccache-dir)
                       (("@builtin@") name))
                     (chmod script #o755)))
                 (copy-file "gash/config.scm.in" "gash/config.scm")
                 (substitute* "gash/config.scm"
                   (("@guile_site_ccache_dir@") guile-site-ccache-dir)
                   (("@VERSION@") ,version)
                   (("@COMPRESS@") (string-append out "/bin/compress"))
                   (("@BZIP2@") (which "bzip2"))
                   (("@GZIP@") (which "gzip"))
                   (("@XZ@") (which "xz")))
                 (for-each
                  (lambda (s) (make-script "scripts/gash.in" s)) ',shells)
                 (for-each
                  (lambda (s) (make-script "bin/builtin.in" s)) ',builtins))
               #t))
           (add-after 'install 'install-scripts
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (libexec/gash (string-append out "/libexec/gash")))
                 (install-file "scripts/gash" bin)
                 (for-each
                  (lambda (name)
                    (install-file (string-append "bin/" name) libexec/gash))
                  ',(append builtins shells)))
               #t)))))
      (native-inputs
       `(("guile" ,guile-2.2)
         ("guile-readline" ,guile-readline)))
      (home-page "https://gitlab.com/rutgervanbeusekom/gash")
      (synopsis "Guile As SHell")
      (description
       "Gash--Guile As SHell-- aims to produce at least a POSIX compliant sh
replacement or even implement GNU bash.  On top of that it also intends to
make Scheme available for interactive and scripting application.")
      (license license:gpl3+))))

(define-public gash.git
 (let ((version "0.1")
        (revision "0")
        (commit (read-string (open-pipe "git show HEAD | head -1 | cut -d ' ' -f 2" OPEN_READ))))
    (package
      (inherit guile-gash)
      (name "gash.git")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source (local-file %source-dir #:recursive? #t #:select? git-file?)))))

;; Return it here so `guix build/environment/package' can consume it directly.
gash.git
