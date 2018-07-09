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
             (guix build-system trivial)
             (guix gexp)
             (guix download)
             (guix git-download)
             (guix licenses)
             (guix packages))

(define %source-dir (dirname (current-filename)))

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

(define-public gash
  (let ((commit "7b9871478b573e14fa94a84f3585c2cbed6f02a3")
        (revision "0")
        (version "0.1"))
    (package
      (name "gash")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/janneke/gash")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32 "11708vl2f04xpgs57mac4z7illx6wf4ybb32mh9ajfwimlkvwl4f"))))
      (build-system gnu-build-system)
      (propagated-inputs
       `(("guile-readline" ,guile-readline)))
      (inputs
       `(("guile" ,guile-2.2)))
      (native-inputs
       `(("texinfo" ,texinfo)))
      (synopsis "A POSIX compliant sh replacement for Guile.")
      (description
       "Gash [Guile As Shell] aims to produce at least a POSIX compliant sh replacement
or even implement GNU bash.  On top of that it also intends to make
scheme available for interactive and scripting application.")
      (home-page "https://gitlab.com/rutger.van.beusekom/gash")
      (license gpl3+))))

(define-public gash.git
 (let ((version "0.1")
        (revision "0")
        (commit (read-string (open-pipe "git show HEAD | head -1 | cut -d ' ' -f 2" OPEN_READ))))
    (package
      (inherit gash)
      (name "gash.git")
      (version (string-append version "-" revision "." (string-take commit 7)))
      (source (local-file %source-dir #:recursive? #t #:select? git-file?)))))

;; Return it here so `guix build/environment/package' can consume it directly.
gash.git
