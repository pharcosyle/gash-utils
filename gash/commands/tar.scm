;;; Gash-Utils
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Gash-Utils.
;;;
;;; Gash-Utils is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Gash-Utils is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash-Utils.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(define-module (gash commands tar)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-26)
  #:use-module (gash commands config)
  #:use-module (gash compat)
  #:use-module (gash compress)
  #:use-module (gash ustar)
  #:use-module (gash guix-utils)
  #:use-module (gash shell-utils)

  #:export (
            tar
            ))

(define (tar . args)
  (let* ((option-spec
	  '((create (single-char #\c))
            (compress (single-char #\Z))
            (directory (single-char #\C) (value #t))
            (gzip (single-char #\z))
            (bzip2 (single-char #\j))
            (xz (single-char #\J))
            (group (value #t))
            (exclude (value #t))
            (extract (single-char #\x))
            (file (single-char #\f) (value #t))
            (help (single-char #\h))
            (mtime (value #t))
            (list (single-char #\t))
            (numeric-owner?)
            (owner (value #t))
            (preserve-permissions (single-char #\p))
            (read-full-records (single-char #\B))
            (same-permissions)
            (sort (value #t))
            (strip (value #t))
            (strip-components (value #t))
	    (verbose (single-char #\v))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
         (options (if (or (option-ref options 'create #f)
                          (option-ref options 'extract #f)
                          (option-ref options 'list #f)
                          (null? (cdr args))
                          (string-prefix? "-" (cadr args))) options
                          (let ((args (cons* (car args)
                                             (string-append "-" (cadr args))
                                             (cddr args))))
                            (getopt-long args option-spec))))
         (create? (option-ref options 'create #f))
         (list? (option-ref options 'list #f))
         (extract? (option-ref options 'extract #f))
         (file (option-ref options 'file "-"))
	 (files (option-ref options '() '()))
         (compress? (option-ref options 'compress #f))
         (bzip2? (option-ref options 'bzip2 #f))
         (gzip? (option-ref options 'gzip #f))
         (xz? (option-ref options 'xz #f))
         (compression (cond (bzip2? 'bzip2)
                            (compress? 'compress)
                            (gzip? 'gzip)
                            (xz? 'xz)
                            (else (and (or extract? list? )
                                       (cond ((string-suffix? ".Z" file) 'compress)
                                             ((string-suffix? ".bz2" file) 'bzip2)
                                             ((string-suffix? ".gz" file) 'gzip)
                                             ((string-suffix? ".xz" file) 'xz)
                                             (else #f))))))
         (directory (option-ref options 'directory #f))
         (sort-order (and=> (option-ref options 'sort #f) string->symbol))
         (strip (string->number
                 (or (option-ref options 'strip #f)
                     (option-ref options 'strip-components #f)
                     "0")))
	 (help? (option-ref options 'help #f))
	 (usage? (and (not help?) (not (or (and create? (pair? files))
                                           extract? list?))))
	 (verbosity (length (multi-opt options 'verbose)))
         (version? (option-ref options 'version #f))
         (file (if (or (not directory) (string-prefix? "/" file) (equal? file "-")) file
                   (string-append (getcwd) "/" file))))
    (when directory
      (chdir directory))
    (cond (version? (format #t "tar (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: tar [OPTION]... [FILE]...
  -C, --directory=DIR        change to directory DIR
  -c, --create               create a new archive
  -f, --file=ARCHIVE         use archive file or device ARCHIVE
      --group=NAME           force NAME as group for added files
  -h, --help                 display this help
      --mtime=DATE-OR-FILE   set mtime for added files from DATE-OR-FILE
      --numeric-owner        always use numbers for user/group names
      --owner=NAME           force NAME as owner for added files
      --sort=ORDER           directory sorting order: none (default), name or
                             inode
      --strip-components=NUM strip NUM leading components from file names
                             names on extraction
  -t, --list                 list the contents of an archive
  -V, --version              display version
  -v, --verbose              verbosely list files processed
  -x, --extract              extract files from an archive
  -z, --gzip                 filter the archive through gzip
  -Z, --compress             filter the archive through compress

Ignored for compatibility:
  -B, --read-full-records
      --exclude=PATTERN
  -p, --preserve-permissions, --same-permissions
")
           (exit (if usage? 2 0)))
          (create?
           (let ((files (if (eq? sort-order 'name) (sort files string<)
                            files))
                 (group (and=> (option-ref options 'group #f) string->number))
                 (mtime (and=> (option-ref options 'mtime #f) string->number))
                 (numeric-owner? (option-ref options 'numeric-owner? #f))
                 (owner (and=> (option-ref options 'owner #f) string->number)))
             (if (or compression (equal? file "-"))
                 (let ((port (if (equal? file "-") (current-output-port)
                                 (open-file file "wb"))))
                   (call-with-compressed-output-port compression port
                     (cut apply write-ustar-port <>
                          `(,files
                            ,@(if group `(#:group ,group) '())
                            ,@(if mtime `(#:mtime ,mtime) '())
                            ,@(if numeric-owner? `(#:numeric-owner? ,numeric-owner?) '())
                            ,@(if owner `(#:owner ,owner) '())
                            ,@(if sort-order `(#:sort-order ,sort-order) '())
                            #:verbosity ,verbosity))))
                 (apply write-ustar-archive
                        `(,file
                          ,files
                          ,@(if group `(#:group ,group) '())
                          ,@(if mtime `(#:mtime ,mtime) '())
                          ,@(if numeric-owner? `(#:numeric-owner? ,numeric-owner?) '())
                          ,@(if owner `(#:owner ,owner) '())
                          ,@(if sort-order `(#:sort-order ,sort-order) '())
                          #:verbosity ,verbosity)))))
          (extract?
           (if (or compression (equal? file "-"))
               (let ((port (if (equal? file "-") (current-input-port)
                               (open-file file "rb"))))
                 (call-with-decompressed-port compression port
                   (cut read-ustar-port <> files #:strip strip #:verbosity verbosity)))
               (read-ustar-archive file files #:verbosity verbosity)))
          (list?
           (if (or compression (equal? file "-"))
               (let ((port (if (equal? file "-") (current-input-port)
                               (open-file file "rb"))))
                 (call-with-decompressed-port compression port
                   (cut list-ustar-port <> files #:strip strip #:verbosity (1+ verbosity))))
               (list-ustar-archive file files #:strip strip #:verbosity (1+ verbosity)))))))

(define main tar)
