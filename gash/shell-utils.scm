;;; Gash --- Guile As SHell
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Gash.
;;;
;;; Gash is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Gash is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; The initial guix-build-utils.scm was taken from Guix.

;;; Code:

(define-module (gash shell-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)

  #:use-module (ice-9 ftw)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)

  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:export (
            delete-file-recursively
            display-tabulated
            display-file
            dump-port
            executable-path
            file-name-predicate
            find-files
            file-exists?*
            grep*
            grep+
            <grep-match>
            grep-match-file-name
            grep-match-string
            grep-match-line
            grep-match-column
            grep-match-end-column
            mkdir-p
            multi-opt

            directory-exists?
            executable-file?
            regular-file?
            symbolic-link?
            substitute*
            substitute-port
            with-atomic-file-replacement
            let-matches
            ))

;;; Commentary:

;;; This code is taken from (guix build utils)

;;;
;;; Directories.
;;;

(define (directory-exists? dir)
  "Return #t if DIR exists and is a directory."
  (let ((s (stat dir #f)))
    (and s
         (eq? 'directory (stat:type s)))))

(define (executable-file? file)
  "Return #t if FILE exists and is executable."
  (let ((s (stat file #f)))
    (and s
         (not (zero? (logand (stat:mode s) #o100))))))

(define (regular-file? file)
  "Return #t if FILE is a regular file."
  (let ((s (stat file #f)))
    (and s
         (eq? (stat:type s) 'regular))))

(define (symbolic-link? file)
  "Return #t if FILE is a symbolic link (aka. \"symlink\".)"
  (let ((s (lstat file)))
    (and s
         (eq? (stat:type s) 'symlink))))

(define (file-name-predicate regexp)
  "Return a predicate that returns true when passed a file name whose base
name matches REGEXP."
  (let ((file-rx (if (regexp? regexp)
                     regexp
                     (make-regexp regexp))))
    (lambda (file stat)
      (regexp-exec file-rx (basename file)))))

(define* (find-files dir #:optional (pred (const #t))
                     #:key (stat lstat)
                     directories?
                     fail-on-error?)
  "Return the lexicographically sorted list of files under DIR for which PRED
returns true.  PRED is passed two arguments: the absolute file name, and its
stat buffer; the default predicate always returns true.  PRED can also be a
regular expression, in which case it is equivalent to (file-name-predicate
PRED).  STAT is used to obtain file information; using 'lstat' means that
symlinks are not followed.  If DIRECTORIES? is true, then directories will
also be included.  If FAIL-ON-ERROR? is true, raise an exception upon error."
  (let ((pred (if (procedure? pred)
                  pred
                  (file-name-predicate pred))))
    ;; Sort the result to get deterministic results.
    (sort (file-system-fold (const #t)
                            (lambda (file stat result) ; leaf
                              (if (pred file stat)
                                  (cons file result)
                                  result))
                            (lambda (dir stat result) ; down
                              (if (and directories?
                                       (pred dir stat))
                                  (cons dir result)
                                  result))
                            (lambda (dir stat result) ; up
                              result)
                            (lambda (file stat result) ; skip
                              result)
                            (lambda (file stat errno result)
                              (format (current-error-port) "find-files: ~a: ~a~%"
                                      file (strerror errno))
                              (when fail-on-error?
                                (error "find-files failed"))
                              result)
                            '()
                            dir
                            stat)
          string<?)))

(define* (delete-file-recursively dir
                                  #:key follow-mounts?)
  "Delete DIR recursively, like `rm -rf', without following symlinks.  Don't
follow mount points either, unless FOLLOW-MOUNTS? is true.  Report but ignore
errors."
  (let ((dev (stat:dev (lstat dir))))
    (file-system-fold (lambda (dir stat result)    ; enter?
                        (or follow-mounts?
                            (= dev (stat:dev stat))))
                      (lambda (file stat result)   ; leaf
                        (delete-file file))
                      (const #t)                   ; down
                      (lambda (dir stat result)    ; up
                        (rmdir dir))
                      (const #t)                   ; skip
                      (lambda (file stat errno result)
                        (format (current-error-port)
                                "warning: failed to delete ~a: ~a~%"
                                file (strerror errno)))
                      #t
                      dir

                      ;; Don't follow symlinks.
                      lstat)))

(define* (dump-port in out
                    #:key (buffer-size 16384)
                    (progress (lambda (t k) (k))))
  "Read as much data as possible from IN and write it to OUT, using chunks of
BUFFER-SIZE bytes.  Call PROGRESS at the beginning and after each successful
transfer of BUFFER-SIZE bytes or less, passing it the total number of bytes
transferred and the continuation of the transfer as a thunk."
  (define buffer
    (make-bytevector buffer-size))

  (define (loop total bytes)
    (or (eof-object? bytes)
        (let ((total (+ total bytes)))
          (put-bytevector out buffer 0 bytes)
          (progress total
                    (lambda ()
                      (loop total
                            (get-bytevector-n! in buffer 0 buffer-size)))))))

  ;; Make sure PROGRESS is called when we start so that it can measure
  ;; throughput.
  (progress 0
            (lambda ()
              (loop 0 (get-bytevector-n! in buffer 0 buffer-size)))))

(define-immutable-record-type <grep-match>
  (make-grep-match file-name string line column end-column)
  grep-match?
  (file-name grep-match-file-name)
  (string grep-match-string)
  (line grep-match-line)
  (column grep-match-column)
  (end-column grep-match-end-column))

(define* (grep* pattern #:key (port (current-input-port)) (file-name "<stdin>"))
  ;; FIXME: collect later?  for scripting usage implicit collect is
  ;; nice; for pipeline usage not so much
  (let loop ((line (read-line port)) (ln 1) (matches '()))
    (if (eof-object? line) (reverse matches)
        (let* ((m (list-matches pattern line))
               (m (and (pair? m) (car m))))
          (loop (read-line port) (1+ ln)
                (if m (cons (make-grep-match file-name
                                             (match:string m)
                                             ln
                                             (match:start m)
                                             (match:end m)) matches)
                    matches))))))

(define (grep+ pattern file)
  (cond ((and (string? file)
              (not (equal? file "-"))) (call-with-input-file file
                                         (lambda (in)
                                           (grep* pattern #:port in #:file-name file))))
        (else (grep* pattern))))

(define (mkdir-p dir)
  "Create directory DIR and all its ancestors."
  (define absolute?
    (string-prefix? "/" dir))

  (define not-slash
    (char-set-complement (char-set #\/)))

  (let loop ((components (string-tokenize dir not-slash))
             (root       (if absolute?
                             ""
                             ".")))
    (match components
      ((head tail ...)
       (let ((path (string-append root "/" head)))
         (catch 'system-error
           (lambda ()
             (mkdir path)
             (loop tail path))
           (lambda args
             (if (= EEXIST (system-error-errno args))
                 (loop tail path)
                 (apply throw args))))))
      (() #t))))

(define (file-exists?* file)
  "Like 'file-exists?' but emits a warning if FILE is not accessible."
  (catch 'system-error
    (lambda ()
      (stat file))
    (lambda args
      (let ((errno (system-error-errno args)))
        (format (current-error-port) "~a: ~a~%"
                file (strerror errno))
        #f))))

(define* (display-tabulated lst
                            #:key
                            (terminal-width 80)
                            (column-gap 2))
  "Display the list of string LST in as many columns as needed given
TERMINAL-WIDTH.  Use COLUMN-GAP spaces between two subsequent columns."
  (define len (length lst))
  (define column-width
    ;; The width of a column.  Assume all the columns have the same width
    ;; (GNU ls is smarter than that.)
    (+ column-gap (reduce max 0 (map string-length lst))))
  (define columns
    (max 1
         (quotient terminal-width column-width)))
  (define pad
    (if (zero? (modulo len columns))
        0
        columns))
  (define items-per-column
    (quotient (+ len pad) columns))
  (define items (list->vector lst))

  (let loop ((indexes (unfold (cut >= <> columns)
                              (cut * <> items-per-column)
                              1+
                              0)))
    (unless (>= (first indexes) items-per-column)
      (for-each (lambda (index)
                  (let ((item (if (< index len)
                                  (vector-ref items index)
                                  "")))
                    (display (string-pad-right item column-width))))
                indexes)
      (newline)
      (loop (map 1+ indexes)))))

(define* (display-file file-name #:optional st)
  (define (display-rwx perm sticky)
    (display (if (zero? (logand perm 4)) "-" "r"))
    (display (if (zero? (logand perm 2)) "-" "w"))
    (display (let ((x (logand perm 1)))
               (if (zero? sticky) (if (zero? x) "-" "x")
                   (if (= sticky 1) (if (zero? x) "T" "t")
                       (if (zero? x) "S" "s"))))))
  (define (display-bcdfsl type)
    (display
     (case type
       ((block-special) "b")
       ((char-special) "c")
       ((directory) "d")
       ((fifo) "p")
       ((regular) "-")
       ((socket) "s")
       ((symlink) "l")
       (else "?"))))
  (let* ((st (or st (lstat file-name)))
         (mode (stat:mode st))
         (uid  (stat:uid st))
         (gid  (stat:gid st))
         (size (stat:size st))
         (date (strftime "%c" (localtime (stat:mtime st))))
         (sticky (ash mode -9)))
    (display-bcdfsl (stat:type st))
    (display-rwx (ash mode -6) (logand sticky 4))
    (display-rwx (ash (logand mode #o70) -3) (logand sticky 2))
    (display-rwx (logand mode #o7) (logand sticky 1))
    (display " ")
    (let ((ent (catch #t (compose passwd:name (cut getpwuid uid)) (const uid))))
      (format #t "~8a" ent))
    (display " ")
    (let ((ent (catch #t (compose group:name (cut getgrgid gid)) (const gid))))
      (format #t "~8a" ent))
    (format #t "~8d" size)
    (display " ")
    (display date)
    (display " "))
  (display file-name))

(define (multi-opt options name)
  (let ((opt? (lambda (o) (and (eq? (car o) name) (cdr o)))))
    (filter-map opt? (reverse options))))

(define %not-colon (char-set-complement (char-set #\:)))
(define (executable-path)
  "Return the search path for programs as a list."
  (match (getenv "PATH")
    (#f  '())
    (str (string-tokenize str %not-colon))))


;;;
;;; Text substitution (aka. sed).
;;;

(define (with-atomic-file-replacement file proc)
  "Call PROC with two arguments: an input port for FILE, and an output
port for the file that is going to replace FILE.  Upon success, FILE is
atomically replaced by what has been written to the output port, and
PROC's result is returned."
  (let* ((template (string-append file ".XXXXXX"))
         (out      (mkstemp! template))
         (mode     (stat:mode (stat file))))
    (with-throw-handler #t
      (lambda ()
        (call-with-input-file file
          (lambda (in)
            (let ((result (proc in out)))
              (close out)
              (chmod template mode)
              (rename-file template file)
              result))))
      (lambda (key . args)
        (false-if-exception (delete-file template))))))

(define (substitute* file pattern+procs)
  "PATTERN+PROCS is a list of regexp/two-argument-procedure pairs.  For each
line of FILE, and for each PATTERN that it matches, call the corresponding
PROC as (PROC LINE MATCHES); PROC must return the line that will be written as
a substitution of the original line.  Be careful about using '$' to match the
end of a line; by itself it won't match the terminating newline of a line."
  (let ((rx+proc  (map (match-lambda
                         ;; (((? regexp? pattern) . proc)
                         ;;  (cons pattern proc))
                         (((pattern . flags) . proc)
                          (cons (apply make-regexp pattern flags)
                                proc)))
                       pattern+procs)))
    (with-atomic-file-replacement file
      (lambda (in out)
        (let loop ((line (read-line in 'concat)))
          (if (eof-object? line)
              #t
              (let ((line (fold (lambda (r+p line)
                                  (match r+p
                                    ((regexp . proc)
                                     (match (list-matches regexp line)
                                       ((and m+ (_ _ ...))
                                        (proc line m+))
                                       (_ line)))))
                                line
                                rx+proc)))
                (display line out)
                (loop (read-line in 'concat)))))))))

(define (substitute-port pattern+procs)
  (let ((rx+proc  (map (match-lambda
                         ;; (((? regexp? pattern) . proc)
                         ;;  (cons pattern proc))
                         (((pattern . flags) . proc)
                          (cons (apply make-regexp pattern flags)
                                proc)))
                       pattern+procs))
        (in (current-input-port))
        (out (current-output-port)))
    (let loop ((line (read-line in 'concat)))
      (if (eof-object? line)
          #t
          (let ((line (fold (lambda (r+p line)
                              (match r+p
                                ((regexp . proc)
                                 (match (list-matches regexp line)
                                   ((and m+ (_ _ ...))
                                    (proc line m+))
                                   (_ line)))))
                            line
                            rx+proc)))
            (display line out)
            (loop (read-line in 'concat)))))))
