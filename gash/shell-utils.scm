;;; Gash --- Guile As SHell
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gash util)
  #:export (
            delete-file-recursively
            display-tabulated
            display-file
            dump-port
            executable-path
            file-class
            file-name-predicate
            find-files
            file-exists?*
            copy-file*
            copy-files
            link-file*
            link-files
            read-lines
            read-lines/reversed
            string-numeric<?
            symlink-file*
            symlink-files

            <chmodifier>
            make-chmodifier
            chmodifier-users
            chmodifier-operation
            chmodifier-permissions
            make-numeric-chmodifier
            chmodifier->mode
            chmodifiers->mode
            apply-chmodifiers
            parse-chmodifiers

            <grep-match>
            grep*
            grep+
            grep-match-file-name
            grep-match-string
            grep-match-line
            grep-match-column
            grep-match-end-column
            mkdir-p
            rmdir-p
            multi-opt
            mixed-multi-opt

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

(define* (read-lines #:optional (port (current-input-port)))
  (let loop ((line (read-line port)))
    (if (eof-object? line) '()
        (cons line (loop (read-line port))))))

(define* (read-lines/reversed #:optional (port (current-input-port)))
  (let loop ((line (read-line port)) (acc '()))
    (match line
      ((? eof-object?) acc)
      (_ (loop (read-line port) (cons line acc))))))

(define-immutable-record-type <grep-match>
  (make-grep-match file-name string line column end-column)
  grep-match?
  (file-name grep-match-file-name)
  (string grep-match-string)
  (line grep-match-line)
  (column grep-match-column)
  (end-column grep-match-end-column))

(define (list-matches* regexps str)
  (let loop ((regexps regexps))
    (match regexps
      (() '())
      ((regexp . rest)
       (match (list-matches regexp str)
         (() (loop rest))
         (matches matches))))))

(define (string-numeric<? a b)
  (< (or (string->number a) 0)
     (or (string->number b) 0)))

(define* (grep* pattern #:key (port (current-input-port)) (file-name "<stdin>") (matching 'basic) inverted?)
  ;; FIXME: collect later?  for scripting usage implicit collect is
  ;; nice; for pipeline usage not so much

  (define (pattern->regexp pattern)
    (match matching
      ('basic (make-regexp pattern regexp/basic))
      ('extended (make-regexp pattern regexp/extended))
      ;; XXX: Just use regular string matching.
      ('string (make-regexp (regexp-quote pattern)))))

  (define rxs
    (match pattern
      ((patterns ...) (map pattern->regexp patterns))
      (_ (list (pattern->regexp pattern)))))

  (let loop ((line (read-line port)) (ln 1) (matches '()))
    (if (eof-object? line) (reverse matches)
        (let* ((m (list-matches* rxs line))
               (m (and (pair? m) (car m))))
          (loop (read-line port) (1+ ln)
                (cond
                 ((and m (not inverted?))
                  (cons (make-grep-match file-name (match:string m) ln
                                         (match:start m) (match:end m))
                        matches))
                 ((and (not m) inverted?)
                  (cons (make-grep-match file-name line ln
                                         0 (string-length line))
                        matches))
                 (else matches)))))))

(define* (grep+ pattern file #:key (matching 'basic) inverted?)
  (cond ((and (string? file)
              (not (equal? file "-"))) (call-with-input-file file
                                         (lambda (in)
                                           (grep* pattern #:port in #:file-name file #:matching matching #:inverted? inverted?))))
        (else (grep* pattern #:matching matching #:inverted? inverted?))))

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

(define (rmdir-p dir)
  "Remove directory DIR and all its ancestors."
  (rmdir dir)
  (let loop ((dir (dirname dir)))
    (when (not (equal? dir "."))
      (rmdir dir)
      (loop (dirname dir)))))

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

(define* (copy-file* source dest #:key force? verbose?)
  (when verbose?
    (format (current-error-port) "'~a' -> '~a'\n" source dest))
  (if (not force?) (copy-file source dest)
      (catch 'system-error
        (lambda _
          (copy-file source dest))
        (lambda (key func fmt msg errno . rest)
          (when (getenv "GASH_DEBUG")
            (format (current-error-port) "errno:~s\n" (car errno)))
          (match errno
            (((or 13 17))
             (delete-file dest)
             (copy-file source dest))
            (_ (throw key func fmt msg errno)))))))

(define* (copy-files #:optional files #:key force? verbose?)
  (match files
    ((source (and (? directory-exists?) dir))
     (copy-file* source (string-append dir "/" (basename source))
                 #:force? force #:verbose? verbose?))
    ((source dest)
     (copy-file* source dest #:force? force #:verbose? verbose?))
    ((sources ... dir)
     (unless (directory-exists? dir)
       (error (format #f "cp: target `~a' is not a directory\n" dir)))
     (for-each
      (cut copy-file* <> <> #:force? force? #:verbose? verbose?)
      sources
      (map (compose (cute string-append dir "/" <>) basename)
           sources)))))

(define* (link-file* source dest #:key force? verbose?)
  (when verbose?
    (format (current-error-port) "'~a' => '~a'\n" dest source))
  (if (not force?) (link source dest)
      (catch 'system-error
        (lambda _
          (link source dest))
        (lambda (key func fmt msg errno . rest)
          (when (getenv "GASH_DEBUG")
            (format (current-error-port) "errno:~s\n" (car errno)))
          (match errno
            (((or 13 17))
             (delete-file dest)
             (link source dest))
            (_ (throw key func fmt msg errno)))))))

(define* (link-files #:optional files #:key force? verbose?)
  (match files
    ((source (and (? directory-exists?) dir))
     (link-file* source (string-append dir "/" (basename source))
                 #:force? force #:verbose? verbose?))
    ((source dest)
     (link-file* source dest #:force? force #:verbose? verbose?))
    ((sources ... dir)
     (unless (directory-exists? dir)
       (error (format #f "cp: target `~a' is not a directory\n" dir)))
     (for-each
      (cut link-file* <> <> #:force? force? #:verbose? verbose?)
      sources
      (map (compose (cute string-append dir "/" <>) basename)
           sources)))))

(define* (symlink-file* source dest #:key force? verbose?)
  (when verbose?
    (format (current-error-port) "'~a' -> '~a'\n" dest source))
  (if (not force?) (symlink source dest)
      (catch 'system-error
        (lambda _
          (symlink source dest))
        (lambda (key func fmt msg errno . rest)
          (when (getenv "GASH_DEBUG")
            (format (current-error-port) "errno:~s\n" (car errno)))
          (match errno
            (((or 13 17))
             (delete-file dest)
             (symlink source dest))
            (_ (throw key func fmt msg errno)))))))

(define* (symlink-files #:optional files #:key force? verbose?)
  (match files
    ((source (and (? directory-exists?) dir))
     (symlink-file* source (string-append dir "/" (basename source))
                 #:force? force #:verbose? verbose?))
    ((source dest)
     (symlink-file* source dest #:force? force #:verbose? verbose?))
    ((sources ... dir)
     (unless (directory-exists? dir)
       (error (format #f "cp: target `~a' is not a directory\n" dir)))
     (for-each
      (cut symlink-file* <> <> #:force? force? #:verbose? verbose?)
      sources
      (map (compose (cute string-append dir "/" <>) basename)
           sources)))))

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

(define (file-class st)
  (case (stat:type st)
    ((regular)
     (if (zero? (logand (stat:perms st) #o111)) "" "*"))
    ((directory) "/")
    ((symlink) "@")
    (else "")))

(define* (display-file file-name #:optional st #:key classify?)
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
    (display " ")
    (display file-name)
    (cond ((eq? (stat:type st) 'symlink)
           (display " -> ")
           (let ((target (readlink file-name)))
             (display target)
             (when (and classify? (file-exists? target))
               (display (file-class (stat target))))))
          (classify? (display (file-class st))))))

(define (multi-opt options name)
  (let ((opt? (lambda (o) (and (eq? (car o) name) (cdr o)))))
    (filter-map opt? (reverse options))))

(define (mixed-multi-opt options names)
  (let ((opt? (lambda (o) (and (memq (car o) names) o))))
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


;;;
;;; Permissions.
;;;
(define-immutable-record-type <chmodifier>
  (make-chmodifier users operation permissions)
  chmodifier?
  (users chmodifier-users)
  (operation chmodifier-operation)
  (permissions chmodifier-permissions))

(define (parse-chmodifier o)
  (let* ((c (string->symbol (substring o 0 1)))
         (o (if (memq c '(- + =)) (string-append "a" o) o))
         (users (string->symbol (substring o 0 1)))
         (program (car (command-line))))
    (when (not (memq users '(u g o a)))
      (error (format #f "~a: no such user: ~a" program users)))
    (let ((operation (string->symbol (substring o 1 2))))
      (when (not (memq operation '(- + =)))
        (error (format #f "~a: no such operation: ~a" program operation)))
      (let* ((perm-string (substring o 2))
             (perm (string->number perm-string 8)))
        (if perm (make-numeric-chmodifier perm)
            (let ((perms (map string->symbol (string->string-list perm-string))))
              (make-chmodifier users operation perms)))))))

(define (parse-chmodifiers o)
  (or (and=> (string->number o 8) (compose list (cut make-numeric-chmodifier <>)))
      (map parse-chmodifier (string-split o #\,))))

(define (make-numeric-chmodifier o)
  (make-chmodifier 'o '= (list o)))

(define* (chmodifiers->mode modifiers #:optional (mode 0))
  (let loop ((modifiers modifiers) (mode mode))
    (if (null? modifiers) mode
        (loop (cdr modifiers)
              (chmodifier->mode (car modifiers) mode)))))

(define* (chmodifier->mode modifier #:optional (mode 0))
  (let* ((executable? (if (zero? (logand mode #o111)) 0 1))
         (n (chmodifier-numeric-mode modifier executable?))
         (o (chmodifier-operation modifier))
         (program (car (command-line))))
    (case o
      ((=) n)
      ((+) (logior mode n))
      ((-) (logand mode (logxor n -1)))
      (else (error
             (format #f
                     "~a: operation not supported: ~s\n"
                     program o))))))

(define (apply-chmodifiers file modifiers)
  (let ((mode (chmodifiers->mode modifiers (stat:mode (lstat file)))))
    ((@ (guile) chmod) file mode)))

(define (chmodifier-numeric-mode o executable?)
  (let* ((permissions (chmodifier-permissions o))
         (users (chmodifier-users o)))
    (let loop ((permissions permissions))
      (if (null? permissions) 0
          (+ (let* ((p (car permissions))
                    (base (cond ((number? p) p)
                                ((symbol? p)
                                 (case p
                                   ((r) 4)
                                   ((w) 2)
                                   ((x) 1)
                                   ((X) executable?))))))
               (case users
                 ((a) (+ base (ash base 3) (ash base 6)))
                 ((o) base)
                 ((g) (ash base 3))
                 ((u) (ash base 6))))
             (loop (cdr permissions)))))))

