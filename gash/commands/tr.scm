;;; Gash -- Guile As SHell
;;; Copyright © 2018 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;;
;;; This file is part of Gash.
;;;
;;; Gash is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Gash is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Gash.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(define-module (gash commands tr)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gash commands config)
  #:use-module (gash util)
  #:export (
            tr
            ))

(define (gfilter pred gen)
  (lambda ()
    (let loop ()
      (match (gen)
        ((? eof-object? eof) eof)
        (x (if (pred x) x (loop)))))))

(define (gremove pred gen)
  (gfilter (negate pred) gen))

(define (gfor-each proc gen)
  (let loop ()
    (match (gen)
      ((? eof-object?) #t)
      (x (proc x) (loop)))))

(define* (gsqueeze pred gen #:optional (= equal?))
  (let ((previous #f)
        (previous-set? #f))
    (lambda ()
      (let loop ()
        (match (gen)
          ((? eof-object? eof) eof)
          (x (if (and previous-set? (= previous x) (pred x))
                 (loop)
                 (begin
                   (set! previous-set? #t)
                   (set! previous x)
                   x))))))))

(define* (delete-chars gen s #:optional squeeze)
  (let ((deleted (gremove (cut string-index s <>) gen)))
    (if squeeze
        (gsqueeze (cut string-index squeeze <>) deleted char=?)
        deleted)))

(define* (translate-chars gen s1 s2 #:key squeeze?)
  (let ((translated (lambda ()
                      (match (gen)
                        ((? eof-object? eof) eof)
                        (chr (let ((index (string-index s1 chr)))
                               (if index
                                   (string-ref s2 index)
                                   chr)))))))
    (if squeeze?
        (gsqueeze (cut string-index s2 <>) translated char=?)
        translated)))

(define read-escape
  (let ((octal-inputs "01234567")
        (escape-inputs "\\abfnrtv")
        (escape-outputs "\\\a\b\f\n\r\t\v"))
    (lambda* (s #:optional (start 0) (end (string-length s)))
      (let loop ((index start) (code #f))
        (let ((chr (and (< index end) (string-ref s index)))
              (return (lambda () (values (and code (integer->char code))
                                         (- index start)))))
          (cond
           ((not chr) (return))
           ((string-index octal-inputs chr)
            => (lambda (i) (loop (1+ index) (+ (* 8 (or code 0)) i))))
           ((and (not code) (string-index escape-inputs chr))
            => (lambda (i)
                 (values (string-ref escape-outputs i)
                         (- (1+ index) start))))
           (else (return))))))))

(define* (read-char-array s #:optional (start 0) (end (string-length s)))
  (let loop ((index start) (acc '()))
    (match (and (< index end) (string-ref s index))
      (#f (reverse-list->string acc))
      (#\\ (receive (chr count) (read-escape s (1+ index) end)
             (match chr
               (#f (error "TR: Bad backslash escape"))
               (_ (loop (+ index 1 count) (cons chr acc))))))
      (#\[
       (let* ((chr (string-ref s (1+ index)))
              (acc (if (eq? chr #\-) (cons chr acc) acc))
              (index (if (eq? chr #\-) (1+ index) index)))
         (let loop2 ((index (1+ index)) (acc acc))
           (if (or (>= index end)
                   (eq? (string-ref s index) #\]))
               (loop (1+ index) acc)
               (let* ((chr (string-ref s index)))
                 (if (eq? chr #\-)
                     (let* ((index (1+ index))
                            (start (1+ (char->integer (last acc))))
                            (end (char->integer (string-ref s index)))
                            (count (1+ (- end start))))
                       (loop2 (1+ index) (append
                                          (map integer->char
                                               (iota count end -1)) acc)))
                     (loop2 (1+ index) (cons chr acc))))))))
      (chr (loop (1+ index) (cons chr acc))))))

(define (tr . args)
  (let* ((option-spec
	  '((delete (single-char #\d))
            (squeeze-repeats (single-char #\s))
            (help (single-char #\h))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
         (delete? (option-ref options 'delete #f))
         (squeeze? (option-ref options 'squeeze-repeats #f))
         (files (option-ref options '() '()))
	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
         (usage? (and (not help?) (or (< (length files) 1)
                                      (> (length files) 2)))))
    (cond (version? (format #t "tr (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: tr [OPTION]... SET1 [SET2]

Options:
  -d, --delete           delete characters in SET1, do not translate
  -s, --squeeze-repeats  delete repeats of characters in the last SET
  -h, --help             display this help and exit
  -V, --version          display version information and exit
")
           (exit (if usage? 2 0)))
          (delete?
           (when (and (not squeeze?) (pair? (cdr files)))
             (error "TR: With -d but not -s, SET2 must be omitted"))
           (let* ((s (read-char-array (car files)))
                  (squeeze (if (and squeeze? (pair? (cdr files)))
                               (read-char-array (cadr files))
                               #f)))
             (gfor-each write-char
                        (delete-chars read-char s squeeze))
             (exit 0)))
          ((pair? (cdr files))
           (let* ((s1 (read-char-array (car files)))
                  (s2 (read-char-array (cadr files))))
             (gfor-each write-char
                        (translate-chars read-char s1 s2
                                         #:squeeze? squeeze?))
             (exit 0)))
          (squeeze?
           (let ((s (read-char-array (car files))))
             (gfor-each write-char
                        (gsqueeze (cut string-index s <>) read-char char=?))))
          (else
           (error "TR: With only one SET, -d or -s must be specified")))))

(define main tr)
