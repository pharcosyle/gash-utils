;;; Gash -- Guile As Shell
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gash bournish-commands)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (gash guix-build-utils)
  #:use-module (gash io)
  #:export (
            cat-command
            ls-command
            reboot-command
            rm-command
            wc-command
            which-command
            ))

;;; Commentary:

;;; This code is taken from (guix build bournish)

;;;
;;; This is a super minimal Bourne-like shell language for Guile.  It is meant
;;; to be used at the REPL as a rescue shell.  In a way, this is to Guile what
;;; eshell is to Emacs.
;;;
;;; Code:

(define (expand-variable str)
  "Return STR or code to obtain the value of the environment variable STR
refers to."
  ;; XXX: No support for "${VAR}".
  (if (string-prefix? "$" str)
      `(or (getenv ,(string-drop str 1)) "")
      str))

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

(define ls-command-implementation
  ;; Run-time support procedure.
  (case-lambda
    (()
     (display-tabulated (scandir ".")))
    (files
     (let ((files (append-map (lambda (file)
                                (catch 'system-error
                                  (lambda ()
                                    (match (stat:type (lstat file))
                                      ('directory
                                       ;; Like GNU ls, list the contents of
                                       ;; FILE rather than FILE itself.
                                       (match (scandir file
                                                       (match-lambda
                                                         ((or "." "..") #f)
                                                         (_ #t)))
                                         (#f
                                          (list file))
                                         ((files ...)
                                          (map (cut string-append file "/" <>)
                                               files))))
                                      (_
                                       (list file))))
                                  (lambda args
                                    (let ((errno (system-error-errno args)))
                                      (format (current-error-port) "~a: ~a~%"
                                              file (strerror errno))
                                      '()))))
                              files)))
       (display-tabulated files)))))

(define (ls-command . files)
  (apply ls-command-implementation files))

(define (which-command program . rest)
  (stdout (search-path (executable-path) program)))

(define (cat-command file . rest)
  (call-with-input-file file
    (lambda (port)
      (dump-port port (current-output-port))
      *unspecified*)))

(define (rm-command . args)
  "Emit code for the 'rm' command."
  (cond ((member "-r" args)
         (for-each delete-file-recursively
                   (apply delete (cons "-r" args))))
        (else
         (for-each delete-file args))))

(define (lines+chars port)
  "Return the number of lines and number of chars read from PORT."
  (let loop ((lines 0) (chars 0))
    (match (read-char port)
      ((? eof-object?)              ;done!
       (values lines chars))
      (#\newline                    ;recurse
       (loop (1+ lines) (1+ chars)))
      (_                            ;recurse
       (loop lines (1+ chars))))))

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

(define (wc-print file)
  (let-values (((lines chars)
                (call-with-input-file file lines+chars)))
              (format #t "~a ~a ~a~%" lines chars file)))

(define (wc-l-print file)
  (let-values (((lines chars)
                (call-with-input-file file lines+chars)))
              (format #t "~a ~a~%" lines file)))

(define (wc-c-print file)
  (let-values (((lines chars)
                (call-with-input-file file lines+chars)))
              (format #t "~a ~a~%" chars file)))

(define (wc-command-implementation . files)
  (for-each wc-print (filter file-exists?* files)))

(define (wc-l-command-implementation . files)
  (for-each wc-l-print (filter file-exists?* files)))

(define (wc-c-command-implementation . files)
  (for-each wc-c-print (filter file-exists?* files)))

(define (wc-command . args)
  "Emit code for the 'wc' command."
  (cond ((member "-l" args)
         (apply wc-l-command-implementation (delete "-l" args)))
        ((member "-c" args)
         (apply wc-c-command-implementation (delete "-c" args)))
        (else
         (apply wc-command-implementation args))))

(define (reboot-command . args)
  "Emit code for 'reboot'."
  ;; Normally Bournish is used in the initrd, where 'reboot' is provided
  ;; directly by (guile-user).  In other cases, just bail out.
  (if (defined? 'reboot)
      (reboot)
      (begin
        (format (current-error-port)
                "I don't know how to reboot, sorry about that!~%")
        1)))

(define %not-colon (char-set-complement (char-set #\:)))
(define (executable-path)
  "Return the search path for programs as a list."
  (match (getenv "PATH")
    (#f  '())
    (str (string-tokenize str %not-colon))))
