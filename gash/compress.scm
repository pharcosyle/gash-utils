;;; Gash-Utils
;;; Copyright © 2013 Daniel Hartwig <mandyke@gmail.com>
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

;;; The initial lzw.scm was taken from the Guile100 challenge
;;; https://github.com/spk121/guile100 from a contribution by Daniel
;;; Hartwig.

;;; Code:

(define-module (gash compress)
  #:use-module (gash compat)
  #:use-module (gash lzw)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-37)
  #:export (compress-file
            compress-port
            uncompress-file
            uncompress-port))

(define *program-name* "compress (GASH)")

(define (_ msg . rest)
  msg)

(define (error* status msg . args)
  (force-output)
  (let ((port (current-error-port)))
    (when *program-name*
      (display *program-name* port)
      (display ": " port))
    (apply format port msg args)
    (newline port)
    (unless (zero? status)
      ;; This call to 'abort' causes 'main' to immediately return the
      ;; specified status value.  Similar to 'exit' but more
      ;; controlled, for example, when using the REPL to debug,
      ;; 'abort' will not cause the entire process to terminate.
      ;;
      ;; This is also handy to attempt processing every file, even
      ;; after an error has occured.  To do this, establish another
      ;; prompt at an interesting place inside 'main'.
      (abort (lambda (k)
               status)))))

(define (make-file-error-handler filename)
  (lambda args
    (error* 1 (_ "~a: ~a")
            filename
            (strerror (system-error-errno args)))))

(define (system-error-handler key subr msg args rest)
  (apply error* 1 msg args))

(define (compression-ratio nbytes-in nbytes-out)
  (exact->inexact (/ (- nbytes-in nbytes-out) nbytes-in)))

(define (write-lzw-header port bits)
  (put-bytevector port (u8-list->bytevector (list #x1F #x9D bits))))

(define (compress-port in out bits verbose?)
  (set-port-encoding! in "ISO-8859-1")
  (set-port-encoding! out "ISO-8859-1")
  #;
  (begin
    (write-lzw-header out bits)
    (%lzw-compress (cute get-u8 in)
                   (cute put-u16 out <>)
                   eof-object?
                   (expt 2 bits)))
  (let* ((in-bv (get-bytevector-all in))
         (out-bv (lzw-compress in-bv #:table-size (expt 2 bits))))
    (write-lzw-header out bits)
    (put-bytevector out out-bv)))

(define (compress-file infile bits verbose?)
  (catch 'system-error
    (lambda ()
      (let ((outfile (string-append infile ".Z")))
        (when (string-suffix? ".Z" infile)
          (error* 1 (_ "~a: already has .Z suffix") infile))
        (when (file-exists? outfile)
          (error* 1 (_ "~a: already exists") outfile))
        (let ((in (open-file infile "rb"))
              (out (open-file outfile "wb")))
            ;; TODO: Keep original files ownership, modes, and access
          ;; and modification times.
          (compress-port in out bits verbose?)
          (when verbose?
            (format #; (current-error-port)
                    (current-output-port)
                    (_ "~a: compression: ~1,2h%\n") ; '~h' is localized '~f'.
                    infile
                    (* 100 (compression-ratio (port-position in)
                                              (port-position out)))))
          (for-each close-port (list in out))
          (delete-file infile))))
    system-error-handler))

(define (read-lzw-header port)
  (match (bytevector->u8-list (get-bytevector-n port 3))
    ((#x1F #x9D bits)
     (and (<= 9 bits 16)
          (values bits)))
    (x #f)))

(define (uncompress-port in out verbose?)
  (set-port-encoding! in "ISO-8859-1")
  (set-port-encoding! out "ISO-8859-1")
  (let ((bits (read-lzw-header in)))
    (unless bits
      (error* 1 (_ "incorrect header")))
    #;
    (%lzw-uncompress (cute get-u16 in)
                     (cute put-u8 out <>)
                     eof-object?
                     (expt 2 bits))
    (let* ((in-bv (get-bytevector-all in))
           (out-bv (lzw-uncompress in-bv #:table-size (expt 2 bits))))
      (put-bytevector out out-bv))))

(define (uncompress-file infile verbose?)
  (catch 'system-error
    (lambda ()
      (let ((outfile (string-drop-right infile 2)))
        (when (not (string-suffix? ".Z" infile))
          (error* 1 (_ "~a: does not have .Z suffix") infile))
        (when (file-exists? outfile)
          (error* 1 (_ "~a: already exists") outfile))
        (let ((in (open-file infile "rb"))
              (out (open-file outfile "wb")))
          (uncompress-port in out verbose?)
          (when verbose?
            (format #; (current-error-port)
                    (current-output-port)
                    (_ "~a: compression: ~1,2h%\n") ; '~h is localized '~f'.
                    infile
                    (* 100 (compression-ratio (port-position out)
                                              (port-position in)))))
          (for-each close-port (list in out))
          (delete-file infile))))
    system-error-handler))
