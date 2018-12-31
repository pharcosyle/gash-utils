;;; Gash --- Guile As SHell
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

;;; Commentary:

;;; The initial bournish.scm was taken from Guix.

;;; Code:

(define-module (gash commands wc)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)

  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (gash shell-utils)
  #:export (
            wc
            ))

(define (lines+words+chars port)
  "Return the number of lines, words and chars read from PORT."
  (let loop ((lines 0) (words 0) (chars 0) (word? #f))
    (let ((c (read-char port)))
      (match c
       ((? eof-object?)                 ;done!
        (values lines words chars))
       (#\newline                       ;recurse
        (loop (1+ lines) words (1+ chars) #f))
       ((or #\return #\space #\tab #\vtab) ;recurse
        (loop lines words (1+ chars) #f))
       (_                               ;recurse
        (loop lines (if word? words (1+ words)) (1+ chars) #t))))))

(define (wc-print file)
  (let-values (((lines words chars)
                (if (string-null? file) (lines+words+chars (current-input-port))
                    (call-with-input-file file lines+words+chars))))
    (format #t "~7@a ~7@a ~7@a ~a~%" lines words chars file)))

(define (wc-l-print file)
  (let-values (((lines words chars)
                (if (string-null? file) (lines+words+chars (current-input-port))
                    (call-with-input-file file lines+words+chars))))
    (format #t "~a ~a~%" lines file)))

(define (wc-c-print file)
  (let-values (((lines words chars)
                (if (string-null? file) (lines+words+chars (current-input-port))
                    (call-with-input-file file lines+words+chars))))
    (format #t "~a ~a~%" chars file)))

(define (wc-w-print file)
  (let-values (((lines words chars)
                (if (string-null? file) (lines+words+chars (current-input-port))
                    (call-with-input-file file lines+words+chars))))
    (format #t "~7@a ~a~%" words file)))

(define (wc- . files)
  (if (null? files) (wc-print "")
      (for-each wc-print (filter file-exists?* files))))

(define (wc-l . files)
  (if (null? files) (wc-l-print "")
   (for-each wc-l-print (filter file-exists?* files))))

(define (wc-c . files)
  (if (null? files) (wc-c-print "")
      (for-each wc-c-print (filter file-exists?* files))))

(define (wc-w . files)
  (if (null? files) (wc-w-print "")
      (for-each wc-w-print (filter file-exists?* files))))

(define (wc name . args)
  (cond ((member "-l" args)
         (apply wc-l (delete "-l" args)))
        ((member "-w" args)
         (apply wc-w (delete "-w" args)))
        ((member "-c" args)
         (apply wc-c (delete "-c" args)))
        (else
         (apply wc- args))))

(define main wc)
