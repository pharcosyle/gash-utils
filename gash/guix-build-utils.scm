;;; Gash -- Guile As SHell
;;; Copyright © 2012, 2013, 2014, 2015, 2016, 2017, 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
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


(define-module (gash guix-build-utils)
  ;; #:use-module (srfi srfi-1)
  ;; #:use-module (srfi srfi-11)
  ;; #:use-module (srfi srfi-26)
  ;; #:use-module (srfi srfi-34)
  ;; #:use-module (srfi srfi-35)
  ;; #:use-module (srfi srfi-60)
  ;; #:use-module (ice-9 ftw)
  ;; #:use-module (ice-9 match)
  ;; #:use-module (ice-9 regex)
  ;; #:use-module (ice-9 rdelim)
  ;; #:use-module (ice-9 format)
  ;; #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:export (dump-port))

;;; Commentary:

;;; This code is taken from (guix build utils)

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
