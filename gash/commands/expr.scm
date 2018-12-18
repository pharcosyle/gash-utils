;;; Gash --- Guile As SHell
;;; Copyright © 2018 Timothy Sample <samplet@ngyro.com>
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

(define-module (gash commands expr)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1))

(define-syntax-rule (invalid-options proc msg args ...)
  (scm-error 'invalid-options proc msg (list args ...) '()))

(define operators
  '(("|" . 0)
    ("&" . 1)
    ("=" . 2) (">" . 2) (">=" . 2) ("<" . 2) ("<=" . 2) ("!=" . 2)
    ("+" . 3) ("-" . 3)
    ("*" . 4) ("/" . 4) ("%" . 4)
    (":" . 5)))

(define (operator? x)
  (assoc x operators))

(define (operator-precedence op)
  (assoc-ref operators op))

(define (operator>=? a b)
  (>= (operator-precedence a) (operator-precedence b)))

(define (string->integer s)
  (receive (n count) (locale-string->integer s)
    (and (= count (string-length s)) n)))

(define (if-integers a b integers-proc strings-proc)
  (let ((n (if (integer? a) a (string->integer a)))
        (m (if (integer? b) b (string->integer b))))
    (if (and n m)
        (integers-proc n m)
        (let ((a (if (integer? a) (number->locale-string a) a))
              (b (if (integer? b) (number->locale-string b) b)))
          (strings-proc a b)))))

(define (assert-integers a b proc)
  (let ((n (if (integer? a) a (string->integer a)))
        (m (if (integer? b) b (string->integer b))))
    (unless (and n m)
      (invalid-options "assert-integers" "Not an integer" (if n b a)))
    (proc n m)))

(define (integer-and a b)
  (if (or (zero? a) (zero? b)) 0 a))

(define (string-and a b)
  (if (or (string-null? a) (string-null? b)) 0 a))

(define (bool->integer b)
  (if b 1 0))

(define (expr-apply op a b)
  (match op
    ("|" (match (string->integer a)
           (#f (if (string-null? a) b a))
           (n (if (zero? a) b a))))
    ("&" (if-integers a b integer-and string-and))
    ("=" (bool->integer (if-integers a b = string=?)))
    (">" (bool->integer (if-integers a b > string-locale>?)))
    (">=" (bool->integer (if-integers a b >= (negate string-locale<?))))
    ("<" (bool->integer (if-integers a b < string-locale<?)))
    ("<=" (bool->integer (if-integers a b <= (negate string-locale>?))))
    ("!=" (bool->integer (if-integers a b (negate =) (negate string=?))))
    ("+" (assert-integers a b +))
    ("-" (assert-integers a b -))
    ("*" (assert-integers a b *))
    ("/" (assert-integers a b quotient))
    ("%" (assert-integers a b remainder))
    (":" (let* ((a (if (integer? a) (number->locale-string a) a))
                (b (if (integer? b) (number->locale-string b) b))
                (rx (make-regexp (string-append "^" b) regexp/basic))
                (m (regexp-exec rx a)))
           (if m
               (if (> (match:count m) 1)
                   (or (match:substring m 1) "")
                   (- (match:end m) (match:start m)))
               0)))))

(define (apply-while pred ops vals)
  (let loop ((ops ops) (vals vals))
    (match ops
      (((? pred head) . tail)
       (match vals
         ((a b . vals*)
          
          (loop tail (cons (expr-apply head b a) vals*)))
         (_ (invalid-options "apply-while" "Invalid options"))))
      (_ (values ops vals)))))

(define (expr . args)
  (let loop ((args args) (start? #t) (vals '()) (ops '()))
    (match args
      ((head . tail)
       (if start?
           (match head
             ("(" (loop tail #t vals (cons 'open-bracket ops)))
             (_ (loop tail #f (cons head vals) ops)))
           (match head
             (")"
              (receive (ops vals) (apply-while operator? ops vals)
                (match ops
                  (('open-bracket . ops*) (loop tail #f vals ops*))
                  (_ (invalid-options "expr" "Mismatched brackets")))))
             ((? operator?)
              (receive (ops vals)
                  (apply-while (lambda (o)
                                 (and (operator? o)
                                      (operator>=? o head)))
                               ops vals)
                (loop tail #t vals (cons head ops))))
             (_ (invalid-options "expr" "Unknown operator: ~s" head)))))
      (() (receive (ops vals) (apply-while operator? ops vals)
            (match ops
              (() (match vals
                    ((result)
                     (format #t "~a~%" result)
                     (match result
                       ((or 0 "") #f)
                       (_ #t)))
                    (_ (invalid-options "expr" "Invalid options"))))
              (_ (invalid-options "expr" "Mismatched brackets"))))))))

(define (main . args)
  (let ((name (match args ((name . _) name) (_ "???")))
        (ags (if (null? args) args (cdr args))))
    (catch #t
      (lambda ()
        (primitive-exit (if (apply expr (cdr args))
                            EXIT_SUCCESS
                            EXIT_FAILURE)))
      (lambda args
        (match args
          (('invalid-options proc msg args data)
           (format (current-error-port) "~a: " name)
           (apply format (current-error-port) msg args)
           (newline (current-error-port))
           (exit 2))
          (('system-error proc msg args data)
           (format (current-error-port) "~a: " name)
           (display (strerror (car args)) (current-error-port)))
          (_ (format (current-error-port)
                     "~a: Caught exception: ~s" name args)))
        (newline (current-error-port))
        (exit 3)))))
