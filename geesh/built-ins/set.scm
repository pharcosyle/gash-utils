;;; The Geesh Shell Interpreter
;;; Copyright 2018 Timothy Sample <samplet@ngyro.com>
;;;
;;; This file is part of Geesh.
;;;
;;; Geesh is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Geesh is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Geesh.  If not, see <http://www.gnu.org/licenses/>.

(define-module (geesh built-ins set)
  #:use-module (geesh environment)
  #:use-module (ice-9 match))

;;; Commentary:
;;;
;;; The 'set' utility.
;;;
;;; Code:

(define (option? o)
  (memq o *option-names*))

(define (option-letter? chr)
  (assoc chr *option-letters*))

(define (set-option! option value args)
  (setopt! option value)
  (unless (null? args)
    (set-program-arguments (cons (car (program-arguments)) args))))

(define (main . args)
  (match args
    (("-o")
     (for-each (lambda (option)
                 (format #t "~a\t~a~%"
                         option (getopt option)))
               *option-names*)
     EXIT_SUCCESS)
    (("+o")
     (for-each (lambda (option)
                 (format #t "set ~a ~a~%"
                         (if (getopt option) "-o" "+o") option))
               *option-names*)
     EXIT_SUCCESS)
    (_ (let loop ((args args))
         (match args
           (() EXIT_SUCCESS)
           (("--" . args)
            (set-program-arguments (cons (car (program-arguments)) args))
            EXIT_SUCCESS)
           (("-o" option-string . args)
            (let ((option (string->symbol option-string)))
              (match option
                ((? option?)
                 (setopt! option #t)
                 (loop args))
                (_ (format (current-error-port)
                           "~a: set: invalid option ~a~%"
                           (car (program-arguments)) option)
                   EXIT_FAILURE))))
           (("+o" option-string . args)
            (let ((option (string->symbol option-string)))
              (match option
                ((? option?)
                 (setopt! option #f)
                 (loop args))
                (_ (format (current-error-port)
                           "~a: set: invalid option ~a~%"
                           (car (program-arguments)) option)
                   EXIT_FAILURE))))
           ((op . args)
            (match (string->list op)
              ((#\- (? option-letter? chr))
               (setopt! (assoc-ref *option-letters* chr) #t)
               (loop args))
              ((#\+ (? option-letter? chr))
               (setopt! (assoc-ref *option-letters* chr) #f)
               (loop args))
              (_ (loop (cons* "--" op args)))))
           (_ (format (current-error-port)
                      "~a: set: invalid options ~s~%"
                      (car (program-arguments)) args)
              EXIT_FAILURE))))))
