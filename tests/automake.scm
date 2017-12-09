;;; The Geesh Shell Interpreter
;;; Copyright 2017 Timothy Sample <samplet@ngyro.com>
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

(define-module (tests automake)
  #:use-module (srfi srfi-64))

;;; Commentary:
;;;
;;; This module sets up the SRFI 64 test system so that it plays well
;;; with automake's test system.
;;;
;;; Code:

(test-runner-factory
 (lambda ()
   (let ((runner (test-runner-simple)))
     (test-runner-aux-value! runner (current-output-port))
     (test-runner-on-group-begin! runner
       (lambda (runner suite-name count)
         (let ((log (test-runner-aux-value runner)))
           (when (output-port? log)
             (when (null? (test-runner-group-stack runner))
               (display "%%%% Starting test " log)
               (display suite-name log)
               (newline log))
             (display "Group begin: " log)
             (display suite-name log)
             (newline log)))))
     (test-runner-on-final! runner
       (lambda (runner)
         (let ((log (test-runner-aux-value runner)))
           (when (output-port? log)
             (for-each (lambda (value-label-pair)
                         (when (> (car value-label-pair) 0)
                           (display (cdr value-label-pair) log)
                           (display (car value-label-pair) log)
                           (newline log)))
                       `((,(test-runner-pass-count runner)
                          . "# of expected passes      ")
                         (,(test-runner-xfail-count runner)
                          . "# of expected failures    ")
                         (,(test-runner-xpass-count runner)
                          . "# of unexpected successes ")
                         (,(test-runner-fail-count runner)
                          . "# of unexpected failures  ")
                         (,(test-runner-skip-count runner)
                          . "# of skipped tests        "))))
           (when (or (> (test-runner-fail-count runner) 0)
                     (> (test-runner-xpass-count runner) 0))
             (exit EXIT_FAILURE)))))
     runner)))
