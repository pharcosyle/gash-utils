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

(define-module (gash commands sort)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim))

;;; Commentary:

;;; Code:

(define* (read-lines/reversed #:optional (port (current-input-port)))
  (let loop ((line (read-line port)) (acc '()))
    (match line
      ((? eof-object?) acc)
      (_ (loop (read-line port) (cons line acc))))))

(define (sort name)
  (for-each (lambda (line)
              (display line)
              (newline))
            (sort! (read-lines/reversed) string-locale<?))
  EXIT_SUCCESS)

(define (main . args)
  (apply sort args))
