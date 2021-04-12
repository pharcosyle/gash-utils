;;; Gash-Utils
;;; Copyright © 2022 Timothy Sample <samplet@ngyro.com>
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
;;; along with Gash-Utils.  If not, see <http://www.gnu.org/licenses/>.

(define-module (test-file-formats)
  #:use-module (gash-utils file-formats)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-64)
  #:use-module (tests unit automake))

;;; Commentary:
;;;
;;; Tests for the file formats module.
;;;
;;; Code:

(define conversion-adapter
  (make-conversion-adapter
   (lambda (s seed)
     (unless (string? s)
       (error "Invalid format argument type (expecting string)" s))
     (values s seed))
   (lambda (n seed)
     (unless (number? (pk n))
       (error "Invalid format argument type (expecting number)" n))
     (values n seed))
   (lambda (c seed)
     (unless (char? c)
       (error "Invalid format argument type (expecting character)" c))
     (values c seed))))

(define (sprintf format-string . args)
  (let ((format (parse-file-format format-string)))
    (receive (result _)
        (apply fold-file-format conversion-adapter #t format args)
      result)))

(test-begin "file-formats")

(test-equal "Formats as string"
  "hello world"
  (sprintf "hello %s" "world"))

(test-equal "Formats as integer"
  "42"
  (sprintf "%d" 42))

(test-equal "Integers respect the '+' flag"
  "+64"
  (sprintf "%+d" 64))

(test-equal "The '+' flag does nothing to negative numbers"
  "-64"
  (sprintf "%+d" -64))

(test-equal "Integers respect the ' ' flag"
  " 64"
  (sprintf "% d" 64))

(test-equal "Formats as unsigned"
  "42"
  (sprintf "%u" 42))

(test-equal "Formats as hexadecimal"
  "2a"
  (sprintf "%x" 42))

(test-equal "Formats as hexadecimal (capital)"
  "2A"
  (sprintf "%X" 42))

(test-equal "Formats as hexadecimal (with '#' flag)"
  "0x2a"
  (sprintf "%#x" 42))

(test-equal "Formats an integer with precision"
  "005"
  (sprintf "%.3d" 5))

(test-equal "Formats an integer with precision (overflow)"
  "1234"
  (sprintf "%.3d" 1234))

(test-equal "Formats zero with zero precision"
  ""
  (sprintf "%.0d" 0))

(test-equal "Formats with precision and flags"
  "0x00f"
  (sprintf "%#.3x" 15))

(test-equal "Formats an integer with width"
  "  42"
  (sprintf "%4d" 42))

(test-equal "Formats a negative integer with width"
  " -42"
  (sprintf "%4d" -42))

(test-equal "Formats a negative integer with 0-padded width"
  "-042"
  (sprintf "%04d" -42))

(test-equal "Formats an integer with multiple options"
  "  +00042"
  (sprintf "%+08.5d" 42))

(test-equal "Pads a string on the left"
  "    left-pad"
  (sprintf "%12s" "left-pad"))

(test-equal "Pads a string on the right"
  "right-pad   "
  (sprintf "%-12s" "right-pad"))

(test-equal "Escapes a '%'"
  "%"
  (sprintf "%%"))

(test-end "file-formats")
