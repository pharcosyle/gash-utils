;;; Gash-Utils
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Timothy Sample <samplet@ngyro.com>
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

;;; Code:

(define-module (gash commands uname)
  #:use-module (gash commands config)
  #:use-module (gash-utils options)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (uname))

(define *help-message* "\
Usage: uname [OPTION]...
Print certain system information.  With no OPTION, same as -s.

Options:
  -a, --all                print all information, in the following order,
                             except omit -p and -i if unknown:
  -s, --kernel-name        print the kernel name
  -n, --nodename           print the network node hostname
  -r, --kernel-release     print the kernel release
  -v, --kernel-version     print the kernel version
  -m, --machine            print the machine hardware name
  -p, --processor          print the processor type (non-portable)
  -i, --hardware-platform  print the hardware platform (non-portable)
  -o, --operating-system   print the operating system
      --help               display this help and exit
      --version            output version information and exit
")

(define *version-message*
  (format #f "uname (~a) ~a~%" %package-name %version))

(define (utsname:osname un)
  (match (utsname:sysname un)
    ("Linux" "GNU/Linux")
    (x x)))

(define *fields*
  ;; all?  key                flag  accessor
  `((#t    kernel-name        #\s   ,utsname:sysname)
    (#t    node-name          #\n   ,utsname:nodename)
    (#t    kernel-release     #\r   ,utsname:release)
    (#t    kernel-version     #\v   ,utsname:version)
    (#t    machine            #\m   ,utsname:machine)
    (#f    processor          #\p   ,(const #f))
    (#f    hardware-platform  #\i   ,(const #f))
    (#t    operating-system   #\o   ,utsname:osname)))

(define *options-grammar*
  (make-options-grammar
   `((flag all #\a)
     ,@(map (match-lambda
              ((_ key chr _) `(flag ,key ,chr)))
            *fields*)
     (message ("help" #\h) ,*help-message*)
     (message ("version" #\V) ,*version-message*))
   #:default
   (lambda (arg result)
     (format (current-error-port) "uname: Invalid argument: ~a~%" arg)
     (exit 1))))

(define (get-specified-fields options)
  (let ((need-all? (assoc-ref options 'all))
        (un ((@ (guile) uname))))
    (filter-map (match-lambda
                  ((all? key _ get)
                   (and (or (and need-all? all?)
                            (assoc-ref options key))
                        (or (get un) "unknown"))))
                *fields*)))

(define (uname . args)
  (let* ((options (match (parse-options args *options-grammar*)
                    (() '((kernel-name . #t)))
                    (xs xs)))
         (specified-fields (get-specified-fields options)))
    (display (string-join specified-fields))
    (newline)))

(define main uname)
