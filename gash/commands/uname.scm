;;; Gash-Utils
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define *options-grammar*
  (make-options-grammar
   `((flag all #\a)
     (flag hardware-platform #\i)
     (flag machine #\m)
     (flag node-name #\n)
     (flag operating-system #\o)
     (flag processor #\p)
     (flag kernel-release #\r)
     (flag kernel-name #\s)
     (flag kernel-version #\v)
     (message ("help" #\h) ,*help-message*)
     (message ("version" #\V) ,*version-message*))
   #:default
   (lambda (arg result)
     (format (current-error-port) "uname: Invalid argument: ~a~%" arg)
     (exit 1))))

(define (uname . args)
  (let* ((options (parse-options args *options-grammar*))
         (all? (or (assoc-ref options 'all) #f))
         (kernel-name? (or (assoc-ref options 'kernel-name) #f))
         (node-name? (or (assoc-ref options 'node-name) #f))
         (kernel-release? (or (assoc-ref options 'kernel-release) #f))
         (kernel-version? (or (assoc-ref options 'kernel-version) #f))
         (machine? (or (assoc-ref options 'machine) #f))
         (processor? (or (assoc-ref options 'processor) #f))
         (hardware-platform? (or (assoc-ref options 'hardware-platform) #f))
         (operating-system? (or (assoc-ref options 'operating-system) #f))
         (kernel-name? (not (or all? kernel-name? node-name? kernel-release? kernel-version? machine? processor? hardware-platform? operating-system?)))
         (un ((@ (guile) uname)))
           (output (filter
                    identity
                    (list (and (or all? kernel-name?)
                               (format #f "~a" (utsname:sysname un)))
                          (and (or all? node-name?)
                               (format #f "~a" (utsname:nodename un)))
                          (and (or all? kernel-release?)
                               (format #f "~a" (utsname:release un)))
                          (and (or all? kernel-version?)
                               (format #f "~a" (utsname:version un)))
                          (and (or all? machine?)
                               (format #f "~a" (utsname:machine un)))
                          (and processor?
                               (format #f "~a" "unknown"))
                          (and hardware-platform?
                               (format #f "~a" "unknown"))
                          (and (or all? operating-system?)
                               (format #f "GNU/~a" (utsname:sysname un)))))))
    (display (string-join output))
    (newline)))

(define main uname)
