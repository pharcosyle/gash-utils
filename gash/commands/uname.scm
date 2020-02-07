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
  #:use-module (ice-9 getopt-long)
  #:use-module (gash commands config)
  #:export (uname))

(define (uname . args)
  (let* ((option-spec
	  '((all (single-char #\a))
            (kernel-name (single-char #\s))
            (node-name (single-char #\n))
            (kernel-release (single-char #\r))
            (kernel-version (single-char #\v))
            (machine (single-char #\m))
            (processor (single-char #\p))
            (hardware-platform (single-char #\i))
            (operating-system (single-char #\o))

            (help (single-char #\h))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
         (all? (option-ref options 'all #f))
         (kernel-name? (option-ref options 'kernel-name #f))
         (node-name? (option-ref options 'node-name #f))
         (kernel-release? (option-ref options 'kernel-release #f))
         (kernel-version? (option-ref options 'kernel-version #f))
         (machine? (option-ref options 'machine #f))
         (processor? (option-ref options 'processor #f))
         (hardware-platform? (option-ref options 'hardware-platform #f))
         (operating-system? (option-ref options 'operating-system #f))
         (kernel-name? (not (or all? kernel-name? node-name? kernel-release? kernel-version? machine? processor? hardware-platform? operating-system?)))

	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
	 (files (option-ref options '() '()))
         (usage? (and (not help?) (pair? files))))
    (cond (version? (format #t "uname (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
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
           (exit (if usage? 2 0)))
          (else
           (let* ((un ((@ (guile) uname)))
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
             (newline))))))

(define main uname)
