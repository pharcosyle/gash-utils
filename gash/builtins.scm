;;; Gash -- Guile As SHell
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

(define-module (gash builtins)
  #:use-module (ice-9 match)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash bournish-commands)
  #:use-module (gash config)
  #:use-module (gash environment)
  #:use-module (gash gash)
  #:use-module (gash job)
  #:use-module (gash io)
  ;;#:use-module (gash peg)

  #:export (
            %builtin-commands
            PATH-search-path
            bg-command
            cd-command
            echo-command
            exit-command
            fg-command
            help-command
            pwd-command
            set-command
            ))
 
(define (PATH-search-path program)
  (search-path (string-split (getenv "PATH") #\:) program))

(define (cd-command . args)
  (match args
    (() (chdir (getenv "HOME")))
    ((dir)
     (chdir dir))
    ((args ...)
     (format (current-error-port) "cd: too many arguments: ~a\n" (string-join args)))))

(define (echo-command . args)
  (match args
    (() (newline))
    (("-n" args ...) (map display args))
    (_ (map display args) (newline))))

(define (bg-command . args)
  (match args
    (() (bg 1))
    ((job x ...) (bg (string->number (car job))))))

(define (fg-command . args)
  (match args
    (() (fg 1))
    ((job x ...) (fg (string->number (car job))))))

(define pwd-command (lambda _ (stdout (getcwd))))

(define (set-command . args) ;; TODO export; env vs set
  (define (display-var o)
    (format #t "~a=~a\n" (car o) (cdr o)))
  (match args
    (() (for-each display-var global-variables))
    (("-e") (set-shell-opt! "errexit" #t))
    (("+e") (set-shell-opt! "errexit" #f))
    (("-x") (set-shell-opt! "xtrace" #t))
    (("+x") (set-shell-opt! "xtrace" #f))))

(define (exit-command . args)
  (match args
    (() (exit 0))
    ((status)
     (exit (string->number status)))
    ((args ...)
     (format (current-error-port) "exit: too many arguments: ~a\n" (string-join args)))))

(define (help-command . _)
  (display "\
Hello, this is GASH, Guile As SHell.

GASH is work in progress; many language constructs work, globbing
mostly works, pipes work, some redirections work.
")
  (when (or %prefer-builtins? (not (PATH-search-path "ls")))
    (display "\nIt features the following, somewhat naive builtin commands\n")
    (display-tabulated (map car %builtin-commands))))

(define (cp-command-implementation source dest . rest)
  (copy-file source dest))

(define cp-command (wrap-command cp-command-implementation "cp"))

(define %builtin-commands
  `(
    ("bg"     . ,bg-command)
    ("cat"    . ,cat-command)
    ("cd"     . ,cd-command)
    ("cp"     . ,cp-command)
    ("echo"   . ,echo-command)
    ("exit"   . ,exit-command)
    ("fg"     . ,fg-command)
    ("help"   . ,help-command)
    ("jobs"   . ,jobs-command)
    ("ls"     . ,ls-command)
    ("pwd"    . ,pwd-command)
    ("reboot" . ,reboot-command)
    ("rm"     . ,rm-command)
    ("set"    . ,set-command)
    ("wc"     . ,wc-command)
    ("which"  . ,which-command)
    ))
