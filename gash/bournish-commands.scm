;;; Gash --- Guile As SHell
;;; Copyright © 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
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

;;; Commentary:

;;; The initial bournish.scm was taken from Guix.

;;; Code:

(define-module (gash bournish-commands)
  #:use-module (srfi srfi-26)
  #:use-module (gash io)
  #:use-module (gash config)
  #:use-module (gash shell-utils)

  #:use-module (gash commands cat)
  #:use-module (gash commands compress)
  #:use-module (gash commands cp)
  #:use-module (gash commands find)
  #:use-module (gash commands grep)
  #:use-module (gash commands ls)
  #:use-module (gash commands reboot)
  #:use-module (gash commands rm)
  #:use-module (gash commands sed)
  #:use-module (gash commands tar)
  #:use-module (gash commands touch)
  #:use-module (gash commands wc)
  #:use-module (gash commands which)

  #:export (
            %bournish-commands
            cat-command
            compress-command
            cp-command
            find-command
            grep-command
            ls-command
            reboot-command
            rm-command
            sed-command
            rm-command
            wc-command
            which-command
            ))

(define (wrap-command name command)
  (lambda args
    (lambda _
      (catch #t
        (cut apply command (cons name args))
        (lambda (key . args)
          (format (current-error-port) "~a: ~a ~a\n" name key args)
          (case key
            ((quit) (car args))
            (else 1)))))))

(define cat-command (wrap-command cat "cat"))
(define compress-command (wrap-command "compress" compress))
(define cp-command (wrap-command "cp" cp))
(define find-command (wrap-command "find" find))
(define grep-command (wrap-command "grep" grep))
(define ls-command (wrap-command "ls" ls))
(define mkdir-command (wrap-command "mkdir" mkdir))
(define reboot-command (wrap-command "reboot" reboot'))
(define rm-command (wrap-command "rm" rm))
(define rmdir-command (wrap-command "rmdir" rmdir))
(define sed-command (wrap-command "sed" sed))
(define tar-command (wrap-command "tar" tar))
(define touch-command (wrap-command "touch" touch))
(define wc-command (wrap-command "wc" wc))
(define which-command (wrap-command "which" which))

(define (%bournish-commands)
  `(
    ("cat"      . ,cat-command)
    ("compress" . ,compress-command)
    ("cp"       . ,cp-command)
    ("find"     . ,find-command)
    ("grep"     . ,grep-command)
    ("ls"       . ,ls-command)
    ("mkdir"    . ,mkdir)
    ("reboot"   . ,reboot-command)
    ("rm"       . ,rm-command)
    ("rmdir"    . ,rmdir-command)
    ("sed"      . ,sed-command)
    ("tar"      . ,tar-command)
    ("touch"    . ,touch-command)
    ("wc"       . ,wc-command)
    ("which"    . ,which-command)
    ))
