;;; Gash -- Guile As SHell
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

;;; Code:

(define-module (gash commands awk)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash config)
  #:use-module (gash util)
  #:use-module (gash commands awk lexer)
  #:use-module (gash commands awk parser)

  #:export (
            awk
            ))

(use-modules )

(define (delete-var name variables)
  (filter (negate (compose (cut eq? <> name) car)) variables))

(define (assign name value variables)
  (acons name value (delete-var name variables)))

(define (assign-array name index value variables)
  (let* ((array (or (assoc-ref variables name) '()))
         (array (assign index value array)))
    (assign name array variables)))

(define (get-var name variables)
  (or (assoc-ref variables name) ""))

(define (awk-not x variables)
  (match x
    (#t 0)
    (#f 1)
    (0 1)
    ((? number?) 0)
    ("" 1)
    ((? string?) 1)))

(define (awk-expression->string expression)
  (match expression
    ((? string?) expression)
    ((? number?) (number->string expression))
    (#t "1")
    (#f "0")
    ((lst ...) (string-join (map awk-expression->string lst) ""))))

(define (awk-expression expression variables)
  (match expression
    (('<awk-name> name) (get-var name variables))
    (('<awk-array-ref> name index) (get-var (awk-expression index variables) (get-var name variables)))
    (('<awk-in> index name) (and (assoc-ref (get-var name variables) index)))
    (('<awk-field> ('<awk-name> "NF")) (last (get-var "*fields*" variables)))
    (('<awk-field> number) (let ((field (awk-expression number variables))
                                 (fields (get-var "*fields*" variables))
                                 (line (get-var "*line*" variables)))
                             (cond ((zero? field) line)
                                   ((>= field (length fields)) "")
                                   (else (list-ref fields (1- field))))))
    ((? number?) expression)
    ((? string?) expression)
    (('+ x y) (+ (awk-expression x variables) (awk-expression y variables)))
    (('- x y) (- (awk-expression x variables) (awk-expression y variables)))
    (('* x y) (* (awk-expression x variables) (awk-expression y variables)))
    (('/ x y) (/ (awk-expression x variables) (awk-expression y variables)))
    (('&& x y) (and (awk-expression x variables) (awk-expression y variables)))
    (('|| x y) (or (awk-expression x variables) (awk-expression y variables)))
    (('== x y) (equal? (awk-expression x variables) (awk-expression y variables)))
    (('!= x y) (not (equal? (awk-expression x variables) (awk-expression y variables))))
    (('= ('<awk-name> x) y) (assign x (awk-expression y variables) variables))
    (('= ('<awk-array-ref> name index) value) (assign-array name (awk-expression index variables) (awk-expression value variables) variables))
    (('! x) (awk-not (awk-expression x variables) variables))
    ((lst ...) (awk-expression->string (map (cut awk-expression <> variables) lst)))))

(define (awk-expression->boolean expression variables)
  (match expression
    ((? number?) (not (zero? expression)))
    ((? string?) (not (string-null? expression)))
    ((? boolean?) expression)
    (_ (awk-expression->boolean (awk-expression expression variables) variables))))

(define (awk-pattern pattern variables)
  (match pattern
    (('<awk-pattern> expression) (awk-expression expression variables))))

(define (run-commands inport outport fields command variables)
  (match command
    (('<awk-item> ('<awk-pattern> pattern) action)
     (if (awk-expression->boolean pattern variables) (run-commands inport outport fields action variables)
         variables))
    (('<awk-action> actions ...)
     (fold (cut run-commands inport outport fields <> <>) variables actions))
    (('<awk-expr> expr)
     (awk-expression expr variables))
    (('<awk-print>)
     (let ((count  (min (get-var "NF" variables) (length fields))))
       (display (string-join (list-head fields count))))
     (newline)
     variables)
    (('<awk-print> expr)
     (display (awk-expression expr variables)) (newline)
     variables)
    (('<awk-print> expr ..1)
     (display (awk-expression expr variables)) (newline)
     variables)
    (('<awk-for-in> key array action)
     (format (current-error-port) "action=~s\n" action)
     (let* ((save-key (assoc-ref variables key))
            (keys (map car (get-var array variables)))
            (variables (fold (lambda (value variables)
                               (run-commands inport outport fields action `((,key . ,value) ,@variables)))
                             variables
                             keys)))
       (if save-key (assign key save-key variables)
           (delete-var key variables))))
    (('<awk-if> expr then)
     (if (awk-expression->boolean expr variables) (run-commands inport outport fields then variables)
         variables))
    (('<awk-if> expr then else)
     (if (awk-expression->boolean expr variables)
         (run-commands inport outport fields then variables)
         (run-commands inport outport fields else variables)))
    ((lst ...) (fold (cut run-commands inport outport fields <> <>) variables lst))
    (_ (format (current-error-port) "skip: ~s\n" command)
       variables)))

(define* (run-awk-file program outport file-name variables)
  (let ((inport (if (equal? file-name "-") (current-input-port)
                    (open-input-file file-name))))
    (let loop ((line (read-line inport)) (variables `(("FILENAME" . ,file-name)
                                                      ("FNR" . 0)
                                                      ,@variables)))
      (if (eof-object? line) variables
        (let* ((fields (string-split line (get-var "FS" variables)))
               (variables (assign "NF" (length fields) variables))
               (variables (assign "NR" (1+ (get-var "NR" variables)) variables))
               (variables (assign "FNR" (1+ (get-var "FNR" variables)) variables))
               (variables (assign "*line*" line variables))
               (variables (assign "*fields*" fields variables)))
          (let ((variables (run-commands inport outport fields program variables)))
            (loop (read-line inport) variables)))))))

(define (begin-block x)
  (match x
    (('<awk-item> ('<awk-begin>) action) action)
    (_ #f)))

(define (end-block x)
  (match x
    (('<awk-item> ('<awk-end>) action) action)
    (_ #f)))

(define (awk . args)
  (let* ((option-spec
	  '((file (single-char #\f) (value #t))
            (field-separator (single-char #\F) (value #t))

            (help (single-char #\h))
            (version (single-char #\V))))
	 (options (getopt-long args option-spec))
         (program-file (option-ref options 'file #f))
         (delimiter (option-ref options 'field-separator " "))
         (delimiter (car (string->list delimiter)))

	 (help? (option-ref options 'help #f))
         (version? (option-ref options 'version #f))
	 (files (option-ref options '() '()))
         (usage? (and (not help?) (not program-file) (null? files))))
    (cond (version? (format #t "awk (GASH) ~a\n" %version) (exit 0))
          ((or help? usage?) (format (if usage? (current-error-port) #t)
                                     "\
Usage: awk [OPTION]...
      --help               display this help and exit
      --version            output version information and exit
")
           (exit (if usage? 2 0)))
          (else
           (receive (parse-tree files)
               (if program-file (values (with-input-from-file program-file read-awk) files)
                   (values (with-input-from-string (car files) read-awk) (cdr files)))
             (when (getenv "AWK_DEBUG")
               (pretty-print parse-tree (current-error-port)))
             (let* ((files (if (pair? files) files
                               '("-")))
                    (outport (current-output-port))
                    (begin-blocks (filter-map begin-block parse-tree))
                    (program (filter (negate (disjoin begin-block end-block)) parse-tree))
                    (end-blocks (filter-map end-block parse-tree))
                    (variables `(("FS" . ,delimiter) ("NR" . 0)))
                    (variables (fold (cut run-commands #f outport '() <> <>) variables begin-blocks))
                    (variables (fold (cut run-awk-file program outport <> <>) variables files)))
               (fold (cut run-commands #f outport '() <> <>) variables end-blocks)))))))

(define main awk)
