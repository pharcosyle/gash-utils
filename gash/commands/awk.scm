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
  #:use-module (ice-9 regex)

  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:use-module (gash config)
  #:use-module (gash util)
  #:use-module (gash commands awk lexer)
  #:use-module (gash commands awk parser)

  #:export (
            awk
            ))

;; Builtins

(define (awk-split variables string array delimiter delimiters)
  (let* ((split (string-split string (car (string->list delimiter))))
         (count (length split))
         (variables (fold (cut assign-array array <> <> <>)
                          variables (iota count 1) split)))
    (when delimiters
      (error (format #f "awk: split: delimiters not supported: ~s\n" delimiters)))
    (values count variables)))

(define (awk-length variables expr)
  (values (string-length (awk-expression->string expr)) variables))

(define* (awk-substr variables string start #:optional length)
  (let ((start (1- (awk-expression->number start variables)))
        (end (if length (1- (+ start (awk-expression->number length variables)))
                 (string-length string))))
    (values (substring string start end) variables)))

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

(define (awk-expression->boolean expression variables)
  (match expression
    ((? number?) (values (not (zero? expression)) variables))
    ((? string?) (values (not (string-null? expression)) variables))
    ((? boolean?) (values expression variables))
    ((? vector?) (values 1 variables))
    (_ (receive (expression variables) (awk-expression expression variables)
         (awk-expression->boolean expression variables)))))

(define (awk-expression->number expression variables)
  (match expression
    ("" (values 0 variables))
    ((? string?) (values (string->number expression) variables))
    ((? number?) (values expression variables))
    (#t (values 1 variables))
    (#f (values 0 variables))
    (_ (receive (v variables) (awk-expression expression variables)
         (awk-expression->number v variables)))))

(define (awk-name o)
  (match o
    (('<awk-name> name) name)
    ((? string?) o)
    (() #f)))

(define (awk-expression expression variables)
  (match expression
    (('<awk-name> name) (values (get-var name variables) variables))
    (('<awk-array-ref> name index) (values (get-var (awk-expression index variables) (get-var name variables)) variables))
    (('<awk-in> index name) (values (and (assoc-ref (get-var name variables) index)) variables))
    (('<awk-field> ('<awk-name> "NF")) (values (last (get-var "*fields*" variables)) variables))
    (('<awk-field> number) (let ((field (awk-expression number variables))
                                 (fields (get-var "*fields*" variables))
                                 (line (get-var "*line*" variables)))
                             (values (cond ((zero? field) line)
                                           ((>= field (length fields)) "")
                                           (else (list-ref fields (1- field))))
                                     variables)))
    ;; FIXME
    (('<awk-call> (and name "split") (string array arguments ...))
     (receive (string variables) (awk-expression string variables)
       (let* ((array (awk-name array))
              (delimiter (if (pair? arguments) (car arguments) (get-var "FS" variables)))
              (delimiters (if (= (length arguments) 2) (awk-name (cadr arguments)) #f)))
         ((get-var name variables) variables string array delimiter delimiters))))
    (('<awk-call> name (argument)) ((get-var name variables) variables (awk-expression argument variables)))
    (('<awk-call> name (arguments ..1)) (apply (get-var name variables)
                                               (cons variables
                                                     (map (cut awk-expression <> variables) arguments))))
    (('<awk-post-inc> x) (receive (v variables) (awk-expression->number x variables)
                           (values v (assign (awk-name x) (1+ v) variables))))
    (('<awk-post-dec> x) (receive (v variables) (awk-expression->number x variables)
                           (values x (assign (awk-name x) (1- v) variables))))
    (('<awk-pre-inc> x) (receive (v variables) (1+ (awk-expression->number x variables))
                          (values v (assign (awk-name x) v variables))))
    (('<awk-pre-dec> x) (receive (v variables) (1- (awk-expression->number x variables))
                          (values v (assign (awk-name x) v variables))))
    ((? number?) (values expression variables))
    ((? string?) (values expression variables))
    (('+ x y) (receive (x variables) (awk-expression->number x variables)
                (receive (y variables) (awk-expression->number y variables)
                  (values (+ x y) variables))))
    (('- x y) (receive (x variables) (awk-expression->number x variables)
                (receive (y variables) (awk-expression->number y variables)
                  (values (- x y) variables))))
    (('* x y) (receive (x variables) (awk-expression->number x variables)
                (receive (y variables) (awk-expression->number y variables)
                  (values (* x y) variables))))
    (('/ x y) (receive (x variables) (awk-expression->number x variables)
                (receive (y variables) (awk-expression->number y variables)
                  (values (/ x y) variables))))
    (('&& x y) (receive (x variables) (awk-expression x variables)
                 (if (not (awk-expression->boolean x variables))
                     (values #f variables)
                     (receive (y variables) (awk-expression y variables)
                       (values y variables)))))
    (('|| x y) (receive (x variables) (awk-expression x variables)
                 (if (awk-expression->boolean x variables)
                     (values x variables)
                     (receive (y variables) (awk-expression y variables)
                       (values y variables)))))
    (('~ x ('<awk-regex> regex)) (receive (x variables) (awk-expression x variables)
                                   (let ((x (awk-expression->string x)))
                                     (awk-expression->boolean (string-match regex x) variables))))
    (('!~ x ('<awk-regex> regex)) (receive (x variables) (awk-expression x variables)
                                    (let ((x (awk-expression->string x)))
                                      (awk-expression->boolean (not (string-match regex x)) variables))))
    (('== x y) (receive (x variables) (awk-expression x variables)
                 (receive (y variables) (awk-expression y variables)
                   (values (equal? x y) variables))))
    (('!= x y) (receive (x variables) (awk-expression x variables)
                 (receive (y variables) (awk-expression y variables)
                   (values (not (equal? x y)) variables))))
    (('< x y) (receive (x variables) (awk-expression->number x variables)
                (receive (y variables) (awk-expression->number y variables)
                  (values (< x y) variables))))
    (('<= x y) (receive (x variables) (awk-expression->number x variables)
                 (receive (y variables) (awk-expression->number y variables)
                   (values (<= x y) variables))))
    (('> x y) (receive (x variables) (awk-expression->number x variables)
                (receive (y variables) (awk-expression->number y variables)
                  (values (> x y) variables))))
    (('>= x y) (receive (x variables) (awk-expression->number x variables)
                 (receive (y variables) (awk-expression->number y variables)
                   (values (>= x y) variables))))
    (('= ('<awk-name> x) y)
     (receive (y variables) (awk-expression y variables)
       (values y (assign x y variables))))
    (('= ('<awk-array-ref> name index) value)
     (receive (index variables) (awk-expression index variables)
       (receive (value variables) (awk-expression value variables)
         (let ((variables (assign-array name index value variables)))
           (values (get-var name variables) variables)))))
    (('! x) (receive (x variables) (awk-expression x variables)
              (values (awk-not x variables) variables)))
    (('<awk-concat> x y) (receive (x variables) (awk-expression x variables)
                           (receive (y variables) (awk-expression y variables)
                             (values (string-append (awk-expression->string x) (awk-expression->string y)) variables))))
    (('<awk-regex> regex) (values (and (string-match regex (get-var "*line*" variables)) 1) variables))))

(define (run-commands inport outport fields command variables)
  (match command
    (('<awk-item> ('<awk-pattern> pattern) action)
     (receive (expr variables) (awk-expression->boolean pattern variables)
       (if expr (run-commands inport outport fields action variables)
           variables)))
    (('<awk-action> actions ...)
     (fold (cut run-commands inport outport fields <> <>) variables actions))
    (('<awk-expr> expr)
     (receive (expr variables) (awk-expression expr variables) variables))
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
     (let* ((save-key (assoc-ref variables key))
            (keys (map car (get-var array variables)))
            (variables (fold (lambda (value variables)
                               (run-commands inport outport fields action `((,key . ,value) ,@variables)))
                             variables
                             keys)))
       (if save-key (assign key save-key variables)
           (delete-var key variables))))
    (('<awk-for> init test expr action)
     (receive (init-expr variables) (awk-expression init variables)
       (let loop ((variables variables))
         (receive (test variables) (awk-expression->boolean test variables)
           (if (not test) variables
               (let ((variables (run-commands inport outport fields action variables)))
                 (receive (expr variables) (awk-expression expr variables)
                   (loop variables))))))))
    (('<awk-if> expr then)
     (receive (expr variables) (awk-expression->boolean expr variables)
       (if expr
           (run-commands inport outport fields then variables)
           variables)))
    (('<awk-if> expr then else)
     (receive (expr variables) (awk-expression->boolean expr variables)
       (if expr
           (run-commands inport outport fields then variables)
           (run-commands inport outport fields else variables))))
    ;; ? awk-expr?
    (((or '=) x ...) (receive (expr variables) (awk-expression command variables)
                       variables))
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
        (let* ((fields (string-split line (car (string->list (get-var "FS" variables)))))
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
                    (variables `(("FS" . ,delimiter)
                                 ("NR" . 0)
                                 ("length" . ,awk-length)
                                 ("split" . ,awk-split)
                                 ("substr" . ,awk-substr)))
                    (variables (fold (cut run-commands #f outport '() <> <>) variables begin-blocks))
                    (variables (fold (cut run-awk-file program outport <> <>) variables files)))
               (fold (cut run-commands #f outport '() <> <>) variables end-blocks)))))))

(define main awk)
