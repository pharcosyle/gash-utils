;;; Gash-Utils
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Timothy Sample <samplet@ngyro.com>
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

(define-module (gash commands awk)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (gash commands config)
  #:use-module (gash util)
  #:use-module (gash commands awk lexer)
  #:use-module (gash commands awk parser)
  #:export (awk))

(define char-set:awk-non-space
  (char-set-complement (char-set-union char-set:blank (char-set #\newline))))

(define (string-split/awk str delim)
  (cond
   ((string-null? delim)
    (map string (string->list str)))
   ((string=? delim " ")
    (string-tokenize str char-set:awk-non-space))
   ((= (string-length delim) 1)
    (string-split str (string-ref delim 0)))
   (else
    (error "regex field splitting is not supported"))))

;; Builtins

(define (awk-split variables string array delimiter delimiters)
  (let* ((split (string-split/awk string delimiter))
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

(define (awk-index variables s1 s2)
  (let ((s1 (awk-expression->string s1))
        (s2 (awk-expression->string s2)))
    (match (string-contains s1 s2)
      (#f (values 0 variables))
      (idx (values (1+ idx) variables)))))

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
    (('<awk-array-ref> name index)
     (let ((array (get-var name variables)))
       (receive (index variables) (awk-expression index variables)
         (values (get-var index array) variables))))
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
    (('<awk-call> name (argument))
     (receive (argument variables) (awk-expression argument variables)
       ((get-var name variables) variables argument)))
    (('<awk-call> name (arguments ..1))
     (let ((proc (get-var name variables)))
       (let loop ((arguments arguments) (variables variables) (acc '()))
         (match arguments
           (() (apply proc variables (reverse! acc)))
           ((arg . rest)
            (receive (arg variables) (awk-expression arg variables)
              (loop rest variables (cons arg acc))))))))
    (('<awk-post-inc> x) (receive (v variables) (awk-expression->number x variables)
                           (values v (assign (awk-name x) (1+ v) variables))))
    (('<awk-post-dec> x) (receive (v variables) (awk-expression->number x variables)
                           (values x (assign (awk-name x) (1- v) variables))))
    (('<awk-pre-inc> x) (receive (v variables)
                            (awk-expression->number x variables)
                          (let ((v (1+ v)))
                            (values v (assign (awk-name x) v variables)))))
    (('<awk-pre-dec> x) (receive (v variables)
                            (awk-expression->number x variables)
                          (let ((v (1- v)))
                            (values v (assign (awk-name x) v variables)))))
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
    (('+= ('<awk-name> name) y)
     (receive (x variables)
         (awk-expression->number `(<awk-name> ,name) variables)
       (receive (y variables) (awk-expression->number y variables)
         (values (+ x y) (assign name (+ x y) variables)))))
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

(define *next-record-prompt* (make-prompt-tag))

(define (next-record variables)
  (abort-to-prompt *next-record-prompt* variables))

(define (run-commands inport outport fields command variables)
  (match command
    (('<awk-item> ('<awk-pattern> pattern) action)
     (receive (expr variables) (awk-expression->boolean pattern variables)
       (if expr (run-commands inport outport fields action variables)
           variables)))
    (('<awk-item> (or ('<awk-begin>) ('<awk-end>)) action) variables)
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
     (receive (result variables) (awk-expression expr variables)
       (display result)
       (newline)
       variables))
    (('<awk-print> expr ..1)
     (let loop ((exprs expr) (variables variables) (acc '()))
       (match exprs
         (()
          ;; TODO: Use the OFS variable.
          (display (string-join (reverse! acc) " "))
          (newline)
          variables)
         ((expr . rest)
          (receive (result variables) (awk-expression expr variables)
            (loop rest variables
                  (cons (awk-expression->string result) acc)))))))
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
    (('<awk-next>) (next-record variables))
    ((or (? number?) (? string?)) variables)
    (((? symbol?) . rest)
     (receive (expr variables) (awk-expression command variables)
       variables))
    ((lst ...) (fold (cut run-commands inport outport fields <> <>)
                     variables lst))
    (_ (format (current-error-port) "skip: ~s\n" command)
       variables)))

(define* (run-awk-file program outport file-name variables)
  (let ((inport (if (equal? file-name "-") (current-input-port)
                    (open-input-file file-name))))
    (let loop ((line (read-line inport)) (variables `(("FILENAME" . ,file-name)
                                                      ("FNR" . 0)
                                                      ,@variables)))
      (if (eof-object? line) variables
        (let* ((fields (string-split/awk line (get-var "FS" variables)))
               (variables (assign "NF" (length fields) variables))
               (variables (assign "NR" (1+ (get-var "NR" variables)) variables))
               (variables (assign "FNR" (1+ (get-var "FNR" variables)) variables))
               (variables (assign "*line*" line variables))
               (variables (assign "*fields*" fields variables)))
          (let ((variables (call-with-prompt *next-record-prompt*
                             (lambda ()
                               (run-commands inport outport fields
                                             program variables))
                             (lambda (cont variables) variables))))
            (loop (read-line inport) variables)))))))

(define (eval-special-items items pattern out env)
  (match items
    (() env)
    ((('<awk-item> (? (cut equal? <> pattern)) action) . rest)
     (let* ((env* (call-with-prompt *next-record-prompt*
                    (lambda ()
                      (run-commands #f out '() action env))
                    (lambda _
                      (error "awk: next statement in special item")))))
       (eval-special-items rest pattern out env*)))
    ((_ . rest) (eval-special-items rest pattern out env))))

(define* (%eval-awk items names #:optional
                    (out (current-output-port))
                    #:key (field-separator " "))
  (let* ((variables `(("FS" . ,field-separator)
                      ("NR" . 0)
                      ("index" . ,awk-index)
                      ("length" . ,awk-length)
                      ("split" . ,awk-split)
                      ("substr" . ,awk-substr)))
         (variables (eval-special-items items '(<awk-begin>) out variables))
         (variables (fold (cut run-awk-file items out <> <>) variables names)))
    (eval-special-items items '(<awk-end>) out variables)))

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
             (let ((files (if (pair? files) files
                              '("-")))
                   (outport (current-output-port)))
               (%eval-awk parse-tree files outport
                          #:field-separator delimiter)))))))

(define main awk)
