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
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (gash commands config)
  #:use-module (gash util)
  #:use-module (gash commands awk lexer)
  #:use-module (gash commands awk parser)
  #:use-module (gash-utils file-formats)
  #:export (awk))

(define-immutable-record-type <env>
  (make-env variables)
  env?
  (variables env-variables set-env-variables))

(define char-set:awk-non-space
  (char-set-complement (char-set-union char-set:blank (char-set #\newline))))

(define (string-split/regex str pattern)
  (let loop ((matches (list-matches pattern str)) (k 0) (acc '()))
    (match matches
      (()
       (let ((subs (substring str k)))
         (reverse (cons subs acc))))
      ((m . rest)
       (let ((subs (substring str k (match:start m))))
         (loop rest (match:end m) (cons subs acc)))))))

(define (string-split/awk str delim)
  (cond
   ((string-null? delim)
    (map string (string->list str)))
   ((string=? delim " ")
    (string-tokenize str char-set:awk-non-space))
   ((= (string-length delim) 1)
    (string-split str (string-ref delim 0)))
   (else
    (string-split/regex str delim))))

;; Builtins

(define (awk-split env string array delimiter delimiters)
  (let* ((split (string-split/awk string delimiter))
         (count (length split))
         (env (fold (cut assign-array array <> <> <>)
                    env (iota count 1) split)))
    (when delimiters
      (error (format #f "awk: split: delimiters not supported: ~s\n" delimiters)))
    (values count env)))

(define (awk-length env expr)
  (values (string-length (awk-expression->string expr)) env))

(define* (awk-substr env string start #:optional length)
  (let ((start (1- (awk-expression->number start env)))
        (end (if length (1- (+ start (awk-expression->number length env)))
                 (string-length string))))
    (values (substring string start end) env)))

(define (awk-index env s1 s2)
  (let ((s1 (awk-expression->string s1))
        (s2 (awk-expression->string s2)))
    (match (string-contains s1 s2)
      (#f (values 0 env))
      (idx (values (1+ idx) env)))))

(define (delete-var name env)
  (set-env-variables
   env (filter (negate (compose (cut eq? <> name) car))
               (env-variables env))))

(define (assign name value env)
  (set-env-variables
   env (acons name value (env-variables (delete-var name env)))))

(define (assign* pairs env)
  (let* ((keys (map car pairs))
         (variables (filter (negate (compose (cut member <> keys) car))
                            (env-variables env))))
    (set-env-variables env (append pairs variables))))

(define (assign-array name index value env)
  (let* ((array (or (assoc-ref (env-variables env) name) '()))
         (array (alist-delete index array))
         (array (acons index value array)))
    (assign name array env)))

(define (get-var name env)
  (or (assoc-ref (env-variables env) name) ""))

(define (awk-not x env)
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

(define (awk-expression->boolean expression env)
  (match expression
    ((? number?) (values (not (zero? expression)) env))
    ((? string?) (values (not (string-null? expression)) env))
    ((? boolean?) (values expression env))
    ((? vector?) (values 1 env))
    (_ (receive (expression env) (awk-expression expression env)
         (awk-expression->boolean expression env)))))

(define (awk-expression->number expression env)
  (match expression
    ("" (values 0 env))
    ((? string?) (values (or (string->number expression) 0) env))
    ((? number?) (values expression env))
    (#t (values 1 env))
    (#f (values 0 env))
    (_ (receive (v env) (awk-expression expression env)
         (awk-expression->number v env)))))

(define (awk-name o)
  (match o
    (('<awk-name> name) name)
    ((? string?) o)
    (() #f)))

(define (awk-set lvalue value env)
  ;; TODO: Handle fields.
  (match lvalue
    (('<awk-name> name)
     (assign name value env))
    (('<awk-array-ref> name index)
     (receive (result env) (awk-expression index env)
       (assign-array name result value env)))))

(define (awk-expression expression env)
  (match expression
    (('<awk-name> name) (values (get-var name env) env))
    (('<awk-array-ref> name index)
     (let ((array (get-var name env)))
       (receive (index env) (awk-expression index env)
         (values (or (assoc-ref array index) "") env))))
    (('<awk-in> index name) (values (and (assoc-ref (get-var name env) index)) env))
    (('<awk-field> ('<awk-name> "NF")) (values (last (get-var "*fields*" env)) env))
    (('<awk-field> number) (let ((field (awk-expression number env))
                                 (fields (get-var "*fields*" env))
                                 (line (get-var "*line*" env)))
                             (values (cond ((zero? field) line)
                                           ((> field (length fields)) "")
                                           (else (list-ref fields (1- field))))
                                     env)))
    ;; FIXME
    (('<awk-call> (and name "split") (string array arguments ...))
     (receive (string env) (awk-expression string env)
       (let* ((array (awk-name array))
              (delimiter (if (pair? arguments) (car arguments) (get-var "FS" env)))
              (delimiters (if (= (length arguments) 2) (awk-name (cadr arguments)) #f)))
         ((get-var name env) env string array delimiter delimiters))))
    (('<awk-call> name (argument))
     (receive (argument env) (awk-expression argument env)
       ((get-var name env) env argument)))
    (('<awk-call> name (arguments ..1))
     (let ((proc (get-var name env)))
       (let loop ((arguments arguments) (env env) (acc '()))
         (match arguments
           (() (apply proc env (reverse! acc)))
           ((arg . rest)
            (receive (arg env) (awk-expression arg env)
              (loop rest env (cons arg acc))))))))
    (('<awk-post-inc> x)
     (receive (v env) (awk-expression->number x env)
       (values v (awk-set x (1+ v) env))))
    (('<awk-post-dec> x)
     (receive (v env) (awk-expression->number x env)
       (values v (awk-set x (1- v) env))))
    (('<awk-pre-inc> x)
     (receive (v env) (awk-expression->number x env)
       (values (1+ v) (awk-set x (1+ v) env))))
    (('<awk-pre-dec> x)
     (receive (v env) (awk-expression->number x env)
       (values (1- v) (awk-set x (1- v) env))))
    ((? number?) (values expression env))
    ((? string?) (values expression env))
    (('+ x y) (receive (x env) (awk-expression->number x env)
                (receive (y env) (awk-expression->number y env)
                  (values (+ x y) env))))
    (('- x y) (receive (x env) (awk-expression->number x env)
                (receive (y env) (awk-expression->number y env)
                  (values (- x y) env))))
    (('* x y) (receive (x env) (awk-expression->number x env)
                (receive (y env) (awk-expression->number y env)
                  (values (* x y) env))))
    (('/ x y) (receive (x env) (awk-expression->number x env)
                (receive (y env) (awk-expression->number y env)
                  (values (/ x y) env))))
    (('&& x y) (receive (x env) (awk-expression x env)
                 (if (not (awk-expression->boolean x env))
                     (values #f env)
                     (receive (y env) (awk-expression y env)
                       (values y env)))))
    (('|| x y) (receive (x env) (awk-expression x env)
                 (if (awk-expression->boolean x env)
                     (values x env)
                     (receive (y env) (awk-expression y env)
                       (values y env)))))
    (('~ x ('<awk-regex> regex)) (receive (x env) (awk-expression x env)
                                   (let ((x (awk-expression->string x)))
                                     (awk-expression->boolean (string-match regex x) env))))
    (('!~ x ('<awk-regex> regex)) (receive (x env) (awk-expression x env)
                                    (let ((x (awk-expression->string x)))
                                      (awk-expression->boolean (not (string-match regex x)) env))))
    (('== x y) (receive (x env) (awk-expression x env)
                 (receive (y env) (awk-expression y env)
                   (values (equal? x y) env))))
    (('!= x y) (receive (x env) (awk-expression x env)
                 (receive (y env) (awk-expression y env)
                   (values (not (equal? x y)) env))))
    (('< x y) (receive (x env) (awk-expression->number x env)
                (receive (y env) (awk-expression->number y env)
                  (values (< x y) env))))
    (('<= x y) (receive (x env) (awk-expression->number x env)
                 (receive (y env) (awk-expression->number y env)
                   (values (<= x y) env))))
    (('> x y) (receive (x env) (awk-expression->number x env)
                (receive (y env) (awk-expression->number y env)
                  (values (> x y) env))))
    (('>= x y) (receive (x env) (awk-expression->number x env)
                 (receive (y env) (awk-expression->number y env)
                   (values (>= x y) env))))
    (('= lvalue y)
     (receive (y env) (awk-expression y env)
       (values y (awk-set lvalue y env))))
    (('+= x y)
     (receive (v env) (awk-expression->number x env)
       (receive (y env) (awk-expression->number y env)
         (values (+ v y) (awk-set x (+ v y) env)))))
    (('! x) (receive (x env) (awk-expression x env)
              (values (awk-not x env) env)))
    (('<awk-concat> x y) (receive (x env) (awk-expression x env)
                           (receive (y env) (awk-expression y env)
                             (values (string-append (awk-expression->string x) (awk-expression->string y)) env))))
    (('<awk-regex> regex) (values (and (string-match regex (get-var "*line*" env)) 1) env))))

(define *next-record-prompt* (make-prompt-tag))

(define (next-record env)
  (abort-to-prompt *next-record-prompt* env))

(define *break-loop-prompt* (make-prompt-tag))

(define (break-loop env)
  (abort-to-prompt *break-loop-prompt* env))

(define awk-conversion-adapter
  (make-conversion-adapter
   (lambda (expression env)
     (receive (result env) (awk-expression expression env)
       (values (awk-expression->string result) env)))
   awk-expression->number))

(define (awk-printf env format-string . args)
  (receive (format-string env) (awk-expression format-string env)
    (let ((format (parse-file-format (awk-expression->string format-string)
                                     #:escaped? #f)))
      (receive (result env)
          (apply fold-file-format awk-conversion-adapter env format args)
        (display result)
        env))))

(define (run-commands inport outport fields command env)
  (match command
    (('<awk-item> ('<awk-pattern> pattern) action)
     (receive (expr env) (awk-expression->boolean pattern env)
       (if expr (run-commands inport outport fields action env)
           env)))
    (('<awk-item> (or ('<awk-begin>) ('<awk-end>)) action) env)
    (('<awk-pattern> _)
     (let ((command* `(<awk-item> ,command (<awk-print>))))
       (run-commands inport outport fields command* env)))
    (('<awk-action> actions ...)
     (fold (cut run-commands inport outport fields <> <>) env actions))
    (('<awk-expr> expr)
     (receive (expr env) (awk-expression expr env) env))
    (('<awk-print>)
     (let ((count  (min (get-var "NF" env) (length fields))))
       (display (string-join (list-head fields count))))
     (newline)
     env)
    (('<awk-print> expr)
     (receive (result env) (awk-expression expr env)
       (display result)
       (newline)
       env))
    (('<awk-print> expr ..1)
     (let loop ((exprs expr) (env env) (acc '()))
       (match exprs
         (()
          ;; TODO: Use the OFS variable.
          (display (string-join (reverse! acc) " "))
          (newline)
          env)
         ((expr . rest)
          (receive (result env) (awk-expression expr env)
            (loop rest env (cons (awk-expression->string result) acc)))))))
    (('<awk-printf> format-string args ...)
     (apply awk-printf env format-string args))
    (('<awk-print-to> redir . exprs)
     (match redir
       ;; This is an Automake idiom for printing to stderr.
       (('pipe "cat >&2")
        (with-output-to-port (current-error-port)
          (lambda ()
            (let ((command* `(<awk-print> ,@exprs)))
              (run-commands inport outport fields command* env)))))
       (_ (error "awk: cannot redirect output"))))
    (('<awk-for-in> key array action)
     (let* ((save-key (get-var key env))
            (keys (map car (get-var array env)))
            (env (fold (lambda (value env)
                               (run-commands inport outport fields action
                                             (assign key value env)))
                             env
                             keys)))
       (if save-key (assign key save-key env)
           (delete-var key env))))
    (('<awk-for> init test expr action)
     (call-with-prompt *break-loop-prompt*
       (lambda ()
         (receive (init-expr env) (awk-expression init env)
           (let loop ((env env))
             (receive (test env) (awk-expression->boolean test env)
               (if (not test) env
                   (let ((env (run-commands inport outport fields
                                                  action env)))
                     (receive (expr env) (awk-expression expr env)
                       (loop env))))))))
       (lambda (cont env)
         env)))
    (('<awk-while> test action)
     (let loop ((env env))
       (receive (result env) (awk-expression->boolean test env)
         (if result
             (loop (run-commands inport outport fields action env))
             env))))
    (('<awk-do> action test)
     (let loop ((env (run-commands inport outport fields
                                         action env)))
       (receive (result env) (awk-expression->boolean test env)
         (if result
             (loop (run-commands inport outport fields action env))
             env))))
    (('<awk-if> expr then)
     (receive (expr env) (awk-expression->boolean expr env)
       (if expr
           (run-commands inport outport fields then env)
           env)))
    (('<awk-if> expr then else)
     (receive (expr env) (awk-expression->boolean expr env)
       (if expr
           (run-commands inport outport fields then env)
           (run-commands inport outport fields else env))))
    (('<awk-break>) (break-loop env))
    (('<awk-next>) (next-record env))
    ((or (? number?) (? string?)) env)
    (((? symbol?) . rest)
     (receive (expr env) (awk-expression command env)
       env))
    ((lst ...) (fold (cut run-commands inport outport fields <> <>)
                     env lst))
    (_ (format (current-error-port) "skip: ~s\n" command)
       env)))

(define* (run-awk-file program outport file-name env)
  (let ((inport (if (equal? file-name "-") (current-input-port)
                    (open-input-file file-name))))
    (let loop ((line (read-delimited (get-var "RS" env) inport))
               (env (assign* `(("FILENAME" . ,file-name)
                               ("FNR" . 0))
                             env)))
      (if (eof-object? line) env
        (let* ((fields (string-split/awk line (get-var "FS" env)))
               (env (assign* `(("NF" . ,(length fields))
                               ("NR" . ,(1+ (get-var "NR" env)))
                               ("FNR" . ,(1+ (get-var "FNR" env)))
                               ("*line*" . ,line)
                               ("*fields*" . ,fields))
                             env)))
          (let ((env (call-with-prompt *next-record-prompt*
                             (lambda ()
                               (run-commands inport outport fields
                                             program env))
                             (lambda (cont env) env))))
            (loop (read-delimited (get-var "RS" env) inport)
                  env)))))))

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

(define (make-default-env field-separator)
  (define variables
    `(("RS" . "\n")
      ("FS" . ,field-separator)
      ("NR" . 0)
      ("index" . ,awk-index)
      ("length" . ,awk-length)
      ("split" . ,awk-split)
      ("substr" . ,awk-substr)))
  (make-env variables))

(define* (%eval-awk items names #:optional
                    (out (current-output-port))
                    #:key (field-separator " "))
  (let* ((env (make-default-env field-separator))
         (env (eval-special-items items '(<awk-begin>) out env))
         (env (fold (cut run-awk-file items out <> <>) env names)))
    (eval-special-items items '(<awk-end>) out env)))

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
