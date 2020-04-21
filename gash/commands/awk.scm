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

(define-immutable-record-type <awk-array>
  (make-awk-array values)
  awk-array?
  (values awk-array-values set-awk-array-values))

(define awk-array-null (make-awk-array '()))

(define* (awk-array-ref index awk-array #:optional (dflt *awk-undefined*))
  (match (assoc index (awk-array-values awk-array))
    ((_ . value) value)
    (_ dflt)))

(define (awk-array-member? index awk-array)
  (match (assoc index (awk-array-values awk-array))
    ((_ . _) #t)
    (_ #f)))

(define (awk-array-set index value awk-array)
  (let* ((values (awk-array-values awk-array))
         (values* (alist-cons index value (alist-delete index values))))
    (set-awk-array-values awk-array values*)))

(define (awk-array-delete index awk-array)
  (let ((values (awk-array-values awk-array)))
    (set-awk-array-values awk-array (alist-delete index values))))

(define (awk-array-keys awk-array)
  (map car (awk-array-values awk-array)))

(define-immutable-record-type <env>
  (make-env in out record fields variables)
  env?
  (in env-in set-env-in)
  (out env-out)
  (record env-record set-env-record)
  (fields env-fields set-env-fields)
  (variables env-variables set-env-variables))

(define* (env-ref name env #:optional (dflt *awk-undefined*))
  (match (assoc name (env-variables env))
    ((_ . value) value)
    (_ dflt)))

(define (env-set name value env)
  (let* ((variables (alist-delete name (env-variables env))))
    (set-env-variables env (alist-cons name value variables))))

(define (env-set* pairs env)
  (let* ((keys (map car pairs))
         (variables (filter (negate (compose (cut member <> keys) car))
                            (env-variables env))))
    (set-env-variables env (append pairs variables))))

(define (env-ref/array name env)
  (match (env-ref name env)
    ((? awk-array? array)
     (values array env))
    ((? awk-undefined? undef)
     (values awk-array-null (env-set name awk-array-null env)))
    (_ (error "a scalar was used an an array: " name))))

(define (env-set-array name index value env)
  (receive (array env) (env-ref/array name env)
    (env-set name (awk-array-set index value array) env)))

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
         (env (fold (cut env-set-array array <> <> <>)
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

(define *awk-undefined* (list 'awk-undefined))

(define (awk-undefined? x)
  (eq? x *awk-undefined*))

(define (awk-expression->string expression)
  (match expression
    ((? string?) expression)
    ((? number?) (number->string expression))
    (#t "1")
    (#f "0")
    ((? awk-array?) (error "an array was used as a scalar"))
    ((? awk-undefined?) "")
    ((lst ...) (string-join (map awk-expression->string lst) ""))))

(define (awk-expression->boolean expression env)
  (match expression
    ((? number?) (values (not (zero? expression)) env))
    ((? string?) (values (not (string-null? expression)) env))
    ((? boolean?) (values expression env))
    ((? vector?) (values 1 env))
    ((? awk-array?) (error "an array was used as a scalar"))
    ((? awk-undefined?) (values #f env))
    (_ (receive (expression env) (awk-expression expression env)
         (awk-expression->boolean expression env)))))

(define (awk-expression->number expression env)
  (match expression
    ("" (values 0 env))
    ((? string?) (values (or (string->number expression) 0) env))
    ((? number?) (values expression env))
    (#t (values 1 env))
    (#f (values 0 env))
    ((? awk-array?) (error "an array was used as a scalar"))
    ((? awk-undefined?) (values 0 env))
    (_ (receive (v env) (awk-expression expression env)
         (awk-expression->number v env)))))

(define (awk-set lvalue value env)
  ;; TODO: Handle fields.
  (match lvalue
    ((? symbol? name)
     (env-set name value env))
    (('array-ref index name)
     (receive (result env) (awk-expression index env)
       (env-set-array name result value env)))))

(define (awk-expression expression env)
  (match expression
    ((? symbol? name) (values (env-ref name env) env))
    (('array-ref index name)
     (receive (array env) (env-ref/array name env)
       (receive (index env) (awk-expression index env)
         (values (awk-array-ref index array) env))))
    (('array-member? index name)
     (receive (array env) (env-ref/array name env)
       (receive (index env) (awk-expression index env)
         (values (awk-array-member? index array) env))))
    (('$ 'NF) (values (last (env-fields env)) env))
    (('$ number) (let ((field (awk-expression number env))
                       (fields (env-fields env))
                       (line (env-record env)))
                   (values (cond ((zero? field) line)
                                 ((> field (length fields)) "")
                                 (else (list-ref fields (1- field))))
                           env)))
    ;; FIXME
    (('apply (and name 'split) string array arguments ...)
     (receive (string env) (awk-expression string env)
       (let* ((array array)
              (delimiter (if (pair? arguments)
                             (car arguments)
                             (env-ref 'FS env)))
              (delimiters (if (= (length arguments) 2)
                              (cadr arguments)
                              #f)))
         ((env-ref name env) env string array delimiter delimiters))))
    (('apply name argument)
     (receive (argument env) (awk-expression argument env)
       ((env-ref name env) env argument)))
    (('apply name arguments ..1)
     (let ((proc (env-ref name env)))
       (let loop ((arguments arguments) (env env) (acc '()))
         (match arguments
           (() (apply proc env (reverse! acc)))
           ((arg . rest)
            (receive (arg env) (awk-expression arg env)
              (loop rest env (cons arg acc))))))))
    (('post-incr! x)
     (receive (v env) (awk-expression->number x env)
       (values v (awk-set x (1+ v) env))))
    (('post-decr! x)
     (receive (v env) (awk-expression->number x env)
       (values v (awk-set x (1- v) env))))
    (('pre-incr! x)
     (receive (v env) (awk-expression->number x env)
       (values (1+ v) (awk-set x (1+ v) env))))
    (('pre-decr! x)
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
    (('and x y) (receive (x env) (awk-expression x env)
                  (if (not (awk-expression->boolean x env))
                      (values #f env)
                      (receive (y env) (awk-expression y env)
                        (values y env)))))
    (('or x y) (receive (x env) (awk-expression x env)
                 (if (awk-expression->boolean x env)
                     (values x env)
                     (receive (y env) (awk-expression y env)
                       (values y env)))))
    (('string-match x ('re regex))
     (receive (x env) (awk-expression x env)
       (let ((x (awk-expression->string x)))
         (awk-expression->boolean (string-match regex x) env))))
    (('not-string-match x ('re regex))
     (receive (x env) (awk-expression x env)
       (let ((x (awk-expression->string x)))
         (awk-expression->boolean (not (string-match regex x)) env))))
    (('equal? x y) (receive (x env) (awk-expression x env)
                     (receive (y env) (awk-expression y env)
                       (values (equal? x y) env))))
    (('not-equal? x y) (receive (x env) (awk-expression x env)
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
    (('set! lvalue y)
     (receive (y env) (awk-expression y env)
       (values y (awk-set lvalue y env))))
    (('set-op! '+ x y)
     (receive (v env) (awk-expression->number x env)
       (receive (y env) (awk-expression->number y env)
         (values (+ v y) (awk-set x (+ v y) env)))))
    (('not x)
     (receive (x env) (awk-expression->boolean x env)
       (values (not x) env)))
    (('string-append x y)
     (receive (x env) (awk-expression x env)
       (receive (y env) (awk-expression y env)
         (values (string-append (awk-expression->string x)
                                (awk-expression->string y))
                 env))))
    (('re regex) (values (and (string-match regex (env-record env)) 1) env))))

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

(define (run-commands command env)
  (match command
    ;; Output
    (('print)
     (let* ((fields (env-fields env))
            (count  (min (env-ref 'NF env) (length fields))))
       (display (string-join (list-head fields count))))
     (newline)
     env)
    (('print expr)
     (receive (result env) (awk-expression expr env)
       (display (awk-expression->string result))
       (newline)
       env))
    (('print expr ..1)
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
    (('printf format-string args ...)
     (apply awk-printf env format-string args))
    ;; Redirects
    (('with-redirect redir i/o-expr)
     (match redir
       ;; This is an Automake idiom for printing to stderr.
       (('pipe-to "cat >&2")
        (with-output-to-port (current-error-port)
          (lambda ()
            (run-commands i/o-expr env))))
       (_ (error "awk: cannot redirect output"))))
    ;; Loops
    (('for-each (key array) exprs ...)
     (receive (array env) (env-ref/array array env)
       (fold (lambda (value env)
               (fold run-commands (env-set key value env) exprs))
             env
             (awk-array-keys array))))
    (('for (init test expr) exprs ...)
     (call-with-prompt *break-loop-prompt*
       (lambda ()
         (receive (init-expr env) (awk-expression init env)
           (let loop ((env env))
             (receive (test env) (awk-expression->boolean test env)
               (if (not test) env
                   (let ((env (fold run-commands env exprs)))
                     (receive (expr env) (awk-expression expr env)
                       (loop env))))))))
       (lambda (cont env)
         env)))
    (('while test exprs ...)
     (let loop ((env env))
       (receive (result env) (awk-expression->boolean test env)
         (if result
             (loop (fold run-commands env exprs))
             env))))
    (('do test exprs ...)
     (let loop ((env (fold run-commands env exprs)))
       (receive (result env) (awk-expression->boolean test env)
         (if result
             (loop (fold run-commands env exprs))
             env))))
    ;; Conditionals
    (('if expr then)
     (receive (expr env) (awk-expression->boolean expr env)
       (if expr
           (run-commands then env)
           env)))
    (('if expr then else)
     (receive (expr env) (awk-expression->boolean expr env)
       (if expr
           (run-commands then env)
           (run-commands else env))))
    ;; Simple statements
    (('break) (break-loop env))
    (('next) (next-record env))
    ;; Sequencing
    (('progn exprs ...) (fold run-commands env exprs))
    ;; Others
    ((or (? number?) (? string?)) env)
    (((? symbol?) . rest)
     (receive (expr env) (awk-expression command env)
       env))))

(define (load-file filename env)
  (let ((port (if (equal? filename "-")
                  (current-input-port)
                  (open-input-file filename)))
        (pairs `((FILENAME . ,filename)
                 (FNR . 0))))
    (env-set* pairs (set-env-in env port))))

(define (read-record env)
  (match (read-delimited (env-ref 'RS env) (env-in env))
    ((? eof-object? eof) (set-fields env
                           ((env-record) eof)
                           ((env-fields) '())))
    (record
     (let* ((fields (string-split/awk record (env-ref 'FS env)))
            (pairs `((NF . ,(length fields))
                     (NR . ,(1+ (env-ref 'NR env)))
                     (FNR . ,(1+ (env-ref 'FNR env))))))
       (env-set* pairs (set-fields env
                         ((env-record) record)
                         ((env-fields) fields)))))))

(define (eval-item item env)
  (match item
    ((#t exprs ...)
     (fold run-commands env exprs))
    (((or 'begin 'end) exprs ...)
     env)
    ((pattern exprs ...)
     (receive (result env) (awk-expression->boolean pattern env)
       (if result
           (fold run-commands env exprs)
           env)))))

(define* (run-awk-file program file-name env)
  (let ((env (load-file file-name env)))
    (let loop ((env (read-record env)))
      (if (eof-object? (env-record env))
          env
          (let ((env (call-with-prompt *next-record-prompt*
                       (lambda ()
                         (fold eval-item env program))
                       (lambda (cont env)
                         env))))
            (loop (read-record env)))))))

(define (eval-special-items items pattern env)
  (match items
    (() env)
    ((((? (cut eq? <> pattern)) exprs ...) . rest)
     (let* ((env* (call-with-prompt *next-record-prompt*
                    (lambda ()
                      (fold run-commands env exprs))
                    (lambda _
                      (error "awk: next statement in special item")))))
       (eval-special-items rest pattern env*)))
    ((_ . rest) (eval-special-items rest pattern env))))

(define (make-default-env out field-separator)
  (define variables
    `((RS . "\n")
      (FS . ,field-separator)
      (NR . 0)
      (index . ,awk-index)
      (length . ,awk-length)
      (split . ,awk-split)
      (substr . ,awk-substr)))
  (make-env #f out #f '() variables))

(define* (%eval-awk items names #:optional
                    (out (current-output-port))
                    #:key (field-separator " "))
  (let* ((env (make-default-env out field-separator))
         (env (eval-special-items items 'begin env))
         (env (fold (cut run-awk-file items <> <>) env names)))
    (eval-special-items items 'end env)))

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

;;; Local Variables:
;;; eval: (put 'set-fields 'scheme-indent-function 1)
;;; End:
