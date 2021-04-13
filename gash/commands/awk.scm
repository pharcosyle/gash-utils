;;; Gash-Utils
;;; Copyright © 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020-2022 Timothy Sample <samplet@ngyro.com>
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

(define-module (gash commands awk)
  #:use-module (gash commands awk parser)
  #:use-module (gash commands config)
  #:use-module (gash-utils file-formats)
  #:use-module (gash-utils options)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26))

;;; Commentary:

;;; Code:


;; SRFI 71

;; Before we get to Awk, we set up about half of SRFI 71, which
;; redefines 'let' to work with multiple values.  Why SRFI 71?  Because
;; we use multiple values everywhere to juggle results and environments,
;; and SRFI 71 makes it go pretty smoothly.  Why not use the
;; implementation that comes with Guile?  Because it is missing from
;; Guile 2.0.

;; General formula for redefining a 'let'.
(define-syntax %meta-let
  (lambda (x)
    (syntax-case x ()
      ((_ lt lv ((binding-parts ...) ...) body ...)
       (with-syntax ((bindings (map (lambda (xs)
                                      (list (drop-right xs 1) (last xs)))
                                    #'((binding-parts ...) ...))))
         #'(lv bindings body ...)))
      ((_ lt lv forms ...)
       #'(lt forms ...)))))

;; Redfine 'let' using 'let-values'.
(define-syntax-rule (let forms ...)
  (%meta-let (@ (guile) let) let-values forms ...))

;; Redefine 'let*' using 'let*-values'.
(define-syntax-rule (let* forms ...)
  (%meta-let (@ (guile) let*) let*-values forms ...))

;; Now back to our regular programming.  ;)


;; Data types

(define-immutable-record-type <redirect>
  (make-redirect name type port)
  redirect?
  (name redirect-name)   ; string
  (type redirect-type)   ; 'file or 'pipe
  (port redirect-port))  ; port

;; This record type stores the Awk environment.  The record itself is
;; immutable, but it has references to mutable values.  For instance,
;; the variables and arrays are built on top of hash tables and are
;; mutated freely.
(define-immutable-record-type <env>
  (make-env in out input-redirected? redirects record fields locals globals)
  env?
  (in env-in set-env-in)               ; input port
  (out env-out set-env-out)            ; output port
  (input-redirected? env-input-redirected? set-env-input-redirected?)
  (redirects env-redirects set-env-redirects)  ; <redirect> list
  (record env-record %set-env-record)  ; record string
  (fields env-fields %set-env-fields)  ; fields list
  (locals env-locals set-env-locals)   ; hash table of local variables
  (globals env-globals))               ; hash table of global variables

(define (map/env proc lst env)
  "Map the procedure @var{proc} over the list @var{lst}, threading the
environment @var{env} through the calls to @var{proc}."
  (let loop ((xs lst) (env env) (acc '()))
    (match xs
      (() (values (reverse acc) env))
      ((x . xs)
       (let ((x env (proc x env)))
         (loop xs env (cons x acc)))))))

(define-syntax-rule (second-value expr)
  "Return the second value returned by @var{expr}."
  (call-with-values (lambda () expr)
    (lambda (first second . rest)
      second)))

;; Awk has a notion of a numeric string, which is a string that can be
;; promoted to a number in contexts where a normal string would not.  To
;; take the example from the specification, "000" as a numeric string is
;; equal to 0, but "000" as a regular string is not.
(define-immutable-record-type <numeric-string>
  (make-numeric-string string number)
  numeric-string?
  (string numeric-string-string)
  (number numeric-string-number))

(define (import-string str)
  "Convert @var{str} to a @code{numeric-string} where appropriate."
  (let ((result size (locale-string->inexact str)))
    (if (and result (string-every char-set:blank (substring str size)))
        (make-numeric-string str result)
        str)))

;; This is a bit of hack, but I think it is clever enough to keep
;; around.  Awk has a notion of an uninitialized scalar, which is an
;; uninitialized value constrained to be a scalar.  It is supposed to
;; have both string value "" and numeric value 0.  As far as I can tell,
;; this is the same as having a numeric string with those values.  If
;; true, the following value can be used in place of a distinct
;; "uninitialized scalar" value.
(define uninitialized-scalar
  (make-numeric-string "" 0))

;; When passing an uninitialized value as an argument to a function, we
;; need to be able to pass information back up to the original slot that
;; it came from.  In the case that an uninitialized argument becomes an
;; array, it needs to be as if an empty array was passed in to the
;; function by reference.  Hence, we need to send a reference to an
;; uninitialized array back to the original slot.  In the case that it
;; becomes a scalar, we need to pass that type information back to the
;; original slot.  We do this by setting it to 'uninitialized-scalar'.
;; The following record represents an uninitialized argument.  It
;; provides a 'setter' field which can be used to set the value of a
;; variable in the caller's environment.
(define-immutable-record-type <uninitialized-argument>
  (%make-uninitialized-argument setter)
  uninitialized-argument?
  (setter uninitialized-argument-setter))

(define (make-uninitialized-argument name env)
  "Create an uninitialized argument linked to the variable named
@var{name} in environment @var{env}.  The @code{setter} field of the
result can be used to set @var{name} in @var{env}."
  (%make-uninitialized-argument
   (lambda (value)
     (if (hashq-get-handle (env-locals env) name)
         (hashq-set! (env-locals env) name value)
         (hashq-set! (env-globals env) name value)))))

;; If a function delares an argument and the caller omits it, it is a
;; reserved local variable.  It is both uninitalized and not linked to
;; caller's envronment.  We will reuse the 'uninitialized-argument'
;; system, but update the local environment of the function instead of
;; the caller's environment.
(define (make-uninitialized-local name locals)
  "Create an uninitalized argument linked to the local variable named
@var{name} in the local variables hash table @var{locals}."
  (%make-uninitialized-argument
   (lambda (value)
     (hashq-set! locals name value))))

(define (set-uninitialized-argument! arg value)
  "Invoke the @code{setter} filed of the uninitialized argument
@var{arg} with @var{value} as an argument."
  ((uninitialized-argument-setter arg) value))


;; Variable handling

;; These procedures abstract away the global/local distinction, and
;; handle type checking.

(define (env-ref/procedure name env)
  "Lookup the procedure @var{name} in the envrionment @var{env}.  If
@var{name} does not exist or is not a function, raise an error."
  (match (hashq-get-handle (env-globals env) name)
    ((_ . (? procedure? proc)) proc)
    (_ (error "not a function:" name))))

(define (env-set/procedure! name proc env)
  "Set the variable @var{name} to the procedure @var{proc} in the
environment @var{env}."
  (match (hashq-create-handle! (env-globals env) name proc)
    ((_ . value)
     (unless (eq? value proc)
       (error "function name already taken:" name)))))

(define (env-ref/scalar! name env)
  "Lookup the scalar @var{name} in the environment @var{env}.  If
@var{name} does not exist, create it with value
@code{uninitialized-scalar}.  If it does exist and is not a scalar,
raise an error."
  (match (or (hashq-get-handle (env-locals env) name)
             (hashq-create-handle! (env-globals env) name
                                   uninitialized-scalar))
    ((_ . (? procedure?)) (error "function used as a scalar:" name))
    ((_ . (? hash-table?)) (error "array used as a scalar:" name))
    ((_ . (? uninitialized-argument? uarg))
     (set-uninitialized-argument! uarg uninitialized-scalar)
     (hashq-set! (env-locals env) name uninitialized-scalar)
     uninitialized-scalar)
    ((_ . value) value)))

(define (env-set/scalar! name value env)
  "Set the variable @var{name} to the scalar @var{value} in the
environment @var{env}.  If @var{name} already exists and is not a
scalar, raise an error.."
  (match (hashq-get-handle (env-locals env) name)
    ((_ . (? hash-table?)) (error "array used as a scalar:" name))
    ((_ . (? uninitialized-argument? uarg))
     (set-uninitialized-argument! uarg uninitialized-scalar)
     (hashq-set! (env-locals env) name value))
    ((_ . _) (hashq-set! (env-locals env) name value))
    (#f (match (hashq-get-handle (env-globals env) name)
          ((_ . (? procedure?)) (error "function used as a scalar:" name))
          ((_ . (? hash-table?)) (error "array used as a scalar:" name))
          (_ (hashq-set! (env-globals env) name value))))))

(define (env-ref/array! name env)
  "Lookup the array @var{name} in the environment @var{env}.  If
@var{name} does not exist, create it as an empty array.  If it does
exist and is not an array, raise an error."
  (match (or (hashq-get-handle (env-locals env) name)
             (hashq-create-handle! (env-globals env) name
                                   (make-hash-table)))
    ((_ . (? procedure?)) (error "function used as an array:" name))
    ((_ . (? uninitialized-argument? uarg))
     (let ((array (make-hash-table)))
       (set-uninitialized-argument! uarg array)
       (hashq-set! (env-locals env) name array)
       array))
    ((_ . (? hash-table? array)) array)
    ((_ . value) (error "scalar used as an array:" name))))

;; No 'env-set/array!' because we mutate arrays directly with the
;; following procedures.

(define (env-array-ref name key env)
  "Lookup @var{key} in the array named @var{name} in environment
@var{env}."
  (let ((array (env-ref/array! name env)))
    (match (hash-create-handle! array key uninitialized-scalar)
      ((_ . value) value))))

(define (env-array-set! name key value env)
  "Associate @var{value} with @var{key} in the array named @var{name} in
the environment @var{env}."
  (let ((array (env-ref/array! name env)))
    (hash-set! array key value)
    value))

(define (env-array-member? name key env)
  "Check if there is any value associated with @var{key} in the array
named @var{name} in the environment @var{env}."
  (let ((array (env-ref/array! name env)))
    (and (hash-get-handle array key) #t)))

(define (env-array-delete! name key env)
  "Delete any value associated with @var{key} in the array named
@var{name} in the environment @var{env}."
  (let ((array (env-ref/array! name env)))
    (hash-remove! array key)
    key))


;; Current record, fields, and the NF variable

;; These three things are all linked, and updating one of them requires
;; updating the other two.  We gather all of that logic here so that the
;; values can be set easily.

(define (set-env-record env record)
  "Set the @code{env-record} field of @var{env} to the string
@var{record}.  This procedure also updates the @code{env-fields} field
and the @code{'NF} variable accordingly."
  (match record
    (#f (env-set/scalar! 'NF 0 env)
        (set-fields env
          ((env-record) #f)
          ((env-fields) '())))
    (_ (let* ((fs env (eval-awke/string 'FS env))
              (fields (string-split/awk record fs)))
         (env-set/scalar! 'NF (length fields) env)
         (set-fields env
           ((env-record) record)
           ((env-fields) fields))))))

(define (set-env-fields env fields)
  "Set the @code{env-fields} field of @var{env} to the list
@var{fields}.  This procedure also updates the @code{env-record} field
and the @code{'NF} variable accordingly."
  (let* ((string-fields env (map/env eval-awke/string fields env))
         (ofs env (eval-awke/string 'OFS env)))
    (env-set/scalar! 'NF (length fields) env)
    (set-fields env
      ((env-record) (string-join string-fields ofs))
      ((env-fields) fields))))

(define (set-env-nf env raw-nf)
  "Set the @code{'NF} variable in the environment @var{env} to the
scalar @var{raw-nf}.  This procedure also updates the @code{env-record}
field and the @code{env-fields} field of @var{env} accordingly."
  (let* ((numeric-nf env (eval-awke/number raw-nf env))
         (nf (max 0 (truncate numeric-nf)))
         (old-nf (length (env-fields env))))
    (env-set/scalar! 'NF raw-nf env)
    (set-env-fields env (if (<= nf old-nf)
                            (take (env-fields env) nf)
                            (append (env-fields env)
                                    (take (circular-list uninitialized-scalar)
                                          (- nf old-nf)))))))

(define* (get-record env #:key (update-nr? #t))
  "Read a record from the input port of @var{env}.  If @var{update-nr?}
is set (the default), the @var{NR} and @var{FNR} Awk variables will be
updated in @var{env}."
  (match (read-delimited (env-ref/scalar! 'RS env) (env-in env))
    ((? eof-object?) #f)
    (record (when update-nr?
              (env-set/scalar! 'NR (1+ (env-ref/scalar! 'NR env)) env)
              (env-set/scalar! 'FNR (1+ (env-ref/scalar! 'FNR env)) env))
            record)))

(define* (read-record env #:key (update-nr? #t))
  "Read a record from the input port of @var{env} and set it as the
current record in @var{env}, returning the updated environment.  If
@var{update-nr?}  is set (the default), the @var{NR} and @var{FNR} Awk
variables will be updated in @var{env}."
  (set-env-record env (get-record env #:update-nr? update-nr?)))


;; Redirects

(define (lookup-redirect env name)
  (find (lambda (redir)
          (string=? (redirect-name redir) name))
        (env-redirects env)))

(define (remove-redirect env name)
  (set-env-redirects env (remove (lambda (redir)
                                   (string=? (redirect-name redir) name))
                                 (env-redirects env))))

(define (set-redirect env name type purpose create)
  (match (lookup-redirect env name)
    (#f (let ((port (false-if-exception (create))))
          (if port
              (let ((redir (make-redirect name type port))
                    (redirs (env-redirects env)))
                (values port (set-env-redirects env (cons redir redirs))))
              (values #f env))))
    (($ <redirect> _ existing-type port)
     (unless (eq? existing-type type)
       (error (format #f  "cannot use existing ~a redirect as ~a: ~s"
                      existing-type type name)))
     (unless (or (eq? purpose 'input) (output-port? port))
       (error (format #f "cannot write to existing redirect: ~s" name)))
     (unless (or (eq? purpose 'output) (input-port? port))
       (error (format #f "cannot read from existing redirect: ~s" name)))
     (values port env))))

(define* (redirect-output-file env target #:key (truncate? #t))
  (set-redirect env target 'file 'output
                (lambda () (open-file target (if truncate? "w" "a")))))

(define (redirect-input-file env source)
  (set-redirect env source 'file 'input
                (lambda () (open-file source "r"))))

(define (redirect-output-pipe env target)
  (set-redirect env target 'pipe 'output
                (lambda () (open-output-pipe target))))

(define (redirect-input-pipe env source)
  (set-redirect env source 'pipe 'input
                (lambda () (open-input-pipe source))))

(define (close-redirect env name)
  (match (lookup-redirect env name)
    (#f (values -1 env))
    (($ <redirect> _ type port)
     (values (case type
               ((file) (if (false-if-exception (close-port port)) 0 -1))
               ((pipe) (status:exit-val (close-pipe port))))
             (remove-redirect env name)))))


;; String splitting helpers

(define char-set:awk-non-space
  (char-set-complement (char-set-union char-set:blank (char-set #\newline))))

(define (string-split/regex str pattern)
  "Split the string @var{str} by the regular expression @var{pattern}.
The @var{pattern} parameter can be a string or a compiled regular
expression."
  (let loop ((matches (list-matches pattern str)) (k 0) (acc '()))
    (match matches
      (()
       (let ((subs (substring str k)))
         (reverse (cons subs acc))))
      ((m . rest)
       (let ((subs (substring str k (match:start m))))
         (loop rest (match:end m) (cons subs acc)))))))

(define (string-split/awk str delim)
  "Split the string @var{str} by the string @var{delim} using Awk string
splitting semantics."
  (cond
   ((string-null? delim)
    (map string (string->list str)))
   ((string=? delim " ")
    (string-tokenize str char-set:awk-non-space))
   ((= (string-length delim) 1)
    (string-split str (string-ref delim 0)))
   (else
    (string-split/regex str delim))))


;; Special variables

(define *special-variables*
  `((ARGC . 0)
    (ARGV . ())
    (CONVFMT . "%.6g")
    ;; TODO: This should be mapped to the actual environment.
    (ENVIRON . ,(make-hash-table))
    (FILENAME . ,uninitialized-scalar)
    (FNR . 0)
    (FS . " ")
    (NF . ,uninitialized-scalar)
    (NR . 0)
    (OFMT . "%.6g")
    (OFS . " ")
    (ORS . "\n")
    (RLENGTH . ,uninitialized-scalar)
    (RS . "\n")
    (RSTART . ,uninitialized-scalar)
    (SUBSEP . ,(string #\fs))))

(define *special-variable-names* (map car *special-variables*))


;; Main evaluation procedure

;; All of the evaluation procedures take an environment and return two
;; values: their result and the updated environment.

(define (eval-awke expr env)
  "Evaluate the expression @var{expr} in the environment @var{env},
returning the result of evaluation along with the updated environment."
  (match expr
    ;; Control flow
    (('progn exprs ...)
     (eval-awke* env exprs))
    (('if test-expr then-expr)
     (let ((result env (eval-awke/boolean test-expr env)))
       (if result
           (eval-awke then-expr env)
           (values uninitialized-scalar env))))
    (('if test-expr then-expr else-expr)
     (let ((result env (eval-awke/boolean test-expr env)))
       (if result
           (eval-awke then-expr env)
           (eval-awke else-expr env))))
    (('while test-expr exprs ...)
     (let loop ((value uninitialized-scalar) (env env))
       (let ((result env (eval-awke/boolean test-expr env)))
         (if result
             (let ((continue? value env (eval-loop-body env exprs)))
               (if continue?
                   (loop value env)
                   (values value env)))
             (values value env)))))
    (('do test-expr exprs ...)
     (let loop ((value uninitialized-scalar) (env env))
       (let ((continue? value env (eval-loop-body env exprs)))
         (if continue?
             (let ((result env (eval-awke/boolean test-expr env)))
               (if result
                   (loop value env)
                   (values value env)))
             (values value env)))))
    (('for (init-expr test-mexpr update-expr) exprs ...)
     (let loop ((value uninitialized-scalar)
                (env (second-value (eval-awke init-expr env))))
       (let ((result env (if (eq? test-mexpr #t)
                             (values #t env)
                             (eval-awke/boolean test-mexpr env))))
         (if result
             (let ((continue? value env (eval-loop-body env exprs)))
               (if continue?
                   (loop value (second-value (eval-awke update-expr env)))
                   (values value env)))
             (values value env)))))
    (('for-each (variable-name array-name) exprs ...)
     (let ((array (env-ref/array! array-name env)))
       (let loop ((pairs (hash-map->list cons array))
                  (value uninitialized-scalar)
                  (env env))
         (match pairs
           (() (values value env))
           (((key . _) . rest)
            (let* ((_ env (perform-set! variable-name key env))
                   (continue? value env (eval-loop-body env exprs)))
              (if continue?
                  (loop rest value env)
                  (values value env))))))))
    (('break)
     (abort-to-prompt *break-prompt* env))
    (('continue)
     (abort-to-prompt *continue-prompt* env))
    (('next)
     (abort-to-prompt *next-prompt* env))
    (('exit)
     (abort-to-prompt *exit-prompt* 0 env))
    (('exit expr)
     (let ((value env (eval-awke/number expr env)))
       (abort-to-prompt *exit-prompt* value env)))
    (('return)
     (abort-to-prompt *return-prompt* uninitialized-scalar env))
    (('return expr)
     (let ((value env (eval-awke expr env)))
       (abort-to-prompt *return-prompt* value env)))
    ;; Output
    (('print)
     (eval-awke '(print ($ 0)) env))
    (('print exprs ..1)
     (let* ((strings env (map/env eval-awke/string exprs env))
            (ofs env (eval-awke/string 'OFS env)))
       (display (string-join strings ofs) (env-out env))
       (newline (env-out env))
       (values uninitialized-scalar env)))
    (('printf format-expr exprs ...)
     (apply perform-printf env format-expr exprs))
    ;; Input
    (('getline)
     (let* ((update-nr? (not (env-input-redirected? env)))
            (env (read-record env #:update-nr? update-nr?)))
       (values (if (env-record env) 1 0) env)))
    (('getline lvalue)
     (let* ((resolved-lvalue env (resolve-lvalue lvalue env))
            (update-nr? (not (env-input-redirected? env)))
            (record (get-record env #:update-nr? update-nr?)))
       (if record
           (values 1 (second-value (perform-set! resolved-lvalue
                                                 (import-string record)
                                                 env)))
           (values 0 (second-value (perform-set! resolved-lvalue
                                                 uninitialized-scalar
                                                 env))))))
    ;; Redirects
    (('with-redirect redir expr)
     (match redir
       (('truncate target-expr)
        (let ((redirector (cut redirect-output-file <> <> #:truncate? #t)))
          (redirect-output env target-expr redirector expr)))
       (('append target-expr)
        (let ((redirector (cut redirect-output-file <> <> #:truncate? #f)))
          (redirect-output env target-expr redirector expr)))
       (('pipe-to target-expr)
        (redirect-output env target-expr redirect-output-pipe expr))
       (('read source-expr)
        (redirect-input env source-expr redirect-input-file expr))
       (('pipe-from source-expr)
        (redirect-input env source-expr redirect-input-pipe expr))
       (_ (error "unsupported redirect:" redir))))
    ;; Arithmetic
    (('+ expr)
     (let ((value env (eval-awke/number expr env)))
       (values (abs value) env)))
    (('- expr)
     (let ((value env (eval-awke/number expr env)))
       (values (- value) env)))
    (('+ expr1 expr2)
     (perform-arithmetic + expr1 expr2 env))
    (('- expr1 expr2)
     (perform-arithmetic - expr1 expr2 env))
    (('* expr1 expr2)
     (perform-arithmetic * expr1 expr2 env))
    (('/ expr1 expr2)
     (perform-arithmetic / expr1 expr2 env))
    (('modulo expr1 expr2)
     (perform-arithmetic modulo expr1 expr2 env))
    (('expt expr1 expr2)
     (perform-arithmetic expt expr1 expr2 env))
    ;; String concatenation
    (('string-append expr1 expr2)
     (let* ((value1 env (eval-awke/string expr1 env))
            (value2 env (eval-awke/string expr2 env)))
       (values (string-append value1 value2) env)))
    ;; Comparisons
    (('< expr1 expr2)
     (perform-comparison string<? < expr1 expr2 env))
    (('<= expr1 expr2)
     (perform-comparison (negate string>?) <= expr1 expr2 env))
    (('equal? expr1 expr2)
     (perform-comparison string=? = expr1 expr2 env))
    (('not-equal? expr1 expr2)
     (perform-comparison (negate string=?) (negate =) expr1 expr2 env))
    (('> expr1 expr2)
     (perform-comparison string>? > expr1 expr2 env))
    (('>= expr1 expr2)
     (perform-comparison (negate string<?) >= expr1 expr2 env))
    ;; String matching
    (('string-match str-expr pat-expr)
     (let* ((str env (eval-awke/string str-expr env))
            (pat env (eval-awke/regex pat-expr env)))
       (if (string-match pat str)
           (values 1 env)
           (values 0 env))))
    (('not-string-match str-expr pat-expr)
     (let* ((str env (eval-awke/string str-expr env))
            (pat env (eval-awke/regex pat-expr env)))
       (if (string-match pat str)
           (values 0 env)
           (values 1 env))))
    ;; Array operations
    (('array-member? key name)
     (let ((key env (eval-key key env)))
       (if (env-array-member? name key env)
           (values 1 env)
           (values 0 env))))
    (('array-delete! key name)
     (let ((key env (eval-key key env)))
       (env-array-delete! name key env)
       (values uninitialized-scalar env)))
    ;; Boolean expressions
    (('and expr1 expr2)
     (let ((bool1 env (eval-awke/boolean expr1 env)))
       (if bool1
           (let ((bool2 env (eval-awke/boolean expr2 env)))
             (values (if bool2 1 0) env))
           (values 0 env))))
    (('or expr1 expr2)
     (let ((bool1 env (eval-awke/boolean expr1 env)))
       (if bool1
           (values 1 env)
           (let ((bool2 env (eval-awke/boolean expr2 env)))
             (values (if bool2 1 0) env)))))
    (('not expr)
     (let ((value env (eval-awke/boolean expr env)))
       (if value
           (values 0 env)
           (values 1 env))))
    ;; Setting variables
    (('set! lvalue expr)
     (let* ((resolved-lvalue env (resolve-lvalue lvalue env))
            (value env (eval-awke expr env)))
       (perform-set! resolved-lvalue value env)))
    (('set-op! setop lvalue expr)
     (let* ((resolved-lvalue env (resolve-lvalue lvalue env))
            (x env (eval-awke/number resolved-lvalue env))
            (y env (eval-awke/number expr env)))
       (case setop
         ((+) (perform-set! resolved-lvalue (+ x y) env))
         ((-) (perform-set! resolved-lvalue (- x y) env))
         ((*) (perform-set! resolved-lvalue (* x y) env))
         ((/) (perform-set! resolved-lvalue (/ x y) env))
         ((modulo) (perform-set! resolved-lvalue (modulo x y) env))
         ((expt) (perform-set! resolved-lvalue (expt x y) env)))))
    (('post-incr! lvalue)
     (perform-nudge 1+ #t lvalue env))
    (('post-decr! lvalue)
     (perform-nudge 1- #t lvalue env))
    (('pre-incr! lvalue)
     (perform-nudge 1+ #f lvalue env))
    (('pre-decr! lvalue)
     (perform-nudge 1- #f lvalue env))
    ;; Function application
    (('apply (? built-in? name) exprs ...)
     (let ((proc (env-ref/procedure name env)))
       (apply proc env exprs)))
    (('apply name exprs ...)
     (let ((proc (env-ref/procedure name env))
           (args env (map/env eval-awke/argument exprs env)))
       (apply proc env args)))
    ;; Lvalues
    ((? symbol? name)
     (values (env-ref/scalar! name env) env))
    (('array-ref key name)
     (let ((key env (eval-key key env)))
       (values (env-array-ref name key env) env)))
    (('$ field-expr)
     (let* ((field env (eval-awke/number field-expr env))
            (nf env (eval-awke/number 'NF env)))
       (cond
        ((zero? field)
         (values (or (env-record env) uninitialized-scalar) env))
        ((and (> field 0) (<= field nf))
         (values (list-ref (env-fields env) (1- field)) env))
        (else
         (values uninitialized-scalar env)))))
    ;; Literals
    ((? string?)
     (values expr env))
    ((? number?)
     (values expr env))
    (('re pattern)
     (eval-awke `(string-match ($ 0) ,pattern) env))
    (_ (error "not an Awk expression:" expr))))

(define (eval-awke* env exprs)
  "Evaluate each of the expressions @var{exprs} in the environment
@var{env} and return the result of the last expression along with the
updated environment.  If @var{exprs} is the empty list, return
@code{uninitialized-scalar} and @var{env} unchanged."
  (match exprs
    (() (values uninitialized-scalar env))
    ((expr) (eval-awke expr env))
    ((expr . rest)
     (eval-awke* (second-value (eval-awke expr env)) rest))))


;; Typed evaluation procedures

(define (eval-awke/string expr env)
  "Evaluate the expression @var{expr} in the environment @var{env},
returning the result as a string along with the updated environment."
  (match expr
    ((? string?) (values expr env))
    ((? number?) (values (number->string expr) env))
    ((? numeric-string?) (values (numeric-string-string expr) env))
    (_ (let ((result env (eval-awke expr env)))
         (eval-awke/string result env)))))

(define (eval-awke/regex expr env)
  "Evaluate the expression @var{expr} in the environment @var{env},
returning the result as a string along with the updated environment.  If
@var{expr} is a regex, do not evaulate it but return it directly."
  ;; XXX: Hack our way around an old GCC Awk script bug:
  ;; <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=78766>.
  (define hack-pattern
    (match-lambda
      ("^{" (values "^[{]" env))
      (pattern (values pattern env))))
  (match expr
    (('re pattern) (values (hack-pattern pattern) env))
    (_ (let ((pattern env (eval-awke/string expr env)))
         (values (hack-pattern pattern) env)))))

(define (eval-awke/number expr env)
  "Evaluate the expression @var{expr} in the environment @var{env},
returning the result as a number along with the updated environment."
  (match expr
    ((? number?) (values expr env))
    ((? numeric-string?) (values (numeric-string-number expr) env))
    ((? string?) (values (or (string->number expr) 0) env))
    (_ (let ((result env (eval-awke expr env)))
         (eval-awke/number result env)))))

(define (eval-awke/boolean expr env)
  "Evaluate the expression @var{expr} in the environment @var{env},
returning the result as a boolean along with the updated environment."
  (match expr
    ((? number?) (values (not (zero? expr)) env))
    ((? numeric-string?) (values (not (string-null?
                                       (numeric-string-string expr))) env))
    ((? string?) (values (not (string-null? expr)) env))
    (_ (let ((result env (eval-awke expr env)))
         (eval-awke/boolean result env)))))

(define (eval-awke/argument expr env)
  "Evaluate the expression @var{expr} in the environment @var{env},
returning two values: a result suitable to be passed as an argument to a
user-defined function; and the updated environment."
  (match expr
    ((? symbol? name)
     (match (or (hashq-get-handle (env-locals env) name)
                (hashq-get-handle (env-globals env) name))
       (#f (values (make-uninitialized-argument name env) env))
       ((_ . (? procedure?)) (error "function used as an argument:" name))
       ((_ . value) (values value env))))
    (_ (eval-awke expr env))))

(define (eval-awke/character expr env)
  "Evaluate the expression @var{expr} in the environment @var{env},
returning the result as a character along with the updated environment.
If the result of evaluation is a number, return a character with that
code point.  If the result is a string, return the first character of
the string or @code{#\\nul} if the string is null."
  (match expr
    ((? number?) (values (integer->char expr) env))
    ((? numeric-string?) (values (integer->char
                                  (numeric-string-number expr)) env))
    ("" (values #\nul env))
    ((? string?) (values (string-ref expr 0) env))
    (_ (let ((result env (eval-awke expr env)))
         (eval-awke/character result env)))))


;; Evaluation helpers

(define (eval-key key env)
  "Evaluate the array key @var{key} in the environment @var{env},
returning both a string key and the updated environment."
  (define (eval-expr-with-subsep expr env)
    (let* ((subsep env (eval-awke/string 'SUBSEP env))
           (part env (eval-awke/string expr env)))
      (values (string-append subsep part) env)))
  (match key
    (('index first-expr rest-exprs ...)
     (let* ((first-part env (eval-awke/string first-expr env))
            (rest-parts env (map/env eval-expr-with-subsep rest-exprs env)))
       (values (apply string-append first-part rest-parts) env)))
    (expr (eval-awke/string expr env))))

(define (resolve-lvalue lvalue env)
  "Evaluate any sub-expressions in @var{lvalue} in the environment
@var{env} and return two values: an lvalue with the results substituted
in for the original expressions; and the updated environment.  This can
be used to evaluate an lvalue twice without performing the side-effects
of its evaluation twice."
  (match lvalue
    ((? symbol?) (values lvalue env))
    (('array-ref key name)
     (let ((key env (eval-key key env)))
       (values `(array-ref ,key ,name) env)))
    (('$ expr)
     (let ((field env (eval-awke/number expr env)))
       (values `($ ,field) env)))))

(define (perform-set! lvalue value env)
  "Set @var{lvalue} to the scalar @var{value} in the environment
@var{env}, returning @var{value} and the updated environment."

  (define (list-set lst k val dflt)
    (if (< k (length lst))
        (append (list-head lst k)
                (cons val (list-tail lst (1+ k))))
        (append lst
                (take (circular-list dflt) (- k (length lst)))
                (list val))))

  (match lvalue
    ('NF (values value (set-env-nf env value)))
    ((? symbol? name)
     (env-set/scalar! name value env)
     (values value env))
    (('array-ref key name)
     (let ((key env (eval-key key env)))
       (env-array-set! name key value env)
       (values value env)))
    (('$ expr)
     (let* ((raw-field env (eval-awke/number expr env))
            (field (truncate raw-field)))
       (when (< field 0)
         (error "bad field index:" field))
       (if (zero? field)
           (let* ((string-value env (eval-awke/string value env))
                  (env (set-env-record env string-value)))
             (values string-value env))
           (let* ((field (1- field))
                  (old-fields (env-fields env))
                  (fields (list-set old-fields field value
                                    uninitialized-scalar)))
             (values value (set-env-fields env fields))))))))

(define (perform-arithmetic op expr1 expr2 env)
  "Perform @var{op} on the results of evaluating the expressions
@var{expr1} and @var{expr2} numerically in the environment @var{env}.
The return values are the result from @var{op} and the updated
environment."
  (let* ((value1 env (eval-awke/number expr1 env))
         (value2 env (eval-awke/number expr2 env)))
    (values (op value1 value2) env)))

(define (compare-numerically? value1 value2)
  "Check if the scalars @var{value1} and @var{value2} can be compared
numerically."
  (or (and (number? value1) (number? value2))
      (and (number? value1) (numeric-string? value2))
      (and (numeric-string? value1) (number? value2))))

(define (perform-comparison string-op number-op expr1 expr2 env)
  "Perform a comparison on the results of evaluating the expressions
@var{expr1} and @var{expr2} in the environment @var{env}.  If the
results can be compared numerically, @var{number-op} will be used as the
comparison operator.  If they cannot, @var{string-op} will be used.  The
return values are the result of the comparison and the updated
environment."
  (let* ((value1 env (eval-awke expr1 env))
         (value2 env (eval-awke expr2 env)))
    (if (compare-numerically? value1 value2)
        (let* ((value1 env (eval-awke/number value1 env))
               (value2 env (eval-awke/number value2 env)))
          (if (number-op value1 value2)
              (values 1 env)
              (values 0 env)))
        (let* ((value1 env (eval-awke/string value1 env))
               (value2 env (eval-awke/string value2 env)))
          (if (string-op value1 value2)
              (values 1 env)
              (values 0 env))))))

(define (perform-nudge op return-old? lvalue env)
  "Update @var{lvalue} in the environment @var{env} by performing
@var{op} on it.  If @var{return-old?} is true, return the value as it
was before nudging.  Otherwise, return the result of @var{op}.  In
either case, the updated environment is returned as the second value."
  (let* ((resolved-lvalue env (resolve-lvalue lvalue env))
         (old-value env (eval-awke/number resolved-lvalue env))
         (new-value env (perform-set! resolved-lvalue (op old-value) env)))
    (values (if return-old? old-value new-value) env)))

(define awk-conversion-adapter
  (make-conversion-adapter eval-awke/string
                           eval-awke/number
                           eval-awke/character))

(define (perform-printf env format-expr . args)
  "Evaluate @var{format-expr} in the environment @var{env} and use the
result as a format string with arguments @var{args}.  The result is
written to the output port of @var{env}.  Note that @var{args} is a list
of expressions that will be evaluated lazily (only if needed).  This
procedure returns two values: @code{uninitialized-scalar} and the
updated environment."
  (let* ((format-string env (eval-awke/string format-expr env))
         (format (parse-file-format format-string #:escaped? #f))
         (result env (apply fold-file-format awk-conversion-adapter
                            env format args)))
    (display result (env-out env))
    (values uninitialized-scalar env)))

(define (redirect-input env source-expr redirector expr)
  (let* ((source env (eval-awke/string source-expr env))
         (port env (redirector env source)))
    (if port
        (let* ((in (env-in env))
               (input-redirected? (env-input-redirected? env))
               (env (set-fields env
                      ((env-in) port)
                      ((env-input-redirected?) #t)))
               (result env (eval-awke expr env))
               (env (set-fields env
                      ((env-in) in)
                      ((env-input-redirected?) input-redirected?))))
          (values result env))
        (values -1 env))))

(define (redirect-output env target-expr redirector expr)
  (let* ((target env (eval-awke/string target-expr env))
         (port env (redirector env target)))
    (if port
        (let* ((out (env-out env))
               (env (set-env-out env port))
               (result env (eval-awke expr env))
               (env (set-env-out env out)))
          (values result env))
        (values -1 env))))

(define *continue-prompt* (make-prompt-tag "continue"))

(define *break-prompt* (make-prompt-tag "break"))

(define *return-prompt* (make-prompt-tag "return"))

(define *next-prompt* (make-prompt-tag "next"))

(define *exit-prompt* (make-prompt-tag "exit"))

(define (eval-loop-body env exprs)
  "Evaluate @var{exprs} in the envrionment @var{env} with break and
continue prompts in place.  This procedure returns three values: whether
to continue the loop, the result of evaluating the last expression, and
the updated environment."
  (call-with-prompt *break-prompt*
    (lambda ()
      (call-with-prompt *continue-prompt*
        (lambda ()
          (let ((value env (eval-awke* env exprs)))
            (values #t value env)))
        (lambda (cont env)
          (values #t uninitialized-scalar env))))
    (lambda (cont env)
      (values #f uninitialized-scalar env))))


;; Built-ins

(define (unimplemented-built-in name)
  (lambda _
    (error "unimplemented built-in used:" name)))

(define replacement->items
  (let ((stop-chars (char-set #\\ #\&)))
    (lambda* (replacement #:optional (start 0)
                          (end (string-length replacement)))
      (let loop ((k start) (acc '(pre)))
        (match (string-index replacement stop-chars k end)
          (#f (reverse (cons* 'post (substring replacement k end) acc)))
          (idx (let ((acc* (cons (substring replacement k idx) acc)))
                 (match (string-ref replacement idx)
                   (#\\ (match (and (< (1+ idx) end)
                                    (string-ref replacement (1+ idx)))
                          (#\\ (loop (+ idx 2) (cons "\\" acc*)))
                          (#\& (loop (+ idx 2) (cons "&" acc*)))
                          (_ (loop (1+ idx) (cons "\\" acc*)))))
                   (#\& (loop (1+ idx) (cons 0 acc*)))))))))))

(define (perform-sub! sub ere repl lvalue env)
  (let* ((ere env (eval-awke/regex ere env))
         (repl env (eval-awke/string repl env))
         (items (replacement->items repl))
         (resolved-lvalue env (resolve-lvalue lvalue env))
         (in env (eval-awke/string resolved-lvalue env)))
    (let* ((result count (sub ere in items))
           (_ env (perform-set! resolved-lvalue result env)))
      (values count env))))

(define *built-ins*
  `(;; Arithmetic built-ins
    (atan2 . ,(lambda (env y x)
                (let* ((y env (eval-awke/number y env))
                       (x env (eval-awke/number x env)))
                  (values (atan y x) env))))
    (cos . ,(lambda (env x)
              (let ((x env (eval-awke/number x env)))
                (values (cos x) env))))
    (sin . ,(lambda (env x)
              (let ((x env (eval-awke/number x env)))
                (values (sin x) env))))
    (exp . ,(lambda (env x)
              (let ((x env (eval-awke/number x env)))
                (values (exp x) env))))
    (log . ,(lambda (env x)
              (let ((x env (eval-awke/number x env)))
                (values (log x) env))))
    (sqrt . ,(lambda (env x)
               (let ((x env (eval-awke/number x env)))
                 (values (sqrt x) env))))
    (int . ,(lambda (env x)
              (let ((x env (eval-awke/number x env)))
                (values (truncate x) env))))
    (rand . ,(unimplemented-built-in 'rand))
    (srand . ,(unimplemented-built-in 'srand))
    ;; String built-ins
    (gsub . ,(lambda* (env ere repl #:optional (in '($ 0)))
               (define (sub ere in items)
                 (let* ((count 0)
                        (pitems (map (match-lambda
                                       ((? symbol? s) s)
                                       (0 (lambda (m)
                                            (set! count (1+ count))
                                            (match:substring m)))
                                       (x (lambda _
                                            (set! count (1+ count))
                                            x)))
                                     items))
                        (result (apply regexp-substitute/global
                                       #f ere in pitems)))
                   (values result count)))
               (perform-sub! sub ere repl in env)))
    (index . ,(lambda (env s t)
                (let* ((s env (eval-awke/string s env))
                       (t env (eval-awke/string t env)))
                  (match (string-contains s t)
                    (#f (values 0 env))
                    (i (values (1+ i) env))))))
    (length . ,(lambda* (env #:optional (s '($ 0)))
                 (let* ((s env (eval-awke/string s env)))
                   (values (string-length s) env))))
    (match . ,(lambda (env s ere)
                (let* ((s env (eval-awke/string s env))
                       (ere env (eval-awke/regex ere env)))
                  (match (string-match ere s)
                    (#f (env-set/scalar! 'RSTART 0 env)
                        (env-set/scalar! 'RLENGTH -1 env)
                        (values 0 env))
                    (m (let ((len (- (match:end m) (match:start m))))
                         (env-set/scalar! 'RSTART (1+ (match:start m)) env)
                         (env-set/scalar! 'RLENGTH len env)
                         (values (1+ (match:start m)) env)))))))
    (split . ,(lambda* (env s a #:optional (fs 'FS))
                (let* ((s env (eval-awke/string s env))
                       (a (match a
                            ((? symbol?) (env-ref/array! a env))
                            (_ (error "scalar passed to split"))))
                       (fs env (eval-awke/regex fs env))
                       (parts (string-split/awk s fs))
                       (len (length parts)))
                  (hash-clear! a)
                  (for-each (lambda (k v) (hash-set! a k v))
                            (map number->string (iota len 1)) parts)
                  (values len env))))
    (sprintf . ,(lambda (env fmt . exprs)
                  (let* ((fmt env (eval-awke/string fmt env))
                         (format (parse-file-format fmt #:escaped? #f)))
                    (apply fold-file-format awk-conversion-adapter
                           env format exprs))))
    ;; This is a Gawk extension, but it is used by the GNU Build System.
    (strtonum . ,(lambda (env s)
                   (let* ((s env (eval-awke/string s env))
                          (n (if (or (string-prefix? "0x" s)
                                     (string-prefix? "0X" s))
                                 (string->number (substring s 2) 16)
                                 (string->number (string-trim s)))))
                     (values (or n 0) env))))
    (sub . ,(lambda* (env ere repl #:optional (in '($ 0)))
              (define (sub ere in items)
                (match (string-match ere in)
                  (#f (values in 0))
                  (m (values (apply regexp-substitute #f m items) 1))))
              (perform-sub! sub ere repl in env)))
    (substr . ,(lambda* (env s m #:optional n)
                 (define (clamp x lb ub)
                   (truncate (max lb (min ub x))))
                 (let* ((s env (eval-awke/string s env))
                        (m env (eval-awke/number m env))
                        (m (clamp (1- m) 0 (string-length s)))
                        (n (or n (string-length s)))
                        (n env (eval-awke/number n env))
                        (end (clamp (+ m n) m (string-length s))))
                   (values (substring s m end) env))))
    (tolower . ,(lambda (env s)
                  (let ((s env (eval-awke/string s env)))
                    (values (string-downcase s) env))))
    (toupper . ,(lambda (env s)
                  (let ((s env (eval-awke/string s env)))
                    (values (string-upcase s) env))))
    ;; I/O, and general built-ins
    (close . ,(lambda (env s)
                (let ((s env (eval-awke/string s env)))
                  (close-redirect env s))))
    (system . ,(lambda (env s)
                 (let ((s env (eval-awke/string s env)))
                   (values (system s) env))))))

(define *built-in-names* (map car *built-ins*))

(define (built-in? name)
  "Check whether @var{name} is the name of a built-in function."
  (memq name *built-in-names*))


;; Gawk functions

(define *gawk-functions*
  `((asorti . ,(lambda (env source dest*)
                 (define dest
                   (match dest*
                     ((? hash-table?)
                      (hash-clear! dest*)
                      dest*)
                     ((? uninitialized-argument?)
                      (let ((dest (make-hash-table)))
                        (set-uninitialized-argument! dest* dest)
                        dest))
                     (_ (error "asorti: 'dest' argument not an array"))))
                 (unless (hash-table? source)
                   (error "asorti: 'source' argument not an array"))
                 (let* ((indexes (hash-map->list (lambda (k v) k) source))
                        (len (length indexes)))
                   (for-each (cut hash-set! dest <> <>)
                             (map number->string (iota len 1))
                             (sort indexes string<?))
                   (values len env))))))


;; Function definitions

(define* (set-locals names values #:optional (locals (make-hash-table)))
  (match names
    (() locals)
    ((name . names-rest)
     (match values
       (()
        (hashq-set! locals name (make-uninitialized-local name locals))
        (set-locals names-rest '() locals))
       ((value . values-rest)
        (hashq-set! locals name value)
        (set-locals names-rest values-rest locals))))))

(define (make-function arg-names exprs)
  "Make a user-defined function with argument names @var{arg-names} and
code @var{exprs}."
  (lambda (env . args)
    (let* ((locals (set-locals arg-names args))
           (old-locals (env-locals env))
           (env (set-env-locals env locals))
           (result env (call-with-prompt *return-prompt*
                         (lambda ()
                           (values uninitialized-scalar
                                   (second-value (eval-awke* env exprs))))
                         (lambda (cont result env)
                           (values result env)))))
      (values result (set-env-locals env old-locals)))))

(define (eval-function-definition item env)
  "Evaluate the function definition @var{item} with environment
@var{env}, and return the updated environment.  If @var{item} is not a
function definition, do nothing."
  (match item
    (('defun name (arg-names ...) exprs ...)
     (when (any (lambda (x) (memq x *special-variable-names*)) arg-names)
       (error "function has a parameter that shadows a special variable:"
              name))
     (env-set/procedure! name (make-function arg-names exprs) env)
     env)
    (_ env)))


;; Items (excluding function definitions)

(define (eval-special-item type item env)
  "Evaluate the @var{type} special item @var{item} with environment
@var{env}, and return the updated environment.  The @var{type} parameter
must be either @code{'begin} or @code{'end}.  If @var{item} is not a
special item with type @var{type}, do nothing."
  (match item
    (((? (lambda (x) (eq? x type))) exprs ...)
     (call-with-prompt *next-prompt*
       (lambda ()
         (second-value (eval-awke* env exprs)))
       (lambda _
         (error "awk: next statement in special item"))))
    (_ env)))

(define (eval-begin-item item env)
  "Evaluate the @code{'begin} item @var{item} with environment
@var{env}, and return the updated environment.  If @var{item} is not a
@code{'begin} item, do nothing."
  (eval-special-item 'begin item env))

(define (eval-end-item item env)
  "Evaluate the @code{'end} item @var{item} with environment @var{env},
and return the updated environment.  If @var{item} is not a @code{'end}
item, do nothing."
  (eval-special-item 'end item env))

(define (eval-item item env)
  "Evaluate the regular item @var{item} with environment @var{env}, and
return the updated environment. If @var{item} is a special item or a
function definition, do nothing.  If @var{item} is not an item, raise an
error."
  (match item
    (((or 'begin 'end 'defun) . _) env)
    ((#t exprs ...)
     (second-value (eval-awke* env exprs)))
    ((pattern exprs ...)
     (let ((result env (eval-awke/boolean pattern env)))
       (if result
           (second-value (eval-awke* env exprs))
           env)))
    (_ (error "not an Awk item:" item))))


;; Initialization and file processing

(define (make-default-env out files field-separator)
  "Make the initial environment with output port @var{out}, argument
list @var{files}, and field separator @var{field-separator}."
  (let ((globals (make-hash-table))
        (argv (make-hash-table))
        (argc (length files)))
    ;; Initialize special variables.
    (for-each (match-lambda
                ((key . value) (hashq-set! globals key value)))
              *special-variables*)
    ;; Initialize the ARGV array.
    (for-each (lambda (k v)
                (hash-set! argv k v))
              (map number->string (iota argc)) files)
    ;; Update ARGC and ARGV.
    (hashq-set! globals 'ARGC argc)
    (hashq-set! globals 'ARGV argv)
    ;; Update FS.
    (hashq-set! globals 'FS field-separator)
    ;; Set built-in functions.
    (for-each (match-lambda
                ((key . value) (hashq-set! globals key value)))
              *built-ins*)
    ;; Set Gawk functions.
    (for-each (match-lambda
                ((key . value) (hashq-set! globals key value)))
              *gawk-functions*)
    (make-env #f out #f '() #f '() (make-hash-table) globals)))

(define (eval-assignments assigns env)
  "Set each name-value pair in @var{assigns} in the environment
@var{env}, and return the updated environment."
  (for-each (match-lambda
              ((name . value)
               (env-set/scalar! name value env)))
            assigns)
  env)

(define (load-file filename env)
  "Open @var{filename} and store the resulting port in the @code{env-in}
field in @var{env}.  This procedure also updates the @code{'FILENAME}
and @code{'FNR} variables."
  (let ((port (if (equal? filename "-")
                  (current-input-port)
                  (open-input-file filename))))
    (env-set/scalar! 'FILENAME filename env)
    (env-set/scalar! 'FNR 0 env)
    (set-env-in env port)))

(define (process-file items filename env)
  "Evaluate each of the regular items in @var{items} with the
environment @var{env} over each of the records in @var{filename}."
  (let ((env (load-file filename env)))
    (let loop ((env (read-record env)))
      (if (env-record env)
          (let ((env (call-with-prompt *next-prompt*
                       (lambda ()
                         (fold eval-item env items))
                       (lambda (cont env)
                         env))))
            (loop (read-record env)))
          env))))

(define* (%eval-awk items files assigns #:optional
                    (out (current-output-port))
                    #:key (field-separator " "))

  (define* (eval-end-items env #:optional (return-value 0))
    (call-with-prompt *exit-prompt*
      (lambda ()
        (fold eval-end-item env items)
        return-value)
      (lambda (cont value env)
        value)))

  (call-with-prompt *exit-prompt*
    (lambda ()
      (let* ((env (make-default-env out files field-separator))
             (env (fold eval-function-definition env items))
             (env (eval-assignments assigns env))
             (env (fold eval-begin-item env items)))
        (let loop ((k 0) (env env))
          (if (< k (env-ref/scalar! 'ARGC env))
              (let ((file env (eval-awke `(array-ref ,k ARGV) env)))
                (loop (1+ k) (process-file items file env)))
              (eval-end-items env)))))
    (lambda (cont value env)
      (eval-end-items env value))))


;; Command-line interface

(define *help-message* "\
Usage: awk [OPTION]... PROGRAM [ARGUMENT]...
     | awk [OPTION]... -f PROGFILE [ARGUMENT]...
  -f, --file             read a program from a file
  -F, --field-separator  specify the default field separator
  -v, --assign           assign a variable before running the program
  -h, --help             display this help
  -V, --version          display version
")

(define *version-message*
  (format #f "awk (~a) ~a~%" %package-name %version))

(define *options-grammar*
  (make-options-grammar
   `((list file #\f)
     (value field-separator #\F)
     (list assign #\v)
     (message ("help" #\h) ,*help-message*)
     (message ("version" #\V) ,*version-message*))))

(define (get-items-and-args files args)
  (if (null? files)
      (match args
        ((items-string . args)
         (values (with-input-from-string items-string read-awk) args))
        (_ (error "no program specified")))
      (values (append-map (match-lambda
                            ("-" (read-awk))
                            (file (with-input-from-file file read-awk)))
                          files)
              args)))

(define (split-assignment assign)
  (match (string-index assign #\=)
    (#f (error "awk: bad assignment:" assign))
    (k (let ((name (string->symbol (substring assign 0 k)))
             (value (substring assign (1+ k))))
         (cons name value)))))

(define (awk . args)
  (let* ((options (parse-options args *options-grammar*))
         (files (or (assoc-ref options 'file) '()))
         (field-separator (or (assoc-ref options 'field-separator) " "))
         (assignments (or (assoc-ref options 'assign) '()))
         (args (or (assoc-ref options '()) '())))
    (let ((items arguments (get-items-and-args files args)))
      (when (getenv "AWK_DEBUG")
        (pretty-print items (current-error-port)))
      (exit
       (%eval-awk items (if (null? arguments) '("-") arguments)
                  (map split-assignment assignments) (current-output-port)
                  #:field-separator field-separator)))))

(define main awk)

;;; Local Variables:
;;; eval: (put 'set-fields 'scheme-indent-function 1)
;;; End:
