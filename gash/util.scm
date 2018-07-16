(define-module (gash util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export (
            conjoin
            disjoin
            wrap-command
            ))

(define (disjoin . predicates)
  (lambda (. arguments)
    (any (cut apply <> arguments) predicates)))

(define (conjoin . predicates)
  (lambda (. arguments)
    (every (cut apply <> arguments) predicates)))

(define (wrap-command command name)
  (lambda args
    (catch #t
      (cut apply command args)
      (lambda (key . args)
        (format (current-error-port) "~a: ~a ~a\n" name key args)
        1))))
