(define-module (gash io)

  #:use-module (srfi srfi-1)
  #:export (pke stdout stderr))

(define (output port o)
  (map (lambda (o) (display o port)) o)
  (newline port)
  (force-output port))

(define (stdout . o)
  (output (current-output-port) o)
  (last o))

(define (stderr . o)
  (output (current-error-port) o)
  (last o))

(define (pke . stuff)
  (newline (current-error-port))
  (display ";;; " (current-error-port))
  (write stuff (current-error-port))
  (newline (current-error-port))
  (car (last-pair stuff)))
