(define-module (gash io)

  :export (stdout stderr))

(define (output port o)
  (map (lambda (o) (display o port)) o)
  (newline port)
  (force-output port))

(define (stdout . o)
  (output (current-output-port) o)
  o)

(define (stderr . o)
  (output (current-error-port) o)
  o)
