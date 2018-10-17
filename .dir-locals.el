((scheme-mode
  .
  ((eval . (put '<sh-case> 'scheme-indent-function 1))
   (eval . (put '<sh-defun> 'scheme-indent-function 1))
   (eval . (put '<sh-exec-let> 'scheme-indent-function 1))
   (eval . (put '<sh-for> 'scheme-indent-function 1))
   (eval . (put '<sh-until> 'scheme-indent-function 1))
   (eval . (put '<sh-while> 'scheme-indent-function 1))
   (eval . (put '<sh-with-redirects> 'scheme-indent-function 1))
   (eval . (put 'call-with-backquoted-input-port 'scheme-indent-function 1)))))
