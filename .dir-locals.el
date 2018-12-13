((nil
  .
  ((indent-tabs-mode . nil)))
 (makefile-mode
  (indent-tabs-mode . t))
 (scheme-mode
  .
  ((geiser-active-implementations . (guile))
   (eval
    .
    (progn
      (let ((top (locate-dominating-file default-directory ".dir-locals.el")))
        (add-to-list 'geiser-guile-load-path top))))
   (eval . (put '<sh-case> 'scheme-indent-function 1))
   (eval . (put '<sh-defun> 'scheme-indent-function 1))
   (eval . (put '<sh-exec-let> 'scheme-indent-function 1))
   (eval . (put '<sh-for> 'scheme-indent-function 1))
   (eval . (put '<sh-until> 'scheme-indent-function 1))
   (eval . (put '<sh-while> 'scheme-indent-function 1))
   (eval . (put '<sh-with-redirects> 'scheme-indent-function 1))
   (eval . (put 'call-with-backquoted-input-port 'scheme-indent-function 1))
   (eval . (put 'make-script 'scheme-indent-function 1))
   (eval . (put 'sh:for 'scheme-indent-function 1))
   (eval . (put 'sh:subshell 'scheme-indent-function 0))
   (eval . (put 'sh:substitute-command 'scheme-indent-function 0))
   (eval . (put 'sh:with-redirects 'scheme-indent-function 1))
   (eval . (put 'call-with-break 'scheme-indent-function 0))
   (eval . (put 'call-with-continue 'scheme-indent-function 0))
   (eval . (put 'with-arguments 'scheme-indent-function 1))
   (eval . (put 'with-environ 'scheme-indent-function 1))
   (eval . (put 'with-variables 'scheme-indent-function 1)))))
