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
        (add-to-list 'geiser-guile-load-path top)))))))
