(use-modules ((gnu packages bash) #:select (bash))
             (guix build-system)
             (guix packages)
             (guix store)
             (guix utils)
             (ice-9 match))

(define geesh
  (load (string-append (current-source-directory) "/../guix.scm")))

(define geesh-with-links
  (package
    (inherit geesh)
    (arguments
     (substitute-keyword-arguments (package-arguments geesh)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'install 'link-bash
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (symlink (string-append out "/bin/geesh")
                          (string-append out "/bin/sh"))
                 (symlink (string-append out "/bin/geesh")
                          (string-append out "/bin/bash")))))))))))

(define bash-without-bash
  (let ((bash-bag (package->bag bash)))
    (bag
      (inherit bash-bag)
      (build-inputs
       `(("bash" ,geesh-with-links)
         ,@(filter (match-lambda
                     ((name . _)
                      (not (member name '("bash")))))
                   (bag-build-inputs bash-bag)))))))

(bag->derivation (open-connection) bash-without-bash)
