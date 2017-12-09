(use-modules (gnu packages)
             (gnu packages autotools)
             (gnu packages code)
             (gnu packages guile)
             (gnu packages pkg-config)
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (guix packages))

(package
  (name "geesh")
  (version "0.1-rc")
  (source #f)
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("lcov" ,lcov)                ; For generating test coverage data
     ("pkg-config" ,pkg-config)))
  (inputs
   `(("guile" ,guile-2.2)))
  (arguments
   '(#:phases
     (modify-phases %standard-phases
       (add-before 'configure 'bootstrap
         (lambda _
           (zero? (system* "sh" "bootstrap")))))))
  (home-page #f)
  (synopsis #f)
  (description #f)
  (license #f))
