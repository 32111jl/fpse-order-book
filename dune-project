(lang dune 3.16)
  (generate_opam_files true)
  (authors "Yours Truly" "Truly Yours")
  (maintainers "your@email.org")
  (package
   (name fpse-order-book) ; put in your own project name here
   (allow_empty)
   (synopsis "An OCaml library for Helpful Helloing")
   (description "A longer description")
   (depends
    (ounit2 (>= 2.2.7)) ; for each opam dependency list the version
    (core (>= 0.17.1)) ; `opam list core` will display which version of core you have
    (bisect_ppx (>= 2.8.3)) ; for code coverage analysis
 ))