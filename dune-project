(lang dune 3.16)
 (generate_opam_files true)
 (authors "Jonathan Liu" "Nicolas Liu")
 (maintainers "")
 (package
  (name fpse-order-book) ; put in your own project name here
  (allow_empty)
  (synopsis "An OCaml library for a performant order book")
(description "A OCaml library for managing and processing order books in financial trading systems.")
  (depends
   (ounit2 (>= 2.2.7)) ; for each opam dependency list the version
   (core (>= 0.17.1)) ; `opam list core` will display which version of core you have
   (bisect_ppx (>= 2.8.3)) ; for code coverage analysis
   (postgresql (>= 4.10.0)) ; for postgresql
))