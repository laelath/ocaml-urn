(lang dune 3.11)

(name urn)

(generate_opam_files true)

(source
 (github laelath/ocaml-urn))

(authors "Justin Frank")

(maintainers "jpfrank@umd.edu")

(license MIT)

(package
 (name urn)
 (synopsis "Urn data structure for fast pure functional random sampling")
 (description "An implementation of the urn data structure described in https://dl.acm.org/doi/pdf/10.1145/3122955.3122959.")
 (documentation "https://laelath.github.io/ocaml-urn/urn/Urn/index.html")
 (depends (ocaml (>= 4.12)) dune)
 (version 20231124))
