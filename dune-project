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
 (description "A longer description")
 (depends ocaml dune)
 (version 20231123))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
