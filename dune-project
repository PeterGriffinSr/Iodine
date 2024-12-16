(lang dune 3.17)

(name Iodine)
(version 0.1.0)
(generate_opam_files true)
(license "BSD-3-Clause")
(authors "Codezz-ops")
(maintainers "Codezz-ops")

(package
 (name Iodine)
 (synopsis "A Programming Language")
 (description "A Programming Language")
 (depends
  (ocaml (>= 5.2.1))
  "dune"
  "ppx_deriving"))