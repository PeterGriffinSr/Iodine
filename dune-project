(lang dune 3.19)
(using menhir 3.0)

(name Iodine)

(generate_opam_files true)

(source
 (github PeterGriffnSr/Iodine))

(authors "Codezz-ops <codezz-ops@protonmail.com>")

(maintainers "Codezz-ops <codezz-ops@protonmail.com>")

(license MIT)

(package
 (name Iodine)
 (synopsis "An interpreted purely functional language")
 (description "Iodine is a purely functional programming language interpreter written in OCaml.")
 (depends ocaml dune menhir)
 (tags
  ("functional" "interpreter" "language")))
