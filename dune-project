(lang dune 3.19)

(name salt)

(generate_opam_files true)

(source (github username/reponame))

(authors "Author Name <author@example.com>")

(maintainers "Maintainer Name <maintainer@example.com>")

(documentation https://url/to/documentation)

(package
 (name salt)
 (synopsis "A Series of Rusty Interpreters")
 (description "A Series of Rusy Interpreters")
 (depends ocaml menhirLib fmt)
 (tags (rust pedagogical interpreter)))

(using menhir 3.0)
