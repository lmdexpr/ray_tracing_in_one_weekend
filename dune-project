(lang dune 3.7)

(name ray_tracing)

(generate_opam_files true)

(source
 (github lmdexpr/ray_tracing_in_one_weekend))

(authors "Yuki Tajiri")

(maintainers "Yuki Tajiri <lmdexpr@gmail.com>")

(license MIT)

(package
 (name ray_tracing)
 (synopsis "Ray Tracing in One Weekend")
 (depends
   (ocaml (>= 5.0.0))
   ocamlformat
   ocaml-lsp-server
   dune
   core))
