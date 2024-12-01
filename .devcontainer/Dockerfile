FROM ocaml/opam:debian-12-ocaml-5.1

# install basic dependencies
RUN opam install dune.3.17.0 core.v0.17.1 core_unix.v0.17.0 batteries.3.8.0 ppx_deriving

# install IDE/dev specific dependencies
RUN opam install ocaml-lsp-server utop ocamlformat
