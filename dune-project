(lang dune 3.19)

(name timew-mcp)

(generate_opam_files true)

(source
 (github nkxxll/timew-mcp))

(authors "Niklas Kirschall <niki.nice1203@gmail.com>")

(maintainers "Niklas Kirschall <niki.nice1203@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name timew-mcp)
 (synopsis "timew mcp server")
 (description "Tiny timew mcp server for learing the mcp protocol")
 (depends
  ocaml
  core_unix
  logs
  (ocaml-lsp-server :with-dev)
  (ocamlformat :with-dev)
  ppx_deriving
  ppx_expect
  ppx_inline_test
  ppx_yojson_conv)
 (tags
  ("mcp" "learing" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
