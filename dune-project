(lang dune 3.6)

(name gwtolatex)

(generate_opam_files true)

(source
 (github hgouraud/gwtolatex))

(authors "Henri Gouraud")

(maintainers "Henri Gouraud")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name gwtolatex)
 (synopsis "GeneWeb to LaTeX")
 (description "A longer description")
 (depends ocaml dune ezcurl ounit (unidecode (= "0.2.0")))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
