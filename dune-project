(lang dune 3.6)

(name aoc)

(generate_opam_files true)

(source
 (github kolmodin/advent-of-code-2022))

(authors "Lennart Kolmodin")

(maintainers "Lennart Kolmodin")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aoc)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune base stdio)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
