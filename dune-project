(lang dune 3.12)

(name todo)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name todo)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  alcotest
  alcotest-lwt
  ocaml
  dune
  dream
  caqti
  caqti-lwt
  caqti-driver-sqlite3
  ppx_deriving
  ppx_yojson_conv)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
