(lang dune 3.17)

(name temporal-millet)

(version 0.1.0)

(generate_opam_files true)

(implicit_transitive_deps false)

(using menhir 2.1)

(cram enable)

(license MIT)

(authors "Danel Ahman")

(maintainers "Danel Ahman")

(source
 (uri "git+https://github.com/danelahman/temporal-millet.git"))

(homepage "https://danel.ahman.ee/temporal-millet/")

(package
 (name temporal-millet)
 (allow_empty)
 (synopsis
  "Prototype language with modal types and graded temporal effects")
 (description
  "Temporal Millet is a prototype programming language exploring how modal types and graded effect systems can be combined to specify and verify temporal properties of resources.")
 (depends
  (ocaml
   (>= 5.0))
  (menhir
   (>= 20231231))
  vdom
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.28.1)))))
