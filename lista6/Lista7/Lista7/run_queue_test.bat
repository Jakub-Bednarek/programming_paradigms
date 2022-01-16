ocamlc -c queue_list.mli
ocamlc -c queue_list.ml
ocamlc -c lista7zad1.ml
ocamlc -c queue_double_list.mli
ocamlc -c queue_double_list.ml
ocamlc -o lista7zad1 queue_list.cmo queue_double_list.cmo lista7zad1.cmo
ocamlrun lista7zad1
