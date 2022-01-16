ocamlc -c queue_mut.mli
ocamlc -c queue_mut.ml
ocamlc -c lista7zad.ml
ocamlc -o lista7zad2 queue_mut.cmo lista7zad2.cmo
ocamlrun lista7zad2
