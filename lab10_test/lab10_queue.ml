module type OBSLUGA_KOLEJKI =
sig
    type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk
    exception Pusta of string
    val tworz_pusta: unit -> 'a tk
    val do_kolejki: 'a * 'a tk -> 'a tk
    val z_kolejki: 'a tk -> 'a tk
    val pierwszy_element: 'a tk -> 'a
    val kolejka_pusta: 'a tk -> bool
end;;

module Kolejka:OBSLUGA_KOLEJKI =
struct
    type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk;;

    exception Pusta of string;;

    let tworz_pusta() = KolejkaPusta;;

    let rec do_kolejki(value, queue) =
        match queue with
        | KolejkaPusta -> Skladowa(value, KolejkaPusta)
        | Skladowa(v, tl) -> match tl with
                            | Skladowa(_) -> Skladowa(v, do_kolejki(value, tl))
                            | KolejkaPusta -> Skladowa(v, Skladowa(value, KolejkaPusta))

    let z_kolejki queue =
        match queue with
        | Skladowa(_, tl) -> tl
        | KolejkaPusta -> KolejkaPusta

    let pierwszy_element queue =
        match queue with
        | Skladowa(v, _) -> v
        | KolejkaPusta -> raise (Pusta "pierwszy_element wywolano na pustej kolejce!")

    let kolejka_pusta queue =
        match queue with
        | KolejkaPusta -> true 
        | Skladowa(v, _) -> false
end;;

let q = ref (Kolejka.tworz_pusta());;

Kolejka.kolejka_pusta !q;;
Kolejka.pierwszy_element !q;;

q := Kolejka.do_kolejki(5, !q);;
q := Kolejka.do_kolejki(7774, !q);;
q := Kolejka.do_kolejki(9825, !q);;

Kolejka.kolejka_pusta !q;;

Kolejka.pierwszy_element !q;;

q := Kolejka.z_kolejki !q;;

Kolejka.kolejka_pusta !q;;

Kolejka.pierwszy_element !q;;