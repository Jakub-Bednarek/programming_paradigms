(* Zadanie 1 *)

type plec = Kobieta | Mezczyzna;;
type osoba = string * string;;

let check (os:osoba)  = 
    let imie = String.lowercase (fst os) in
    let imie_lenght = String.length (fst os) in
    if imie . [imie_lenght - 1] == 'a' then Kobieta
    else Mezczyzna;;

let (tomek:osoba) = ("Tomasz", "Jakis");;
let (anna:osoba) = ("Anna", "Tomczyk");;
let (krystyna:osoba) = ("Krystyna", "Grzesiak");;
let (kuba:osoba) = ("Jakub", "Niewiadomski");;

check tomek = Mezczyzna;;
check anna = Kobieta;;
check krystyna = Kobieta;;
check kuba = Mezczyzna;;

type samochod = Samochod of string * string * int;;

let average_year (cars_list) =
    let rec average_year_rec (list, accum, elements) =
        match list with
        | [] -> accum / elements
        | Samochod(marka, model, rocznik) :: tl -> average_year_rec(tl, accum + rocznik, elements + 1)
    in average_year_rec(cars_list, 0, 0);;

let (opel:samochod) = Samochod("Opel", "Astra", 1999);;
let (renault:samochod) = Samochod("Renault", "Megane", 2004);;
let (toyota:samochod) = Samochod("Toyota", "Avensis", 2009);;
let (nissan:samochod) = Samochod("Nissa", "Micra", 2003);;

let cars = [opel; renault; toyota; nissan];;

average_year (cars);;

type 'a tree = Leaf of 'a | SingleNode of 'a tree | DoubleNode of 'a tree * 'a tree;;

let check_if_exists (tree, pred) =
    let rec traverse(tree) = 
        match tree with
        | Leaf(v) -> if(pred(v)) then 1 else 0
        | SingleNode(n) -> traverse(n)
        | DoubleNode(l, r) -> traverse(l) + traverse(r)
    in if(traverse(tree) > 0) then true else false;;

let less_than_5 x = if x < 5 then true else false;;
let less_than_0 x = if x < 0 then true else false;;