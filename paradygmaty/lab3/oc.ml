(* Zadanie 2 *)

    let getPositions a list =
        if list == [] then raise (Failure "Array is empty!")
        else let rec check (list, a, position, accum) =
            match list with
            | [] -> List.rev accum
            | hd::tl -> if List.hd list == a then check(List.tl list, a, position + 1, (position :: accum))
                    else check(List.tl list, a, position + 1, accum)
        in check(list, a, 0, []);;


(* Zadanie 1 *)
let calcX a x n =
    a -. x ** n;;

let find2Values a n =
    let rec fin2 a n left right =
        if (a -. left ** n) *. (a -. right ** n) < 0. then (left, right)
        else fin2(a, n, left, right +. 1.)
    in fin2(a, n, 0., 1.);;


let findSqrt a n eps =
    let rec find a n left right =
        if (calcX a left n -. calcX a right n) < eps then calcX a left n +. calcX a right n /. 2.
        else if calcX a left n +. calcX a right n /. 2. < 0. then find a n (calcX a left n +. calcX a right n /. 2.) right
        else find a n left (calcX a left n +. calcX a right n /. 2.) in
    if a < 0. then find a n 0. 1.
    else find a n 1. a;;
