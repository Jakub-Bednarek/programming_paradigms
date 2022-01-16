(* Jakub Bednarek *)
(* Zadanie 2 *)

let (//) f g = fun x -> f (g x);;

let add5 = function x -> x + 5;;
let sub15 = function x -> x - 15;;
let mult2 = function x -> x * 2;;
let div3 = function x -> x / 3;;

let func_list = [(add5, sub15); (mult2, div3)];;

let comp_pairs (funcs, operator) =
    let rec comp_rec (f_list, accum) =
        match f_list with
            | [] -> accum
            | (x, y) :: tl -> comp_rec(tl, (accum // (operator x y)))
    in comp_rec(funcs, function x -> x);;

let (+-) x y = function (v) -> (x v) + (y v) - (2 * v);;

let result = comp_pairs(func_list, (+-));;
result 5 == -10;;
