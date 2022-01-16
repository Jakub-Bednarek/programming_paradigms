(* Zadanie 1 *)
(* A *)
let funcA (a, i, f) =
    if i = 3 && f = 3. then ([a; a], [i])
    else ([a], [i; i]);;

funcA("pwr", 5, 4.);;
funcA("uczelnia", 3, 3.);;

(* B *)
let funcB (list, listList) =
    if List.hd list = 5. && List.hd (List.hd listList) = 5. then true
    else false;;

funcB([5.; 8.; 7.], [[5.; 7.]; [3.; 4.]]);;
funcB([3.; 4.], [[2.]; [7.]]);;

(* C *)
let funcC (listA, listB) =
    if List.hd listA = List.hd listB then 1
    else 0;;

funcC([1; 2; 3; 4; 5], [1; 3]);;

(* Zadanie 2 *)
let createPowers (x, n) =
    if n < 0 then raise(Failure "Invalid argument!")
    else let rec createPowersR (x, n, sum, count) =
            if count < n then [sum] @ createPowersR(x, n, sum * x, count + 1)
            else [sum] in
    createPowersR(x, n, 1, 0);;

createPowers(5, 0);;
createPowers(8, 4);;
createPowers(-12, 3);;
createPowers(7, -5);;