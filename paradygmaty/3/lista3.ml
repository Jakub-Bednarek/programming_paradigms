(* Jakub Bednarek *)

(* Zadanie 2 *)
let curry3 f x y z = f(x, y, z);;
let curry3L = fun f -> fun x -> fun y -> fun z -> f(x, y, z);;

let uncurry3 f(x, y, z) = f x y z;;
let uncurry3L = fun f -> fun(x, y, z) -> f x y z;;

(* Zadanie 3 *)
let sumProd xs = 
	List.fold_left(fun(suma, iloczyn) elem -> (suma + elem, iloczyn * elem)) (0, 1) xs;;

sumProd([1;2;3;4;5]);;