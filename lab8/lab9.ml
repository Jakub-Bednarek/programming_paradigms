type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec ltake(n, lxs) =
    match(n, lxs) with
      (0, _) -> []
    | (_, LNil) -> []
    | (n, LCons(x, xf)) -> x :: ltake(n-1, xf());;


let rec lup(power, value) =
    LCons(Float.pow(value, power), lup(power + 1,))
    
let to_power llist =
    let rec to_power_rec(lxs, pos, currentPow, currentVal) =
        match (currentPow, lxs) with
          (x, LCons(value, func)) -> if pos = currentPow then LCons(currentVal, function () -> to_power_rec(func(), pos +. 1., 1., 1.))
                                     else LCons(currentVal, function () -> to_power_rec(lxs, pos, currentPow +. 1., currentVal *. value))
    in to_power_rec(llist, 1., 1., 1.);;

let rec lfrom k = LCons(k, function () -> if k = 50. then LNil else lfrom(k +. 1.));;

ltake(10, to_power(lfrom 5.));;