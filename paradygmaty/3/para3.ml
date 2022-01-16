let euler round =
    let rec eulerCheck (round, n) =
        let rec countE (i, n, value) =
            if i > n then value
            else countE(i + 1, n, (value +. (1. /. float_of_int i) -. (log(float_of_int n)))) in
    
    if (countE(1, n, 0.) -. countE(1, n + 1, 0.)) < round then n + 1
    else eulerCheck(round, n + 1)
    in eulerCheck(round, 1);;

let delete list number =
    let rec deleteR list number index =
        match list with
            | [] -> []
            | h::t -> if index < number || index > (List.length list - number) then [] :: deleteR(list, number, index + 1)
                      else [List.hd list] :: deleteR(list, number, index + 1)
    in deleteR(list, number, 0);;