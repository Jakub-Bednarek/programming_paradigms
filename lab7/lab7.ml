(* Zadanie 2 *)

let get_part (list, value, length) =
    let rec get_part_rec (list_start, current_length, out_list) =
        match list_start, current_length with
        | (hd::tl, 0) -> if hd = value then get_part_rec(tl, current_length + 1, hd :: out_list) else get_part_rec(tl, current_length, out_list)
        | ([], _) -> List.rev out_list
        | (hd::tl, l) -> if l = length then List.rev out_list else get_part_rec(tl, current_length + 1, hd :: out_list)
    in get_part_rec(list, 0, []);;

let list = [3; 5; 7; 9; 12; 15; 18; 31; 45; 99];;

get_part(list, 18, 3);;
get_part(list, 5, 10);;
get_part(list, 734, 100);;