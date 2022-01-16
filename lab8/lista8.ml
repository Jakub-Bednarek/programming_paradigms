type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

(* Zadanie 3 *)
let func1 (list) = 
    let rec sum_elems (list_rest, accum) =
        match list_rest with
        | hd :: tl -> sum_elems(tl, (hd + 10) :: accum)
        | [] -> List.rev accum
    in sum_elems(list, []);;

let func2 (list) =
    let rec mult_elems (list_rest, accum) =
        match list_rest with
        | hd :: tl -> mult_elems(tl, (hd * 5) :: accum)
        | [] -> List.rev accum
    in mult_elems(list, []);;

let t1 = Node([1; 2; 3], Node([2; 3; 4], Node([4; 5; 6], Empty, Empty), Node([5; 6; 7], Empty, Empty)), Node([3; 4; 5], Node([6; 7; 8], Empty, Empty), Empty));;

let map_tree(tree, func) =
    let rec map_tree_rec(start_tree) =
        match start_tree with
        | Empty -> Empty
        | Node(list, left, right) -> Node(func(list), map_tree_rec(left), map_tree_rec(right))
        | Node(list, left, Empty) -> Node(func(list), map_tree_rec(left), Empty)
        | Node(list, Empty, right) -> Node(func(list), Empty, map_tree_rec(right))
        | Node(value, Empty, Empty) -> Node(value, Empty, Empty)
    in map_tree_rec(tree);;

map_tree(t1, func1) = Node([11; 12; 13], Node([12; 13; 14], Node([14; 15; 16], Empty, Empty), Node([15; 16; 17], Empty, Empty)), Node([13; 14; 15], Node([16; 17; 18], Empty, Empty), Empty));;
map_tree(t1, func2) = Node([5; 10; 15], Node([10; 15; 20], Node([20; 25; 30], Empty, Empty), Node([25; 30; 35], Empty, Empty)), Node([15; 20; 25], Node([30; 35; 40], Empty, Empty), Empty));;

(* Zadanie 1 *)

(*let rec to_list tree =
    match tree with
    | Node(v, l, r) -> to_list l @ v :: (to_list r)
    | Empty -> []

let count_trees(tree1, tree2) =
    let rec count_sublists(tree_list1, tree_list2, sum) =
        match tree_list1 with
        | l -> let rec count_inner(sub_list1, sub_list2) =
                        match (sub_list1, sub_list2) with
                        | (hd1::tl1, hd2::tl2) -> if hd1 = hd2 then count_inner(tl1, tl2) else 0
                        | ([], _) -> 0
                        | (_, []) -> 1
                    in sum + count_inner(l, tree_list2);
                    count_sublists(List.tl l, tree_list2, sum);
        | [] -> sum
    in count_sublists(to_list(tree1), to_list(tree2), 0);;*)

let t2 = Node(1, Node(2, Node(5, Empty, Empty), Empty), Node(5, Node(2, Node(5, Empty, Empty), Empty), Empty));;
let t3 = Node(2, Node(5, Empty, Empty), Empty);;

(*Dobre*)
let count_trees(tree, tree_to_compare) =
    let rec count_trees_rec(tree_rec) =
        match tree_rec with
          Empty -> if tree_rec = tree_to_compare then 1 else 0
        | Node(_, l, r) -> if tree_rec = tree_to_compare then 1
                           else count_trees_rec(l) + count_trees_rec(r)
    in count_trees_rec(tree);;

count_trees(t2, t3) = 2;;
