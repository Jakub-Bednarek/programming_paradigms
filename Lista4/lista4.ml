(* Jakub Bednarek *)

(* Zadanie 2 *)
let rec f x = f x; 

(* Zadanie 3 *)
type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let testTree = Node(1, Node(2, Node(4, Empty, Empty), Empty), Node(3, Node(5, Empty, Node(6, Empty, Empty)), Empty));;
let testTree2 = Node(7, Node(10, Empty, Empty), Node(13, Node(5, Empty, Node(9, Empty, Empty)), Node(4, Empty, Node(2, Node(6, Empty, Empty), Node(3, Empty, Empty)))));;
let testTree3 = Empty;;

let breadthBT tree =
    let rec bfs (nodes, labels) =
        if nodes = [] then labels
        else 
            match (List.hd nodes) with
                Empty -> bfs(List.tl nodes, labels)
              | Node(v, l, r) -> bfs ((List.tl nodes) @ [l; r],  (v :: labels)) 
    in List.rev (bfs ([tree], []));;

breadthBT testTree = [1; 2; 3; 4; 5; 6];;
breadthBT testTree2 = [7; 10; 13; 5; 4; 9; 2; 6; 3];;
breadthBT testTree3 = [];;

(* Zadanie 4 *)
let outerLength tree =
    let rec outerLengthR (node, depth) =
        match node with
            Empty -> depth
        |   Node(v, l, r) -> outerLengthR(l, depth + 1) + outerLengthR(r, depth + 1)
    in outerLengthR(tree, 0);; 

outerLength testTree == 21;;
outerLength testTree2 == 38;;
outerLength testTree3 == 0;;

let innerLength tree =
    let rec innerLengthR (node, depth) =
        match node with
            Empty -> 0
        |   Node(v, l, r) -> depth + innerLengthR(l, depth + 1) + innerLengthR(r, depth + 1)
    in innerLengthR(tree, 0);; 

innerLength testTree == 9;;
innerLength testTree2 == 20;;
innerLength testTree3 = 0;;

(* Zadanie 5 *)
type 'a graph = Graph of ('a -> 'a list);;

let g = Graph
    (function 
      0 -> [3]
    | 1 -> [0; 2; 4]
    | 2 -> [1]
    | 3 -> []
    | 4 -> [0; 2]
    | n -> failwith ("Graph g: node " ^string_of_int n^ " doesn't exist")
    );;

let testGraph = Graph
   (function
      2 -> [6]
    | 3 -> [5]
    | 4 -> [8; 2]
    | 5 -> [4]
    | 6 -> [7; 10]
    | 7 -> []
    | 8 -> []
    | 10 -> []
    | 12 -> [3; 8]
    | n -> failwith ("Graph g: node " ^string_of_int n^ " doesn't exist")
    );;

let testGraph2 = Graph
    (function
      n -> failwith ("Graph g: node " ^string_of_int n^ " doesn't exist")
    );;

let depthSearch (Graph g) startNode =
    let rec dfs visited queue =
        match queue with
            [] -> []
            |   hd :: tl -> 
                    if List.mem hd visited then dfs visited tl
                    else hd :: dfs(hd :: visited)(g hd @ tl)
    in dfs [] [startNode];;

depthSearch g 4 = [4; 0; 3; 2; 1];;
depthSearch testGraph 12 = [12; 3; 5; 4; 8; 2; 6; 7; 10];;
(* depthSearch testGraph2 5 = [];; *)