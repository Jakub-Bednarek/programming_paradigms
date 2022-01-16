(* Jakub Bednarek *)

(* Zadanie 1 *)

type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec toLazyList xs =
    match xs with
     [] -> LNil
    | h :: t -> LCons(h, function () -> toLazyList t);;

let rec lTake (n, lxs) =
    match (n, lxs) with
    | (0, _) -> []
    | (_, LNil) -> []
    | (n, LCons(x, xs)) -> x :: lTake(n - 1, xs());;

let lrepeat (x, lxs) =
    let rec lrepeatIt (count, lList) =
        match (count, lList) with
        | (_, LNil) -> LNil
        | (0, LCons(_, lTail)) -> lrepeatIt (x, lTail())
        | (_, LCons(lHead, _)) -> LCons( lHead, function () -> lrepeatIt(count - 1, lList))
    in lrepeatIt (x, lxs);;


lTake(9, lrepeat(3, toLazyList[1; 2; 3])) = [1; 1; 1; 2; 2; 2; 3; 3; 3];;
lTake(6, lrepeat(2, toLazyList['a'; 'b'; 'c'])) = ['a'; 'a'; 'b'; 'b'; 'c'; 'c'];;

(* Zadanie 2 *)
let lfib =
    let rec lfibIt(last, oneBefore) =
        LCons(oneBefore, function () -> lfibIt(last + oneBefore, last))
    in lfibIt(1, 1);;

lTake(5, lfib) = [1; 1; 2; 3; 5];;
lTake(10, lfib) = [1; 1; 2; 3; 5; 8; 13; 21; 34; 55];;
