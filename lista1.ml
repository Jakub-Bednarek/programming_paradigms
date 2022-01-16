(* Jakub Bednarek *)

(* Task 1 *)
let rec flatten1 list =
if list = [] then []
else (List.hd list) @ flatten1(List.tl list);;

flatten1([]);;
flatten1([[1; 3; 5]; [7; 5; 9]; [12; 39; 45]]);;
flatten1([["Kot"; "Pies"]; ["Niedzwiedz"; "Wilk"]; ["Bobr"]]);;

(* Task 2 *)
let rec count (ele, list) =
if list = [] then 0
else if List.hd list = ele then 1 + count(ele, List.tl list)
else 0 + count(ele, List.tl list);;

count(4, []);;
count(3, [7; 9; 3; 5; 2]);;
count("Ul", ["Ul"; "Tabaluga"; "Pies"; "Somsiad"; "PWR"; "Ul"; "Szczyt"]);;

(* Task 3*)
let rec replicate (n, ele) =
if n = 0 then []
else if n > 0 then ele :: replicate(n - 1, ele)
else [];;

replicate(-3, 12);;
replicate(5, "PWR");;
replicate(7, 1223);;

(* Task 4 *)
let rec sqrList list =
if list = [] then []
else (List.hd list) * (List.hd list) :: sqrList(List.tl list);;

sqrList([]);;
sqrList([1; 3; 5]);;
sqrList([-15; 79; 4]);;
sqrList(["pwr"; "uczelnia"]);;

(* Task 5 *)
let palindrome list =
if list = [] then true
else list = List.rev list;;

palindrome([]);;
palindrome([1;2;3;2;1]);;
palindrome([1;3;2]);;
palindrome([1;7;9;8;8;9;7;1]);;


(* Task 6 *)
let rec listLength list =
if list = [] then 0
else 1 + listLength(List.tl list);;

listLength([]);;
listLength([1; 3; 5; 7; 9; 11]);;
listLength(["Pies"; "Kot"; "Tabaluga"; "Ul"]);;
