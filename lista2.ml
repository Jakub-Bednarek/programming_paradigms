(*Grzegorz Trajnowicz*)

(*zadanie 2a*)
let rec fib numer = 
	if(numer<0) then raise (Failure "Niepoprawna wartosc!")
	else if(numer = 0) then 0
	else if(numer = 1) then 1
	else fib (numer-1) + fib (numer-2);;

fib 42;;
fib 5;;
fib 6;;

(*zadanie 2b*)
let fibTail number = 
	let rec fibIter number =
		if(number<0) then raise (Failure "Niepoprawna wartosc!")
		else if(number = 0) then 0
		else if(number = 1) then 1
		else fibIter (number-1) + fibIter (number-2)
	in fibIter(number)
	;;
	
fibTail 42;;
fibTail 5;;
fibTail 6;;

(*zadanie 3*)
let root3 a =
	let rec rootIt b =
		if abs_float(b*.b*.b-.a) <= 1e-15 *. abs_float(a) then b
		else rootIt(b +. (a /. (b*.b) -. b) /.3.0)
	in rootIt(if a > 1.0 then a /. 3.0 else a);;

root3 (27.0);;
root3 (-27.0);;
root3 (8.0);;
root3 (7.0);;
root3 (1.0);;


(*zadanie 4a*)
let k1 = [-2;-1;0;1;2];;
let [_;_;x;_;_] = k1

(*zadanie 4b*)
let k2 = [(1,2);(0,1)];;
let [(_,_);(x,_)] = k2;;


(*zadanie 5*)
let rec initSegment(first,second) =
	match (first,second) with
	| ([],_) -> true
	| (_,[]) -> false
	| (_,_) -> 
		if List.hd first = List.hd second then initSegment(List.tl first, List.tl second)
		else false;;

initSegment([1;2;3],[1;2;3;4;5;6]);;
initSegment([3;2;1],[1;2;3;4;5;6]);;
initSegment([],[1;2;3]);;
initSegment([1;2],[]);;
initSegment(['a';'b'],['a';'b';'c']);;

(*zadanie 6a*)
let rec replaceNth(list,pos,elem) = 
	match(list,pos) with
	| ([],_) -> []
	| (head :: tail,0) -> elem::tail
	| (head :: tail,_) -> head::replaceNth(tail,pos-1,elem);;

replaceNth(['o';'l';'a'; 'm'; 'a'; 'k'; 'o'; 't'; 'a'], 1, 's');;
replaceNth([1;2;3],0,9);;
replaceNth([1;2;3],2,8);;
replaceNth(['a';'b';'c'],1,'d');;
		
		
		
		
		
		