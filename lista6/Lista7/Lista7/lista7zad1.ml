(* Jakub Bednarek *)

(* Zadanie 1a *)

print_string ("Kolejka na jednej liście:\n");;

let q = ref (Queue_list.empty());;

if Queue_list.isEmpty(!q) then print_string("Queue is empty!\n") else print_string("Queue is not empty!\n");;

q := Queue_list.enqueue(15, !q);;
q := Queue_list.enqueue(23, !q);;
q := Queue_list.enqueue(58, !q);;
q := Queue_list.enqueue(124, !q);;

print_int (Queue_list.first(!q));;

if Queue_list.isEmpty(!q) then print_string("\nQueue is empty!\n") else print_string("\nQueue is not empty!\n");;

q := Queue_list.dequeue(!q);;

print_string ("\n");;
print_int (Queue_list.first(!q));;

q := Queue_list.dequeue(!q);
q := Queue_list.dequeue(!q);
q := Queue_list.dequeue(!q);

if Queue_list.isEmpty(!q) then print_string("\nQueue is empty!\n") else print_string("\nQueue is not empty!\n");;

(* q := Queue_list.dequeue(!q);; *)

(* Zadanie 2a *)

print_string ("Kolejka na dwóch listach: \n");;

let q_double = ref (Queue_double_list.empty());;

if Queue_double_list.isEmpty(!q_double) then print_string("Queue is empty!\n") else print_string("Queue is not empty!\n");;

q_double := Queue_double_list.enqueue(-5, !q_double);;
q_double := Queue_double_list.enqueue(-29, !q_double);;
q_double := Queue_double_list.enqueue(-155, !q_double);;

print_int (Queue_double_list.first(!q_double));;

if Queue_double_list.isEmpty(!q_double) then print_string("\nQueue is empty!\n") else print_string("\nQueue is not empty!\n");;

q_double := Queue_double_list.dequeue(!q_double);;

print_int (Queue_double_list.first(!q_double));;

q_double := Queue_double_list.dequeue(!q_double);

print_string ("\n");;
print_int (Queue_double_list.first(!q_double));;

q_double := Queue_double_list.dequeue(!q_double);

if Queue_double_list.isEmpty(!q_double) then print_string("\nQueue is empty!\n") else print_string("\nQueue is not empty!\n");;

(* q_double := Queue_double_list.dequeue(!q_double); *)