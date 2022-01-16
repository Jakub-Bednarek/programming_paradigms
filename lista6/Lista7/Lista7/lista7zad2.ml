(* Jakub Bednarek *)

(* Zadanie 2 *)

let queue = ref (Queue_mut.empty(5));;

if Queue_mut.isEmpty(!queue) then print_string("Queue is empty\n") else print_string("Queue is not empty\n");;

Queue_mut.enqueue(4, !queue);;
Queue_mut.enqueue(7, !queue);;
Queue_mut.enqueue(9, !queue);;
Queue_mut.enqueue(11, !queue);;
Queue_mut.enqueue(15, !queue);;

if Queue_mut.isEmpty(!queue) then print_string("Queue is empty\n") else print_string("Queue is not empty\n");;

(* Queue_mut.enqueue(25, !queue);; *)

print_int(Queue_mut.first(!queue));;

Queue_mut.dequeue(!queue);;

print_string("\n");;
print_int(Queue_mut.first(!queue));;

Queue_mut.dequeue(!queue);;
Queue_mut.dequeue(!queue);;
Queue_mut.dequeue(!queue);;

print_string("\n");;
print_int(Queue_mut.first(!queue));;

Queue_mut.dequeue(!queue);;
(* Queue_mut.dequeue(!queue);; *)

if Queue_mut.isEmpty(!queue) then print_string("\nQueue is empty\n") else print_string("\nQueue is not empty\n");;