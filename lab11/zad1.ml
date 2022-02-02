type ordering = LT | LE | GE | GT;;

module type ORDER =
sig
    type t
    val compare: t * t -> ordering
end;;

module StringOrder: ORDER with type t = string =
struct
    type t = string
    let compare (s1, s2) = if s1 < s2 then LT
                        else if s1 > s2 then GT
                        else if s1 < s2 || s1 = s2 then LE
                        else GE
end;;

module type QUEUE_FUNCTOR =
sig
    type s
    type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk
    val create: unit -> 'a tk
    val enqueue: 'a * 'a tk -> 'a tk
    val dequeue: 'a tk -> 'a tk
    val first: 'a tk -> 'a
    val empty: 'a tk -> bool
    val count_true: s * ordering * s tk -> int
    val as_list: 'a tk -> 'a list
    exception Empty of string
end;;

module Queue (Ord: ORDER) : QUEUE_FUNCTOR with type s = Ord.t =
struct
    type s = Ord.t
    type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk
    exception Empty of string

    let create() = KolejkaPusta

    let rec enqueue(value, queue) = 
        match queue with
        | KolejkaPusta -> Skladowa(value, KolejkaPusta)
        | Skladowa(v, tl) -> match tl with
                            | Skladowa(_) -> Skladowa(v, enqueue(value, tl))
                            | KolejkaPusta -> Skladowa(v, Skladowa(value, KolejkaPusta))

    let dequeue queue =
        match queue with
        | Skladowa(_, tl) -> tl
        | KolejkaPusta -> KolejkaPusta

    let first queue =
        match queue with
        | Skladowa(v, _) -> v
        | KolejkaPusta -> raise (Empty "pierwszy_element wywolano na pustej kolejce!")

    let empty queue =
        match queue with
        | KolejkaPusta -> true 
        | Skladowa(v, _) -> false

    let count_true (elem, comp, queue) =
        let rec as_list_rec(old_queue, sum) = 
            match old_queue with
            | KolejkaPusta -> sum
            | Skladowa(v, tl) -> if (Ord.compare(v, elem) = comp) then as_list_rec(tl, sum + 1) else as_list_rec(tl, sum)
        in as_list_rec(queue, 0)
    
    let as_list queue =
        let rec as_list_rec(old_queue, out_list) =
            match old_queue with
            | Skladowa(v, tl) -> as_list_rec(tl, v :: out_list)
            | KolejkaPusta -> List.rev out_list
        in as_list_rec(queue, [])    
end;;

module StringQueue = Queue(StringOrder);;

let q = ref (StringQueue.create());;

StringQueue.empty !q;;
StringQueue.first !q;;

q := StringQueue.enqueue("abcd", !q);;

StringQueue.first !q;;

q := StringQueue.enqueue("e", !q);;
q := StringQueue.enqueue("f", !q);;
q := StringQueue.enqueue("g", !q);;
q := StringQueue.enqueue("h", !q);;
q := StringQueue.enqueue("i", !q);;
q := StringQueue.enqueue("j", !q);;
q := StringQueue.enqueue("k", !q);;

StringQueue.empty !q;;

StringQueue.first !q;;

q := StringQueue.dequeue !q;;

StringQueue.empty !q;;

StringQueue.first !q;;

StringQueue.as_list !q;;

StringQueue.count_true("a", GT, !q);;
StringQueue.count_true("g", LT, !q);;