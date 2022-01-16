type ordering = LT | LE | GE | GT;;

module type ORDER =
sig
    type t
    val compare: t -> t -> ordering
end;;

module StringOrder: ORDER with type t = string =
struct
    type t = string
    let compare s1 s2 = if s1 < s2 then LT
                        else if s1 > s2 then GT
                        else if s1 < s2 || s1 = s2 then LE
                        else GE
end;;

module type QUEUE_FUNCTOR =
sig
    type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk
    val create: unit -> 'a tk
    val enqueue: 'a * 'a tk -> 'a tk
    val dequeue: 'a tk -> 'a tk
    val first: 'a tk -> 'a
    val empty: 'a tk -> bool
    val as_list: 'a tk -> 'a list
    val as_pairs: 'a tk -> (int * 'a) list
    exception Empty of string
end;;

module Queue (Ord: ORDER) : QUEUE_FUNCTOR =
struct
    type 'a tk = KolejkaPusta | Skladowa of 'a * 'a tk;;
    exception Empty of string;;

    let create() = KolejkaPusta;;

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

    let as_list queue =
        let rec as_list_rec(old_queue, vals) = 
            match old_queue with
            | Skladowa(v, tl) -> match Ord.compare "abc" v with
                                | case LT -> as_list_rec(tl, v :: vals)
                                | _ -> as_list_rec(tl, v)
            | KolejkaPusta -> vals
        in as_list_rec(queue, [])
    
    let as_pairs queue =
        let rec as_pairs_rec(index, old_queue, out_list) =
            match old_queue with
            | Skladowa(v, tl) -> as_pairs_rec(index + 1, tl, (index, v) :: out_list)
            | KolejkaPusta -> out_list
        in as_pairs_rec(0, queue, [])    
end;;

module StringQueue = Queue(StringOrder);;